use crate::parse::{Command, CommandModule, HandlerBinding, HandlerType, Node, NodeRef, StrIdent};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{cell::RefCell, cmp::Ordering, collections::HashMap};
use syn::{spanned::Spanned, Ident, Lifetime, Type};

pub fn generate_module(input: CommandModule) -> TokenStream {
    // Convert the module name to PascalCase
    let raw_module_name = input.name.as_str();
    let module_struct_name = raw_module_name
        .split("_")
        .map(|element| {
            let mut capitalized = String::with_capacity(element.len());
            let ch = element.chars().next().unwrap();
            capitalized.extend(ch.to_uppercase());
            capitalized.push_str(&element[ch.len_utf8() ..]);
            capitalized
        })
        .fold(
            String::with_capacity(raw_module_name.len()),
            |mut ident, part| {
                ident.push_str(&part);
                ident
            },
        );
    let module_struct_name = Ident::new(&module_struct_name, input.name.span());

    // Check command identifier uniqueness
    let mut all_aliases: HashMap<&str, &StrIdent> = HashMap::new();
    for (alias_ident, alias) in input
        .definitions
        .iter()
        .map(|command| command.aliases.iter())
        .flatten()
        .map(|ident| (ident, ident.as_str()))
    {
        if all_aliases.contains_key(&alias) {
            let original_def = all_aliases.get(&alias).copied().unwrap();
            alias_ident
                .span()
                .unwrap()
                .error("Duplicate command alias.")
                .span_note(
                    original_def.span().unwrap(),
                    "Alias originally defined here",
                )
                .emit();
            return TokenStream::new();
        }

        all_aliases.insert(alias, alias_ident);
    }

    // Unpack the context type information
    let CommandModule {
        context_type,
        context_lifetime,
        ..
    } = &input;

    // Build the dispatch match arms
    let module_dispatch_arms = input.definitions.iter().map(|command| {
        let str_aliases = command
            .aliases
            .iter()
            .map(|alias| Literal::string(alias.as_str()));
        let dispatch_func = ident_dispatch_node(command.name());

        quote! {
            ::core::option::Option::Some(#( #str_aliases )|*) => Self::#dispatch_func(__args, __context, ::core::default::Default::default())
        }
    });
    let suggest_arms = input.definitions
        .iter()
        .map(|command| {
            let str_aliases = command.aliases
                .iter()
                .map(|alias| Literal::string(alias.as_str()));
            let get_suggestions_func = ident_get_suggestions_node(command.name());

            quote! {
                ::core::option::Option::Some(__arg @ #( #str_aliases )|*) => Self::#get_suggestions_func(__args, __context, __arg, ::core::default::Default::default())
            }
        });

    let command_list = input
        .definitions
        .iter()
        .map(|command| Literal::string(command.name().as_str()));

    let data_mod_name = format_ident!("__private_data_{}", input.name);

    let mut graphs = Vec::with_capacity(input.definitions.len());
    for command in &input.definitions {
        match NodeGraph::compile(command) {
            Some(graph) => graphs.push((command.name(), graph)),
            None => return quote! {},
        }
    }

    let data_structs = graphs
        .iter()
        .map(|(name, graph)| graph.gen_data_struct(name));
    let node_fns = graphs.iter().map(|(name, graph)| {
        graph.gen_node_fns(name, &data_mod_name, context_type, context_lifetime)
    });

    quote! {
        mod #data_mod_name {
            #( #data_structs )*
        }

        struct #module_struct_name;

        impl #module_struct_name {
            #( #node_fns )*
        }

        impl<#context_lifetime> ::quartz_commands::CommandModule<#context_type> for #module_struct_name {
            fn dispatch(&self, __command: &str, __context: #context_type) -> ::core::result::Result<(), ::quartz_commands::Error> {
                let mut __args = ::quartz_commands::ArgumentTraverser::new(__command);
                match __args.next() {
                    #( #module_dispatch_arms, )*
                    ::core::option::Option::Some(__cmd @ _) => ::core::result::Result::Err(::std::format!("Invalid command \"{}\"", __cmd)),
                    ::core::option::Option::None => ::core::result::Result::Ok(())
                }
            }

            fn get_suggestions(&self, __command: &str, __context: &#context_type) -> ::std::vec::Vec<::std::string::String> {
                let mut __args = ::quartz_commands::ArgumentTraverser::new(__command);
                match __args.next() {
                    #( #suggest_arms, )*
                    _ => ::std::vec![#( #command_list ),*]
                        .into_iter()
                        .filter(|&__cmd| __cmd.starts_with(__command))
                        .map(|__cmd| __cmd.to_owned())
                        .collect(),
                }
            }
        }
    }
}

struct NodeGraph<'a> {
    flat_graph: HashMap<&'a str, NodeData<'a>>,
    root_executes: Option<&'a HandlerBinding>,
    root_suggests: Option<&'a HandlerBinding>,
    root_successors: Vec<&'a Node>,
}

impl<'a> NodeGraph<'a> {
    fn compile(command_definition: &'a Command) -> Option<Self> {
        let mut graph = NodeGraph {
            flat_graph: HashMap::new(),
            root_executes: None,
            root_suggests: None,
            root_successors: Vec::new(),
        };

        let mut failed = false;
        for branch in command_definition.branches.iter() {
            for candidate_root_successor in branch.nodes[0].flatten() {
                match candidate_root_successor {
                    Node::Argument { .. } | Node::Literal(_) => {
                        if !graph
                            .flat_graph
                            .contains_key(candidate_root_successor.name())
                            && !graph.root_successors.iter().any(|&root_successor| {
                                Self::node_name_eq(candidate_root_successor, root_successor)
                            })
                        {
                            graph.root_successors.push(candidate_root_successor);
                        }
                    }
                    Node::CommandRoot(_) => {}
                    // `flatten` ensures this
                    Node::Any { .. } => unreachable!(),
                }
            }

            for i in 0 .. branch.nodes.len() {
                let node = &branch.nodes[i];
                let successor = branch.nodes.get(i + 1);

                if !graph.resolve_successor(node, successor) {
                    failed = true;
                }
            }
        }

        if failed {
            return None;
        }

        for handler_binding in &command_definition.handler_bindings {
            if let NodeRef::Root(_) = &handler_binding.node_ref {
                match &handler_binding.handler_type {
                    HandlerType::Executor(_) => match graph.root_executes {
                        Some(root_handler_binding) => {
                            root_handler_binding
                                .span()
                                .unwrap()
                                .error("Multiple executors cannot be bound to a node")
                                .span_note(
                                    graph.root_executes.unwrap().span().unwrap(),
                                    "Executor first bound here",
                                )
                                .emit();
                            failed = true;
                        }
                        None => graph.root_executes = Some(handler_binding),
                    },
                    HandlerType::Suggester(_) => match graph.root_suggests {
                        Some(root_handler_binding) => {
                            root_handler_binding
                                .span()
                                .unwrap()
                                .error("Multiple suggestion generators cannot be bound to a node")
                                .span_note(
                                    graph.root_executes.unwrap().span().unwrap(),
                                    "Suggestion generators first bound here",
                                )
                                .emit();
                            failed = true;
                        }
                        None => graph.root_suggests = Some(handler_binding),
                    },
                }

                continue;
            }

            let node_data_for_binding =
                match graph.flat_graph.get_mut(handler_binding.node_ref.name()) {
                    Some(data) => data,
                    None => {
                        handler_binding
                            .node_ref
                            .span()
                            .unwrap()
                            .error("Undefined node reference")
                            .emit();
                        failed = true;
                        continue;
                    }
                };

            match &handler_binding.handler_type {
                HandlerType::Executor(_) => match node_data_for_binding.executes {
                    Some(handler) => {
                        handler
                            .span()
                            .unwrap()
                            .error("Multiple executors cannot be bound to a node")
                            .span_note(handler.span().unwrap(), "Executor first bound here")
                            .emit();
                        failed = true;
                    }
                    None => node_data_for_binding.executes = Some(handler_binding),
                },
                HandlerType::Suggester(_) => match node_data_for_binding.suggests {
                    Some(handler) => {
                        handler
                            .span()
                            .unwrap()
                            .error("Multiple suggestion generators cannot be bound to a node")
                            .span_note(
                                handler.span().unwrap(),
                                "Suggestion generators first bound here",
                            )
                            .emit();
                        failed = true;
                    }
                    None => node_data_for_binding.suggests = Some(handler_binding),
                },
            }
        }

        if failed || graph.has_successor_duplicates() {
            None
        } else {
            graph.track_defined_args();
            Some(graph)
        }
    }

    fn resolve_successor(&mut self, node: &'a Node, successor: Option<&'a Node>) -> bool {
        let successors = successor.into_iter().flat_map(Node::flatten);

        match node {
            Node::Argument {
                name: node_name,
                ty: node_type,
                default,
                ..
            } => {
                if default.is_some()
                    && successor.is_some()
                    && !successor
                        .clone()
                        .into_iter()
                        .flat_map(Node::flatten)
                        .all(|node| {
                            matches!(node, Node::Argument {
                                default: Some(_),
                                ..
                            })
                        })
                {
                    successor
                        .span()
                        .unwrap()
                        .error(
                            "Cannot have non-default argument successors after a default argument \
                             node",
                        )
                        .emit();
                    return false;
                }

                let name_str = node_name.as_str();
                match self.flat_graph.get_mut(name_str) {
                    Some(node_data) => {
                        if node_type.is_some() {
                            node_name
                                .span()
                                .unwrap()
                                .error("Argument already defined")
                                .span_note(
                                    node_data.definition.span().unwrap(),
                                    "Argument first defined here",
                                )
                                .emit();
                            return false;
                        }

                        node_data.successors.extend(successors);
                    }
                    None => {
                        if node_type.is_none() {
                            node_name
                                .span()
                                .unwrap()
                                .error("Argument not defined")
                                .help("Consider adding a type specifier")
                                .emit();
                            return false;
                        }

                        self.flat_graph.insert(
                            name_str,
                            NodeData::with_successors(node, successors.collect()),
                        );
                    }
                }
            }
            Node::Literal(node_literal) => {
                let name_str = node_literal.as_str();
                match self.flat_graph.get_mut(name_str) {
                    Some(node_data) => {
                        node_data.successors.extend(successors);
                    }
                    None => {
                        self.flat_graph.insert(
                            name_str,
                            NodeData::with_successors(node, successors.collect()),
                        );
                    }
                }
            }
            Node::CommandRoot(_) => {
                if successor.is_none() {
                    return true;
                }

                node.span()
                    .unwrap()
                    .error("Root nodes cannot have explicitly defined successors")
                    .span_help(
                        successor.span().unwrap(),
                        "Consider removing this successor",
                    )
                    .emit();
                return false;
            }
            Node::Any { nodes, .. } => {
                return nodes
                    .iter()
                    .all(|node| self.resolve_successor(node, successor));
            }
        }

        true
    }

    fn has_successor_duplicates(&self) -> bool {
        for (_, node_data) in &self.flat_graph {
            for i in 0 .. node_data.successors.len() {
                for j in i + 1 .. node_data.successors.len() {
                    let successor0 = node_data.successors[i];
                    let successor1 = node_data.successors[j];

                    if Self::node_name_eq(successor0, successor1) {
                        successor1
                            .span()
                            .unwrap()
                            .error("Duplicate successor definition")
                            .span_note(
                                successor0.span().unwrap(),
                                "Successor originally defined here",
                            )
                            .emit();
                        return true;
                    }
                }
            }
        }

        false
    }

    fn track_defined_args(&mut self) {
        for (_, node_data) in self.flat_graph.iter() {
            self.add_arg_to_successors(node_data.definition, &node_data.successors);
        }
    }

    fn add_arg_to_successors(&self, arg_node: &'a Node, successors: &[&'a Node]) {
        for &successor in successors
            .into_iter()
            .filter(|&&successor| matches!(successor, Node::Argument { .. } | Node::Literal(_)))
        {
            if matches!(arg_node, Node::Argument { default: None, .. }) {
                let mut defined_args = self
                    .flat_graph
                    .get(successor.name())
                    .unwrap()
                    .defined_arguments
                    .borrow_mut();
                if defined_args
                    .iter()
                    .any(|&node| Self::node_name_eq(node, arg_node))
                {
                    return;
                }
                defined_args.push(arg_node);
            }

            self.add_arg_to_successors(
                successor,
                &self.flat_graph.get(successor.name()).unwrap().successors,
            );
        }
    }

    fn gen_data_struct(&self, name: &impl quote::IdentFragment) -> TokenStream {
        let name = ident_data_struct(name);
        let iter = self
            .flat_graph
            .iter()
            .filter(|(_, data)| matches!(data.definition, Node::Argument { .. }));
        let names = iter
            .clone()
            .map(|(&name, _)| format_ident!("{}", name))
            .collect::<Vec<_>>();
        let types = iter.clone().map(|(_, data)| match data.definition {
            Node::Argument {
                ty: Some(ty),
                default,
                ..
            } => match default {
                Some(_) => quote! { #ty },
                None => quote! { ::core::option::Option<#ty> },
            },
            _ => unreachable!(),
        });
        let defaults = iter.clone().map(|(_, data)| match data.definition {
            Node::Argument { default, .. } => match default {
                Some(lit) => quote! { #lit },
                None => quote! { ::core::option::Option::None },
            },
            _ => unreachable!(),
        });

        quote! {
            pub struct #name<'cmd> {
                #( pub #names : #types, )*
                _phantom: ::std::marker::PhantomData<&'cmd ()>
            }

            impl<'cmd> ::core::default::Default for #name<'cmd> {
                fn default() -> Self {
                    Self {
                        #( #names : #defaults, )*
                        _phantom: ::std::marker::PhantomData
                    }
                }
            }
        }
    }

    fn gen_unpack_data_struct(
        &self,
        name: &impl quote::IdentFragment,
        data_mod_name: &impl ToTokens,
        defined_args: &[&Node],
    ) -> TokenStream {
        let name = ident_data_struct(name);
        let str_names = self
            .flat_graph
            .iter()
            .filter(|(_, data)| matches!(data.definition, Node::Argument { .. }))
            .map(|(&name, _)| name);
        let names = str_names
            .clone()
            .map(|str_name| format_ident!("{}", str_name));
        let unwraps = str_names
            .clone()
            .filter(|&str_name| defined_args.into_iter().any(|node| node.name() == str_name))
            .map(|str_name| format_ident!("{}", str_name))
            .collect::<Vec<_>>();
        quote! {
            let #data_mod_name::#name { #( #names, )* .. } = __data;
            #( let #unwraps = #unwraps.unwrap(); )*
        }
    }

    fn gen_node_fns<T>(
        &self,
        name: &T,
        data_mod_name: &impl ToTokens,
        context_type: &Box<Type>,
        context_lifetime: &Option<Lifetime>,
    ) -> TokenStream
    where
        T: quote::IdentFragment + Spanned,
    {
        let root_dispatch_fn = ident_dispatch_node(name);
        let root_suggestions_fn = ident_get_suggestions_node(name);

        let root_dispatch_context_name = match self.root_executes {
            Some(HandlerBinding { context, .. }) => context.inner().clone(),
            None => format_ident!("__context"),
        };
        let root_suggestions_context_name = match self.root_suggests {
            Some(HandlerBinding { context, .. }) => context.inner().clone(),
            None => format_ident!("__context"),
        };
        let root_suggestions_arg_name = match self.root_suggests {
            Some(HandlerBinding { arg: Some(arg), .. }) => arg.inner().clone(),
            _ => format_ident!("__arg"),
        };

        let data_struct_name = ident_data_struct(name);
        let root_dispatch_match = self.gen_dispatch_match(
            name,
            data_mod_name,
            context_type,
            name,
            self.root_executes.clone(),
            &self.root_successors,
            &[],
        );
        let root_suggestions_match = self.gen_suggestions_match(
            name,
            data_mod_name,
            context_type,
            None,
            self.root_suggests.clone(),
            &self.root_successors,
        );

        let iter = self.flat_graph.iter();

        let node_dispatch_fn_matches = iter.clone().map(
            |(
                _,
                NodeData {
                    definition,
                    executes,
                    successors,
                    defined_arguments,
                    ..
                },
            )| {
                self.gen_dispatch_match(
                    name,
                    data_mod_name,
                    context_type,
                    definition,
                    *executes,
                    successors,
                    &*defined_arguments.borrow(),
                )
            },
        );
        let node_suggestions_fn_matches = iter.clone().map(
            |(
                _,
                NodeData {
                    definition,
                    suggests,
                    successors,
                    ..
                },
            )| {
                self.gen_suggestions_match(
                    name,
                    data_mod_name,
                    context_type,
                    Some(definition),
                    suggests.clone(),
                    successors,
                )
            },
        );

        let node_dispatch_fn_names =
            iter.clone()
                .map(|(_, NodeData { definition, .. })| match definition {
                    Node::Argument {
                        name: node_name, ..
                    } => format_ident!("dispatch_{}_{}", name, node_name),
                    Node::Literal(lit) => format_ident!("dispatch_{}_lit_{}", name, lit),
                    _ => unreachable!(),
                });
        let node_suggestions_fn_names =
            iter.clone()
                .map(|(_, NodeData { definition, .. })| match definition {
                    Node::Argument {
                        name: node_name, ..
                    } => format_ident!("get_suggestions_{}_{}", name, node_name),
                    Node::Literal(lit) => format_ident!("get_suggestions_{}_lit_{}", name, lit),
                    _ => unreachable!(),
                });

        let node_dispatch_fn_context_names =
            iter.clone()
                .map(|(_, NodeData { executes, .. })| match executes {
                    &Some(handler_binding) => handler_binding.context.inner().clone(),
                    None => format_ident!("__context"),
                });
        let node_suggestions_fn_context_names =
            iter.clone()
                .map(|(_, NodeData { suggests, .. })| match suggests {
                    &Some(handler_binding) => handler_binding.context.inner().clone(),
                    None => format_ident!("__context"),
                });
        let node_fn_arg_names = iter
            .clone()
            .map(|(_, NodeData { suggests, .. })| match suggests {
                &Some(HandlerBinding { arg: Some(arg), .. }) => arg.inner().clone(),
                _ => format_ident!("__arg"),
            });

        quote! {
            #(
                fn #node_dispatch_fn_names<'cmd, #context_lifetime>(
                    mut __args: ::quartz_commands::ArgumentTraverser<'cmd>,
                    mut #node_dispatch_fn_context_names: #context_type,
                    mut __data: #data_mod_name::#data_struct_name<'cmd>
                ) -> ::core::result::Result<(), ::quartz_commands::Error>
                {
                    #node_dispatch_fn_matches
                }
            )*

            fn #root_dispatch_fn<'cmd, #context_lifetime>(
                mut __args: ::quartz_commands::ArgumentTraverser<'cmd>,
                mut #root_dispatch_context_name: #context_type,
                mut __data: #data_mod_name::#data_struct_name<'cmd>
            ) -> ::core::result::Result<(), ::quartz_commands::Error>
            {
                #root_dispatch_match
            }

            #(
                fn #node_suggestions_fn_names<'cmd, #context_lifetime>(
                    mut __args: ::quartz_commands::ArgumentTraverser<'cmd>,
                    mut #node_suggestions_fn_context_names: &#context_type,
                    #node_fn_arg_names: &'cmd str,
                    mut __data: #data_mod_name::#data_struct_name<'cmd>
                ) -> ::std::vec::Vec<::std::string::String>
                {
                    let mut __suggestions = ::std::vec::Vec::new();
                    #node_suggestions_fn_matches
                    __suggestions
                }
            )*

            fn #root_suggestions_fn<'cmd, #context_lifetime>(
                mut __args: ::quartz_commands::ArgumentTraverser<'cmd>,
                mut #root_suggestions_context_name: &#context_type,
                #root_suggestions_arg_name: &'cmd str,
                mut __data: #data_mod_name::#data_struct_name<'cmd>
            ) -> ::std::vec::Vec<::std::string::String>
            {
                let mut __suggestions = ::std::vec::Vec::new();
                #root_suggestions_match
                __suggestions
            }
        }
    }

    fn gen_dispatch_match(
        &self,
        root_name: &impl quote::IdentFragment,
        data_mod_name: &impl ToTokens,
        context_type: &Box<Type>,
        definition: &impl Spanned,
        executes: Option<&HandlerBinding>,
        successors: &[&Node],
        defined_args: &[&Node],
    ) -> Option<TokenStream> {
        let context_ident = executes
            .map(|binding| binding.context.inner().clone())
            .unwrap_or(format_ident!("__context"));

        let none_arm = match executes {
            Some(HandlerBinding { handler, .. }) => {
                let unpack = self.gen_unpack_data_struct(root_name, data_mod_name, defined_args);
                quote! {
                    {
                        #unpack
                        #handler
                    }
                }
            }
            None => match successors.len() {
                0 => {
                    definition
                        .span()
                        .unwrap()
                        .error("A node at the end of a branch must have an executor.")
                        .emit();
                    return None;
                }
                1 => match &successors[0] {
                    Node::Argument { name, .. } => {
                        let literal = Literal::string(&format!(
                            "Expected value for argument \"{}\"",
                            name.as_str()
                        ));
                        quote! { ::core::result::Result::Err(#literal.to_owned()) }
                    }
                    Node::Literal(lit) => {
                        let literal =
                            Literal::string(&format!("Expected literal \"{}\"", lit.as_str()));
                        quote! { ::core::result::Result::Err(#literal.to_owned()) }
                    }
                    _ =>
                        quote! { ::core::result::Result::Err("Expected additional arguments".to_owned()) },
                },
                _ => {
                    let successor_list = successors
                        .iter()
                        .map(|&node| match node {
                            Node::Argument { name, .. } => Some(name.to_string()),
                            Node::Literal(lit) => Some(format!("\"{}\"", lit.as_str())),
                            _ => None,
                        })
                        .flatten()
                        .collect::<Vec<_>>()
                        .join(", ");
                    let literal = Literal::string(&format!("Expected one of {}", successor_list));
                    quote! { ::core::result::Result::Err(#literal.to_owned()) }
                }
            },
        };

        let default_some_arm = if successors
            .iter()
            .any(|&node| matches!(node, Node::CommandRoot(_)))
        {
            let root_fn = ident_dispatch_node(root_name);
            quote! { Self::#root_fn(__args, #context_ident, __data) }
        } else {
            quote! { ::core::result::Result::Err(::std::format!("Unexpected argument \"{}\"", __arg)) }
        };

        let mut case_nodes = successors
            .iter()
            .copied()
            .filter(|&node| matches!(node, Node::Argument { .. } | Node::Literal(_)))
            .collect::<Vec<_>>();
        case_nodes.sort_by(|&a, &b| match (a, b) {
            (Node::Argument { .. }, Node::Literal(_)) => Ordering::Less,
            (Node::Literal(_), Node::Argument { .. }) => Ordering::Greater,
            _ => Ordering::Equal,
        });
        let branches = case_nodes.into_iter()
            .flat_map(|node| {
                match node {
                    Node::Argument { name: arg_name, ty: Some(ty), default, .. } => {
                        let dispatch_fn = format_ident!("dispatch_{}_{}", root_name, arg_name);
                        let var_wrapper = if default.is_some() {
                            None
                        } else {
                            Some(quote! { ::core::option::Option::Some })
                        };
                        Some(quote! {
                            ::core::option::Option::Some(__arg) if <#ty as ::quartz_commands::FromArgument<'cmd, #context_type>>::matches(__arg) => {
                                __data.#arg_name = #var_wrapper (<#ty as ::quartz_commands::FromArgument<'cmd, _>>::from_arg(__arg, &#context_ident)?);
                                Self::#dispatch_fn(__args, #context_ident, __data)
                            }
                        })
                    },
                    Node::Literal(lit) => {
                        let dispatch_fn = format_ident!("dispatch_{}_lit_{}", root_name, lit);
                        Some(quote! {
                            ::core::option::Option::Some(#lit) => Self::#dispatch_fn(__args, #context_ident, __data)
                        })
                    },
                    _ => None
                }
            });

        Some(quote! {
            match __args.next() {
                #( #branches, )*
                ::core::option::Option::Some(__arg @ _) => #default_some_arm,
                ::core::option::Option::None => #none_arm
            }
        })
    }

    fn gen_suggestions_match(
        &self,
        root_name: &impl quote::IdentFragment,
        data_mod_name: &impl ToTokens,
        context_type: &Box<Type>,
        definition: Option<&Node>,
        suggests: Option<&HandlerBinding>,
        successors: &[&Node],
    ) -> Option<TokenStream> {
        let context_ident = suggests
            .map(|binding| binding.context.inner().clone())
            .unwrap_or(format_ident!("__context"));

        let default_arm = match suggests {
            Some(HandlerBinding { handler, .. }) => {
                let unpack = self.gen_unpack_data_struct(root_name, data_mod_name, &[]);
                Some(quote! {
                    {
                        #unpack
                        let __helper = move || { #handler };
                        __suggestions.extend(__helper());
                    }
                })
            }
            None => match definition {
                Some(Node::Literal(lit)) => Some(quote! { __suggestions.push(#lit.to_owned()) }),
                _ => None,
            },
        };

        let default_some_arm = if successors
            .iter()
            .any(|&node| matches!(node, Node::CommandRoot(_)))
        {
            let root_fn = ident_get_suggestions_node(root_name);
            quote! { __suggestions.extend(Self::#root_fn(__args, #context_ident, __data)); }
        } else {
            let literals = successors.into_iter().flat_map(|&node| match node {
                Node::Literal(literal) => Some(literal.inner().clone()),
                _ => None,
            });

            quote! {
                __suggestions.extend(
                    [#( #literals ),*]
                        .iter()
                        .filter(|lit: &&&'static str| lit.starts_with(__arg))
                        .map(|&lit| lit.to_owned())
                );
            }
        };

        let branches = successors
            .iter()
            .copied()
            .filter(|&node| matches!(node, Node::Argument { .. }))
            .flat_map(|node| {
                match node {
                    Node::Argument { name: arg_name, ty: Some(ty), default, .. } => {
                        let get_suggestions_fn = format_ident!("get_suggestions_{}_{}", root_name, arg_name);
                        let var_wrapper = if default.is_some() {
                            None
                        } else {
                            Some(quote! { ::core::option::Option::Some })
                        };
                        Some(quote! {
                            if <#ty as ::quartz_commands::FromArgument<'cmd, #context_type>>::partial_matches(__arg) {
                                if let ::core::result::Result::Ok(value) = <#ty as ::quartz_commands::FromArgument<'cmd, _>>::from_arg(__arg, &#context_ident) {
                                    __data.#arg_name = #var_wrapper (value);
                                }
                                __suggestions.extend(Self::#get_suggestions_fn(__args, #context_ident, __arg, __data));
                            }
                        })
                    },
                    _ => None
                }
            });

        Some(quote! {
            if let ::core::option::Option::Some(__arg) = __args.next() {
                #( #branches )*
                #default_some_arm
            } else {
                #default_arm
            }
        })
    }

    fn node_name_eq(a: &Node, b: &Node) -> bool {
        match &(a, b) {
            (Node::Argument { name: name0, .. }, Node::Argument { name: name1, .. }) =>
                name0 == name1,
            (Node::Literal(lit0), Node::Literal(lit1)) => lit0 == lit1,
            (Node::CommandRoot(_), Node::CommandRoot(_)) => true,
            _ => false,
        }
    }
}

struct NodeData<'a> {
    definition: &'a Node,
    executes: Option<&'a HandlerBinding>,
    suggests: Option<&'a HandlerBinding>,
    successors: Vec<&'a Node>,
    defined_arguments: RefCell<Vec<&'a Node>>,
}

impl<'a> NodeData<'a> {
    pub fn with_successors(definition: &'a Node, successors: Vec<&'a Node>) -> Self {
        let defined_args = if matches!(definition, Node::Argument { default: None, .. }) {
            vec![definition]
        } else {
            Vec::new()
        };

        NodeData {
            definition,
            executes: None,
            suggests: None,
            successors,
            defined_arguments: RefCell::new(defined_args),
        }
    }
}

fn ident_dispatch_node<T: quote::IdentFragment>(name: &T) -> Ident {
    format_ident!("dispatch_{}", name)
}

fn ident_get_suggestions_node<T: quote::IdentFragment>(name: &T) -> Ident {
    format_ident!("get_suggestions_{}", name)
}

fn ident_data_struct<T: quote::IdentFragment>(name: &T) -> Ident {
    format_ident!("CommandData{}", name)
}
