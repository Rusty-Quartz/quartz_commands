use crate::parse::{Command, CommandModule, HandlerBinding, HandlerType, Node, NodeRef, StrIdent};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{cell::{Cell, RefCell}, cmp::Ordering, collections::HashMap};
use syn::{spanned::Spanned, Ident, Lifetime, Type};

// Generates a struct and module of data structs which implement the supplied command
// format, assuming it is valid.
pub fn generate_module(input: CommandModule) -> Option<TokenStream> {
    // // Extract the visibility to apply to the generated struct
    // let module_vis = &input.vis;

    // // Convert the module name to PascalCase
    // let raw_module_name = input.name.as_str();
    // let module_struct_name = raw_module_name
    //     .split("_")
    //     .map(|element| {
    //         let mut capitalized = String::with_capacity(element.len());
    //         let ch = element.chars().next().unwrap();
    //         capitalized.extend(ch.to_uppercase());
    //         capitalized.push_str(&element[ch.len_utf8() ..]);
    //         capitalized
    //     })
    //     .fold(
    //         String::with_capacity(raw_module_name.len()),
    //         |mut ident, part| {
    //             ident.push_str(&part);
    //             ident
    //         },
    //     );
    // let module_struct_name = Ident::new(&module_struct_name, input.name.span());

    // // Check command identifier uniqueness
    // let mut all_aliases: HashMap<&str, &StrIdent> = HashMap::new();
    // let mut failed = false;
    // for (alias_ident, alias) in input
    //     .definitions
    //     .iter()
    //     .map(|command| command.aliases.iter())
    //     .flatten()
    //     .map(|ident| (ident, ident.as_str()))
    // {
    //     // An alias was already defined
    //     if all_aliases.contains_key(&alias) {
    //         let original_def = all_aliases.get(&alias).copied().unwrap();
    //         alias_ident
    //             .span()
    //             .unwrap()
    //             .error("Duplicate command alias.")
    //             .span_note(
    //                 original_def.span().unwrap(),
    //                 "Alias originally defined here",
    //             )
    //             .emit();
    //         failed = true;
    //         continue;
    //     }

    //     all_aliases.insert(alias, alias_ident);
    // }

    // // We failed the alias uniqueness check
    // if failed {
    //     return None;
    // }

    // // Unpack the context type information
    // let CommandModule {
    //     context_type,
    //     context_lifetime,
    //     ..
    // } = &input;

    // // Build the module's dispatch arms. These will map commands and their aliases to their
    // // associated dispatch function.
    // let module_dispatch_arms = input.definitions.iter().map(|command| {
    //     // Convert the aliases to string literals
    //     let str_aliases = command
    //         .aliases
    //         .iter()
    //         .map(|alias| Literal::string(alias.as_str()));
    //     // Get the name of the dispatcher function
    //     let dispatch_func = ident_dispatch_node(command.name());

    //     quote! {
    //         ::core::option::Option::Some(#( #str_aliases )|*) =>
    //             Self::#dispatch_func(&mut __args, __context, ::core::default::Default::default())
    //     }
    // });

    // // Build the module's suggestion generator arms. These will map commands and their aliases
    // // to their associated suggestion generation functions.
    // let module_suggest_arms = input.definitions
    //     .iter()
    //     .map(|command| {
    //         // Convert the aliases to string literals
    //         let str_aliases = command
    //             .aliases
    //             .iter()
    //             .map(|alias| Literal::string(alias.as_str()));
    //         // Get the name of the suggester function
    //         let get_suggestions_func = ident_get_suggestions_node(command.name());

    //         quote! {
    //             ::core::option::Option::Some(__arg @ (#( #str_aliases )|*)) =>
    //                 Self::#get_suggestions_func(
    //                     &mut __args,
    //                     __context,
    //                     __arg,
    //                     &mut ::core::default::Default::default()
    //                 )
    //         }
    //     });

    // // Get a list of commands as string literals for the default suggestions
    // let command_list = input
    //     .definitions
    //     .iter()
    //     .map(|command| Literal::string(command.name().as_str()));

    // // The name of the rust module containing the command data structures
    // let data_mod_name = format_ident!("__private_data_{}", input.name);

    // Compile the commands and their node branches into a set of graphs. If a command compiles,
    // then we can be sure that it is semantically correct and has a valid graph.
    // let mut graphs = Vec::with_capacity(input.definitions.len());
    for command in &input.definitions {
        // match NodeGraph::compile(command) {
        //     Some(graph) => graphs.push((command.name(), graph)),
        //     None => failed = true,
        // }
        let _ = NodeGraph::compile(command);
    }

    None

    // if failed {
    //     return None;
    // }

    // // Generate the data structs for each command
    // let data_structs = graphs
    //     .iter()
    //     .map(|(name, graph)| graph.gen_data_struct(name));

    // // Generate all the methods for dispatching and generating suggestions
    // let mut node_fns = Vec::with_capacity(graphs.len());
    // for (name, graph) in graphs.iter() {
    //     node_fns.push(graph.gen_node_fns(name, &data_mod_name, context_type, context_lifetime)?);
    // }

    // Some(quote! {
    //     mod #data_mod_name {
    //         use super::*;
    //         #( #data_structs )*
    //     }

    //     #module_vis struct #module_struct_name;

    //     impl #module_struct_name {
    //         #( #node_fns )*
    //     }

    //     impl<#context_lifetime> ::quartz_commands::CommandModule<#context_type> for #module_struct_name {
    //         fn dispatch(&self, __command: &str, __context: #context_type) -> ::core::result::Result<(), ::quartz_commands::Error> {
    //             let mut __args = ::quartz_commands::ArgumentTraverser::new(__command);
    //             match __args.next() {
    //                 #( #module_dispatch_arms, )*
    //                 ::core::option::Option::Some(__cmd @ _) => ::core::result::Result::Err(::std::format!("Invalid command \"{}\"", __cmd)),
    //                 ::core::option::Option::None => ::core::result::Result::Ok(())
    //             }
    //         }

    //         fn get_suggestions(&self, __command: &str, __context: &#context_type) -> ::std::vec::Vec<::std::string::String> {
    //             let mut __args = ::quartz_commands::ArgumentTraverser::new(__command);
    //             match __args.next() {
    //                 #( #module_suggest_arms, )*
    //                 _ => ::std::vec![#( #command_list ),*]
    //                     .into_iter()
    //                     .filter(|&__cmd: &&'static str| __cmd.starts_with(__command))
    //                     .map(|__cmd| __cmd.to_owned())
    //                     .collect(),
    //             }
    //         }
    //     }
    // })
}

// A node graph for a single command. This structure is the result of compiling a set of branches
// into a flattened graph of nodes paired with their successors.
struct NodeGraph<'a> {
    flat_graph: HashMap<String, NodeData<'a>>,
    named_any_nodes: HashMap<String, &'a Node>,
    root_executes: Option<&'a HandlerBinding>,
    root_suggests: Option<&'a HandlerBinding>,
    root_successors: Vec<&'a Node>,
}

impl<'a> NodeGraph<'a> {
    // Compile a command into a graph, returning `None` if the given input does not map to a valid
    // graph.
    fn compile(command_definition: &'a Command) -> Option<Self> {
        // Initialize the graph
        let mut graph = NodeGraph {
            flat_graph: HashMap::new(),
            named_any_nodes: HashMap::new(),
            root_executes: None,
            root_suggests: None,
            root_successors: Vec::new(),
        };

        // let mut failed = false;

        // Iterate over all the branches and flatten them into pairs of nodes and successors to
        // those nodes
        for branch in command_definition.branches.iter() {
            // Identify the successors to the root of the command
            for candidate_root_successor in branch.nodes[0].flatten() {
                match candidate_root_successor {
                    Node::Argument { .. } | Node::Literal { .. } => {
                        // // If the flattened graph already contains the node, then the user is
                        // // extending another branch. Duplicates are checked during successor
                        // // resolution.
                        // let unique_name = candidate_root_successor.unique_name();
                        // if graph.flat_graph.contains_key(&unique_name) ||
                        //     graph.named_any_nodes.contains_key(&unique_name)
                        // {
                        //     continue;
                        // }

                        // // If this node is already registered as a root successor, then just ignore
                        // // it, duplicates are handled later.
                        // if graph.root_successors.iter().any(|&root_successor| {
                        //     node_name_eq(candidate_root_successor, root_successor)
                        // }) {
                        //     continue;
                        // }

                        // Add the node as a root successor. Successor resolution ensures that
                        // all argument nodes are well-defined with a type.
                        graph.root_successors.push(candidate_root_successor);
                    }
                    // // The command root cannot have explicitly defined successors, and this error
                    // // will be caught by successor resolution
                    // // FIXME: deny the redundant branch where the user only specifies the root node
                    // // with no predecessors or successors.
                    // Node::CommandRoot(_) => {}
                    // // `flatten` ensures this
                    // Node::Any { .. } => unreachable!(),
                    _ => {}
                }
            }

            // Resolve node successors. This process is fairly complex, but ensures that all
            // predecessor nodes are well-defined and unique, and also upholds invariants
            // regarding argument nodes.
            for i in 0 .. branch.nodes.len() {
                let node = &branch.nodes[i];

                if let Node::Any { name: Some(name), .. } = node {
                    graph.named_any_nodes.insert(name.to_string(), node);
                }

                // let node = graph.node_definition(node);
                let successor = branch.nodes.get(i + 1)
                    // FIXME: this map causes a segfault when running `cargo expand --example simple`
                    .map(|node| graph.node_definition(node));

                graph.resolve_successor(node, successor);
                // if !graph.resolve_successor(node, successor) {
                //     failed = true;
                // }
            }
        }

        // if failed {
        //     return None;
        // }

        // // Apply the handler bindings
        // for handler_binding in &command_definition.handler_bindings {
        //     // We handle the root separately. Essentially, if a handler binding of a certain type
        //     // was not already bound to the root, then bind it, else error.
        //     if let NodeRef::Root(_) = &handler_binding.node_ref {
        //         match &handler_binding.handler_type {
        //             HandlerType::Executor(_) => match graph.root_executes {
        //                 Some(root_handler_binding) => {
        //                     root_handler_binding
        //                         .span()
        //                         .unwrap()
        //                         .error("Multiple executors cannot be bound to a node")
        //                         .span_note(
        //                             graph.root_executes.unwrap().span().unwrap(),
        //                             "Executor first bound here",
        //                         )
        //                         .emit();
        //                     failed = true;
        //                 }
        //                 None => graph.root_executes = Some(handler_binding),
        //             },

        //             HandlerType::Suggester(_) => match graph.root_suggests {
        //                 Some(root_handler_binding) => {
        //                     root_handler_binding
        //                         .span()
        //                         .unwrap()
        //                         .error("Multiple suggestion generators cannot be bound to a node")
        //                         .span_note(
        //                             graph.root_executes.unwrap().span().unwrap(),
        //                             "Suggestion generator first bound here",
        //                         )
        //                         .emit();
        //                     failed = true;
        //                 }
        //                 None => graph.root_suggests = Some(handler_binding),
        //             },
        //         }

        //         // Skip to the next binding
        //         continue;
        //     }

        //     // Grab the node data that we're binding to based off the node ref's unique name
        //     let node_data_for_binding =
        //         match graph.flat_graph.get_mut(&handler_binding.node_ref.unique_name()) {
        //             Some(data) => data,

        //             // Node data not found
        //             None => {
        //                 handler_binding
        //                     .node_ref
        //                     .span()
        //                     .unwrap()
        //                     .error("Undefined node reference")
        //                     .emit();
        //                 failed = true;
        //                 continue;
        //             }
        //         };
            
        //     // If the handler was not already bound, then bind it, else error.
        //     // TODO: figure out how to have the root use the same code
        //     match &handler_binding.handler_type {
        //         HandlerType::Executor(_) => match node_data_for_binding.executes {
        //             Some(handler) => {
        //                 handler
        //                     .span()
        //                     .unwrap()
        //                     .error("Multiple executors cannot be bound to a node")
        //                     .span_note(handler.span().unwrap(), "Executor first bound here")
        //                     .emit();
        //                 failed = true;
        //             }
        //             None => node_data_for_binding.executes = Some(handler_binding),
        //         },

        //         HandlerType::Suggester(_) => match node_data_for_binding.suggests {
        //             Some(handler) => {
        //                 handler_binding
        //                     .span()
        //                     .unwrap()
        //                     .error("Multiple suggestion generators cannot be bound to a node")
        //                     .span_note(
        //                         handler.span().unwrap(),
        //                         "Suggestion generator first bound here",
        //                     )
        //                     .emit();
        //                 failed = true;
        //             }
        //             None => node_data_for_binding.suggests = Some(handler_binding),
        //         },
        //     }
        // }

        // // If we failed the previous step or any nodes have duplicate successors, then a valid
        // // graph does not exist for this command.
        // if failed || graph.has_successor_duplicates() {
        //     None
        // } else {
            // Figure out which arguments are guaranteed to have values so we know what to unwrap
            // during dispatching.
            graph.track_defined_args();

            Some(graph)
        // }
    }

    fn node_definition(&self, node: &'a Node) -> &'a Node {
        match node {
            Node::CommandRoot(_) => node,
            Node::Any { .. } => node,
            Node::Argument { ty: Some(_), .. } => node,
            Node::Literal { .. } => node,
            _ => {
                match self.flat_graph.get(&node.unique_name()).map(|data| data.definition) {
                    Some(node_defn) => return node_defn,
                    None => {}
                }

                match self.named_any_nodes.get(&node.unique_name()) {
                    Some(any_node) => any_node,
                    None => node
                }
            }
        }
    }

    fn resolve_successor(&mut self, node: &'a Node, successor: Option<&'a Node>) -> bool {
        // Flatten a potential `Any` node
        let successors = successor
            .into_iter()
            .flat_map(Node::flatten)
            .map(|node| self.node_definition(node))
            .collect::<Vec<_>>();

        match node {
            Node::Argument {
                name: raw_node_name,
                greedy,
                ty: node_type,
                default,
                ..
            } => {
                // Grab the unique name of the node
                let unique_name = node.unique_name();

                // // If this node has already been defined, then grab information about its
                // // definition, including whether or not it's greedy and/or has a default value.
                // // If the node was not already defined, then use the provided node's information.
                // let (cached_greedy, cached_default) = match self.flat_graph.get(&unique_name) {
                //     Some(NodeData {
                //         definition:
                //             Node::Argument {
                //                 greedy, default, ..
                //             },
                //         ..
                //     }) => (greedy, default),
                //     _ => (greedy, default),
                // };

                // // Since greedy nodes consume the command buffer, it's illogical for them to have
                // // successors
                // if cached_greedy.is_some() && successor.is_some() {
                //     successor
                //         .span()
                //         .unwrap()
                //         .error("Greedy arguments cannot have successors.")
                //         .emit();
                //     return false;
                // }

                // // A node with a default value must have successors with default values as well.
                // if cached_default.is_some()
                //     && successor.is_some()
                //     && !successor
                //         .clone()
                //         .into_iter()
                //         .flat_map(Node::flatten)
                //         .all(|node| {
                //             matches!(node, Node::Argument {
                //                 default: Some(_),
                //                 ..
                //             })
                //         })
                // {
                //     successor
                //         .span()
                //         .unwrap()
                //         .error(
                //             "Cannot have non-default argument successors after a default argument \
                //              node",
                //         )
                //         .emit();
                //     return false;
                // }

                // Try to insert the node into the flat graph, and check for duplicate definitions
                match self.flat_graph.get_mut(&unique_name) {
                    // The node has already been defined
                    Some(node_data) => {
                        // // If there's a type specifier, then the user is trying to illegally
                        // // redefine the node
                        // if node_type.is_some() {
                        //     raw_node_name
                        //         .span()
                        //         .unwrap()
                        //         .error("Argument already defined")
                        //         .span_note(
                        //             node_data.definition.span().unwrap(),
                        //             "Argument first defined here",
                        //         )
                        //         .emit();
                        //     return false;
                        // }

                        node_data.successors.extend(successors);
                    }
                    // The node has not already been defined
                    None => {
                        // // If no type was given to the node, then it's not well-defined
                        // if node_type.is_none() {
                        //     raw_node_name
                        //         .span()
                        //         .unwrap()
                        //         .error("Argument not defined")
                        //         .help("Consider adding a type specifier")
                        //         .emit();
                        //     return false;
                        // }

                        self.flat_graph.insert(
                            unique_name,
                            NodeData::with_successors(node, successors),
                        );
                    }
                }
            }

            Node::Literal { .. } => {
                // Literals are easier to handle. If they were previously defined, extend their
                // successor list, else define them with the given successors.

                let unique_name = node.unique_name();

                match self.flat_graph.get_mut(&unique_name) {
                    Some(node_data) => {
                        node_data.successors.extend(successors);
                    }
                    None => {
                        self.flat_graph.insert(
                            unique_name,
                            NodeData::with_successors(node, successors),
                        );
                    }
                }
            }

            Node::CommandRoot(_) => {
                // // `root` cannot have any explicitly defined successors because we resolve that ourselves.

                // if successor.is_none() {
                //     return true;
                // }

                // node.span()
                //     .unwrap()
                //     .error("Root nodes cannot have explicitly defined successors")
                //     .span_help(
                //         successor.span().unwrap(),
                //         "Consider removing this successor",
                //     )
                //     .emit();
                // return false;
            }
            
            Node::Any { nodes, .. } => {
                // Flatten out the node by resolving the provided successor with every node within
                // this node

                return nodes
                    .iter()
                    .all(|node| self.resolve_successor(node, successor));
            }
        }

        // Yay!
        true
    }

    // /// Iterate over all the nodes with all their successors and make sure there are not any
    // /// duplicates. Note that root successor duplicates are ignored during successor resolution.
    // fn has_successor_duplicates(&self) -> bool {
    //     for (_, node_data) in &self.flat_graph {
    //         for i in 0 .. node_data.successors.len() {
    //             for j in i + 1 .. node_data.successors.len() {
    //                 let successor0 = node_data.successors[i];
    //                 let successor1 = node_data.successors[j];

    //                 if node_name_eq(successor0, successor1) {
    //                     successor1
    //                         .span()
    //                         .unwrap()
    //                         .error("Duplicate successor definition")
    //                         .span_note(
    //                             successor0.span().unwrap(),
    //                             "Successor originally defined here",
    //                         )
    //                         .emit();
    //                     return true;
    //                 }
    //             }
    //         }
    //     }

    //     false
    // }

    /// Track which arguments have defined defined values by endowing their successors with that
    /// information.
    fn track_defined_args(&mut self) {
        for &node in self.root_successors.iter() {
            self.resolve_defined_args(node, Vec::new());
        }
    }

    /// Attempt to resolve which arguments have a defined value for the given node.
    fn resolve_defined_args(
        &self,
        node: &'a Node,
        mut stack: Vec<&'a Node>,
    ) {
        // // The only other option is the root, which is considered to have no arguments defined
        // if !matches!(node, Node::Argument { .. } | Node::Literal { .. }) {
        //     return;
        // }

        // // We've detected a cycle and therefore can exit
        // if stack.iter().any(|&stack_node| node_name_eq(node, stack_node)) {
        //     return;
        // }

        // // This node is now considered defined since we're visiting it, but we only keep track of
        // // non-default argument nodes
        // if matches!(node, Node::Argument { default: None, .. }) {
        //     stack.push(node);
        // }

        // Update the defined arguments with the current stack, see `update` for details
        let node_data = self.flat_graph.get(&node.unique_name()).unwrap();
        // node_data.defined_arguments.update(&stack);

        // No successors means we're done
        if node_data.successors.is_empty() {
            return;
        }

        // We only need to copy the stack if there are more than one successor
        for &successor in node_data.successors.iter().take(node_data.successors.len() - 1) {
            self.resolve_defined_args(successor, stack.clone());
        }

        // Unwrap guaranteed to work because of the check for an empty successor list
        self.resolve_defined_args(node_data.successors.last().unwrap(), stack);
    }

    // Generates a struct for all the arguments in a given command. All non-default arguments
    // will be represented as options, whereas default arguments will be the type they are
    // defined as.
    // fn gen_data_struct(&self, name: &impl quote::IdentFragment) -> TokenStream {
    //     let name = ident_data_struct(name);

    //     // Iterator over node data and node names
    //     let iter = self
    //         .flat_graph
    //         .iter()
    //         .filter(|(_, data)| matches!(data.definition, Node::Argument { .. }));

    //     let names = iter
    //         .clone()
    //         .map(|(name, _)| format_ident!("{}", name))
    //         .collect::<Vec<_>>();

    //     let types = iter.clone().map(|(_, data)| match data.definition {
    //         Node::Argument {
    //             ty: Some(ty),
    //             default,
    //             ..
    //         } => match default {
    //             Some(_) => quote! { #ty },
    //             None => quote! { ::core::option::Option<#ty> },
    //         },
    //         _ => unreachable!(),
    //     });

    //     let defaults = iter.clone().map(|(_, data)| match data.definition {
    //         Node::Argument { default, .. } => match default {
    //             Some(lit) => quote! { #lit },
    //             None => quote! { ::core::option::Option::None },
    //         },
    //         _ => unreachable!(),
    //     });

    //     quote! {
    //         pub struct #name<'cmd> {
    //             #( pub #names : #types, )*
    //             _phantom: ::std::marker::PhantomData<&'cmd ()>
    //         }

    //         impl<'cmd> ::core::default::Default for #name<'cmd> {
    //             fn default() -> Self {
    //                 Self {
    //                     #( #names : #defaults, )*
    //                     _phantom: ::std::marker::PhantomData
    //                 }
    //             }
    //         }
    //     }
    // }

    // Generates code to unpack a data struct into its fields, unwrapping fields which are
    // guaranteed to be defined.
    // fn gen_unpack_data_struct(
    //     &self,
    //     name: &impl quote::IdentFragment,
    //     data_mod_name: &impl ToTokens,
    //     defined_args: &[&Node],
    // ) -> TokenStream {
    //     let name = ident_data_struct(name);

    //     let names = self
    //         .flat_graph
    //         .iter()
    //         .filter(|(_, data)| matches!(data.definition, Node::Argument { .. }))
    //         .map(|(name, data)| (name, data.definition))
    //         .clone()
    //         .map(|(str_name, _)| format_ident!("{}", str_name));

    //     let unwraps = self
    //         .flat_graph
    //         .iter()
    //         .filter(|(_, data)| matches!(data.definition, Node::Argument { default: None, .. }))
    //         .map(|(name, data)| (name, data.definition))
    //         .filter(|&(_, node)| defined_args.into_iter().any(|defined_node| node_name_eq(node, defined_node)))
    //         .map(|(str_name, _)| format_ident!("{}", str_name))
    //         .collect::<Vec<_>>();

    //     quote! {
    //         let #data_mod_name::#name { #( #names, )* .. } = __data;
    //         #( let #unwraps = #unwraps.unwrap(); )*
    //     }
    // }

    // Generates all of the dispatch and suggestion generation functions for this command.
    // fn gen_node_fns<T>(
    //     &self,
    //     name: &T,
    //     data_mod_name: &impl ToTokens,
    //     context_type: &Box<Type>,
    //     context_lifetime: &Option<Lifetime>,
    // ) -> Option<TokenStream>
    // where
    //     T: quote::IdentFragment + Spanned,
    // {
    //     let root_dispatch_fn = ident_dispatch_node(name);
    //     let root_suggestions_fn = ident_get_suggestions_node(name);

    //     let root_dispatch_context_name = match self.root_executes {
    //         Some(HandlerBinding { context, .. }) => context.inner().clone(),
    //         None => format_ident!("__context"),
    //     };
    //     let root_suggestions_context_name = match self.root_suggests {
    //         Some(HandlerBinding { context, .. }) => context.inner().clone(),
    //         None => format_ident!("__context"),
    //     };
    //     let root_suggestions_arg_name = match self.root_suggests {
    //         Some(HandlerBinding { arg: Some(arg), .. }) => arg.inner().clone(),
    //         _ => format_ident!("__arg"),
    //     };

    //     let data_struct_name = ident_data_struct(name);
    //     let root_dispatch_match = self.gen_dispatch_match(
    //         name,
    //         data_mod_name,
    //         context_type,
    //         name,
    //         self.root_executes.clone(),
    //         &self.root_successors,
    //         &[],
    //     );
    //     let root_suggestions_match = self.gen_suggestions_match(
    //         name,
    //         data_mod_name,
    //         context_type,
    //         None,
    //         self.root_suggests.clone(),
    //         &self.root_successors,
    //     );

    //     let iter = self.flat_graph.iter().map(|(_, node_data)| node_data);

    //     let mut node_dispatch_fn_matches = Vec::with_capacity(self.flat_graph.len());
    //     for NodeData { definition, executes, successors, defined_arguments, .. } in iter.clone() {
    //         node_dispatch_fn_matches.push(self.gen_dispatch_match(
    //             name,
    //             data_mod_name,
    //             context_type,
    //             definition,
    //             *executes,
    //             successors,
    //             &*defined_arguments.args.borrow(),
    //         )?);
    //     }
    //     let mut node_suggestions_fn_matches = Vec::with_capacity(self.flat_graph.len());
    //     for NodeData { definition, suggests, successors, .. } in iter.clone() {
    //         node_suggestions_fn_matches.push(self.gen_suggestions_match(
    //             name,
    //             data_mod_name,
    //             context_type,
    //             Some(definition),
    //             suggests.clone(),
    //             successors,
    //         ));
    //     }

    //     let node_dispatch_fn_names = iter.clone().map(|NodeData { definition, .. }| {
    //         format_ident!("dispatch_{}_{}", name, definition.unique_ident())
    //     });
    //     let node_suggestions_fn_names = iter.clone().map(|NodeData { definition, .. }| {
    //         format_ident!("get_suggestions_{}_{}", name, definition.unique_ident())
    //     });

    //     let node_dispatch_fn_context_names =
    //         iter.clone()
    //             .map(|NodeData { executes, .. }| match executes {
    //                 &Some(handler_binding) => handler_binding.context.inner().clone(),
    //                 None => format_ident!("__context"),
    //             });
    //     let node_suggestions_fn_context_names =
    //         iter.clone()
    //             .map(|NodeData { suggests, .. }| match suggests {
    //                 &Some(handler_binding) => handler_binding.context.inner().clone(),
    //                 None => format_ident!("__context"),
    //             });
    //     let node_fn_arg_names = iter
    //         .clone()
    //         .map(|NodeData { suggests, .. }| match suggests {
    //             &Some(HandlerBinding { arg: Some(arg), .. }) => arg.inner().clone(),
    //             _ => format_ident!("__arg"),
    //         });

    //     Some(quote! {
    //         #(
    //             fn #node_dispatch_fn_names<'cmd, #context_lifetime>(
    //                 __args: &mut ::quartz_commands::ArgumentTraverser<'cmd>,
    //                 mut #node_dispatch_fn_context_names: #context_type,
    //                 mut __data: #data_mod_name::#data_struct_name<'cmd>
    //             ) -> ::core::result::Result<(), ::quartz_commands::Error>
    //             {
    //                 #node_dispatch_fn_matches
    //             }
    //         )*

    //         fn #root_dispatch_fn<'cmd, #context_lifetime>(
    //             __args: &mut ::quartz_commands::ArgumentTraverser<'cmd>,
    //             mut #root_dispatch_context_name: #context_type,
    //             mut __data: #data_mod_name::#data_struct_name<'cmd>
    //         ) -> ::core::result::Result<(), ::quartz_commands::Error>
    //         {
    //             #root_dispatch_match
    //         }

    //         #(
    //             fn #node_suggestions_fn_names<'cmd, #context_lifetime>(
    //                 __args: &mut ::quartz_commands::ArgumentTraverser<'cmd>,
    //                 #node_suggestions_fn_context_names: &#context_type,
    //                 #node_fn_arg_names: &'cmd str,
    //                 __data: &mut #data_mod_name::#data_struct_name<'cmd>
    //             ) -> ::std::vec::Vec<::std::string::String>
    //             {
    //                 let mut __suggestions = ::std::vec::Vec::new();
    //                 #node_suggestions_fn_matches
    //                 __suggestions
    //             }
    //         )*

    //         fn #root_suggestions_fn<'cmd, #context_lifetime>(
    //             __args: &mut ::quartz_commands::ArgumentTraverser<'cmd>,
    //             #root_suggestions_context_name: &#context_type,
    //             #root_suggestions_arg_name: &'cmd str,
    //             __data: &mut #data_mod_name::#data_struct_name<'cmd>
    //         ) -> ::std::vec::Vec<::std::string::String>
    //         {
    //             let mut __suggestions = ::std::vec::Vec::new();
    //             #root_suggestions_match
    //             __suggestions
    //         }
    //     })
    // }

    // Generates a match statement for a node dispatch function.
    // fn gen_dispatch_match(
    //     &self,
    //     root_name: &impl quote::IdentFragment,
    //     data_mod_name: &impl ToTokens,
    //     context_type: &Box<Type>,
    //     definition: &impl Spanned,
    //     executes: Option<&HandlerBinding>,
    //     successors: &[&Node],
    //     defined_args: &[&Node],
    // ) -> Option<TokenStream> {
    //     let context_ident = executes
    //         .map(|binding| binding.context.inner().clone())
    //         .unwrap_or(format_ident!("__context"));

    //     // What to do if there are no arguments left
    //     let none_arm = match executes {
    //         // If there is a executor, then run it
    //         Some(HandlerBinding { handler, .. }) => {
    //             let unpack = self.gen_unpack_data_struct(root_name, data_mod_name, defined_args);
    //             quote! {
    //                 {
    //                     #unpack
    //                     #handler
    //                 }
    //             }
    //         }

    //         // Error based on the successors
    //         None => match successors.len() {
    //             // Invalid graph
    //             0 => {
    //                 definition
    //                     .span()
    //                     .unwrap()
    //                     .error("A node at the end of a branch must have an executor.")
    //                     .emit();
    //                 return None;
    //             }

    //             // We expected one argument
    //             1 => match &successors[0] {
    //                 Node::Argument { name, .. } => {
    //                     let literal = Literal::string(&format!(
    //                         "Expected value for argument \"{}\"",
    //                         name.as_str()
    //                     ));
    //                     quote! { ::core::result::Result::Err(#literal.to_owned()) }
    //                 }
    //                 Node::Literal { lit, .. } => {
    //                     let literal =
    //                         Literal::string(&format!("Expected literal \"{}\"", lit.as_str()));
    //                     quote! { ::core::result::Result::Err(#literal.to_owned()) }
    //                 }
    //                 _ =>
    //                     quote! { ::core::result::Result::Err("Expected additional arguments".to_owned()) },
    //             },

    //             // We expected one of a list of arguments
    //             _ => {
    //                 let successor_list = successors
    //                     .iter()
    //                     .map(|&node| match node {
    //                         Node::Argument { name, .. } => Some(name.to_string()),
    //                         Node::Literal { lit, .. } => Some(format!("\"{}\"", lit.as_str())),
    //                         _ => None,
    //                     })
    //                     .flatten()
    //                     .collect::<Vec<_>>()
    //                     .join(", ");
    //                 let literal = Literal::string(&format!("Expected one of {}", successor_list));
    //                 quote! { ::core::result::Result::Err(#literal.to_owned()) }
    //             }
    //         },
    //     };

    //     // What to do if nothing matches the input arguments. If one of the successors is the root,
    //     // then we match on that and pass the the argument to the root dispatcher, otherwise we
    //     // return an error.
    //     let default_some_arm = if successors
    //         .iter()
    //         .any(|&node| matches!(node, Node::CommandRoot(_)))
    //     {
    //         let root_fn = ident_dispatch_node(root_name);
    //         quote! { Self::#root_fn(__args, #context_ident, __data) }
    //     } else {
    //         quote! { ::core::result::Result::Err(::std::format!("Unexpected argument \"{}\"", __arg)) }
    //     };

    //     // All the nodes which we need to generate match arms for, sorted from most-specific
    //     // to least-specific.
    //     let mut case_nodes = successors
    //         .iter()
    //         .copied()
    //         .filter(|&node| matches!(node, Node::Argument { .. } | Node::Literal { .. }))
    //         .collect::<Vec<_>>();
    //     case_nodes.sort_by(|&a, &b| match (a, b) {
    //         (Node::Argument { .. }, Node::Literal { .. }) => Ordering::Less,
    //         (Node::Literal { .. }, Node::Argument { .. }) => Ordering::Greater,
    //         _ => Ordering::Equal,
    //     });

    //     let branches = case_nodes.into_iter()
    //         .flat_map(|node| {
    //             match node {
    //                 Node::Argument { greedy, ty: Some(ty), default, .. } => {
    //                     let unique_ident = node.unique_ident();
    //                     let dispatch_fn = format_ident!("dispatch_{}_{}", root_name, unique_ident);

    //                     // Determine whether we need to turn the parsed argument into an option due
    //                     // a default value
    //                     let var_wrapper = if default.is_some() {
    //                         None
    //                     } else {
    //                         Some(quote! { ::core::option::Option::Some })
    //                     };

    //                     // Handle greedy strings
    //                     let parser = if greedy.is_some() {
    //                         quote! {
    //                             __args.gobble_remaining().into()
    //                         }
    //                     } else {
    //                         quote! {
    //                             <#ty as ::quartz_commands::FromArgument<'cmd, _>>
    //                                 ::from_arg(__arg, &#context_ident)?
    //                         }
    //                     };

    //                     Some(quote! {
    //                         ::core::option::Option::Some(__arg) if
    //                             <#ty as ::quartz_commands::FromArgument<'cmd, #context_type>>
    //                             ::matches(__arg) =>
    //                         {
    //                             __data.#unique_ident = #var_wrapper (#parser);
    //                             Self::#dispatch_fn(__args, #context_ident, __data)
    //                         }
    //                     })
    //                 },

    //                 Node::Literal { lit, .. } => {
    //                     // Literals just forward to another dispatch function

    //                     let dispatch_fn = format_ident!("dispatch_{}_{}", root_name, node.unique_ident());
    //                     Some(quote! {
    //                         ::core::option::Option::Some(#lit) =>
    //                             Self::#dispatch_fn(__args, #context_ident, __data)
    //                     })
    //                 },

    //                 _ => None
    //             }
    //         });

    //     Some(quote! {
    //         match __args.next() {
    //             #( #branches, )*
    //             ::core::option::Option::Some(__arg @ _) => #default_some_arm,
    //             ::core::option::Option::None => #none_arm
    //         }
    //     })
    // }

    // fn gen_suggestions_match(
    //     &self,
    //     root_name: &impl quote::IdentFragment,
    //     data_mod_name: &impl ToTokens,
    //     context_type: &Box<Type>,
    //     definition: Option<&Node>,
    //     suggests: Option<&HandlerBinding>,
    //     successors: &[&Node],
    // ) -> TokenStream {
    //     let context_ident = suggests
    //         .map(|binding| binding.context.inner().clone())
    //         .unwrap_or(format_ident!("__context"));

    //     // What to do if there isn't an additional argument.
    //     let default_arm = match suggests {
    //         // If there is a handler, then apply it
    //         Some(HandlerBinding { handler, arg, .. }) => {
    //             // Since we allow argument parsing to fail, we can't assume any args are defined
    //             let unpack = self.gen_unpack_data_struct(root_name, data_mod_name, &[]);

    //             Some(quote! {
    //                 {
    //                     #unpack
    //                     let __helper = move || { #handler };
    //                     __suggestions.extend(
    //                         __helper()
    //                             .into_iter()
    //                             .filter(|__cmd| __cmd.starts_with(#arg))
    //                             .map(::core::convert::Into::into)
    //                     );
    //                 }
    //             })
    //         }

    //         // If there is no handler, generate a default handler if the node definition is a
    //         // literal
    //         None => match definition {
    //             Some(Node::Literal { lit, .. }) => Some(quote! {
    //                 if #lit.starts_with(__arg) {
    //                     __suggestions.push(#lit.to_owned());
    //                 }
    //             }),
    //             _ => None,
    //         },
    //     };

    //     // What to do if there is an argument but no branch matches to it. Similar to the dispatch
    //     // functions, if the root is a successor then forward the command to the root suggestion
    //     // generator, else add nothing to the suggestion list.
    //     let default_some_arm = if successors
    //         .iter()
    //         .any(|&node| matches!(node, Node::CommandRoot(_)))
    //     {
    //         let root_fn = ident_get_suggestions_node(root_name);
    //         Some(quote! { __suggestions.extend(Self::#root_fn(__args, #context_ident, __data)); })
    //     } else {
    //         None
    //     };

    //     // Since suggestions are cumulative, there is no need to sort here. If we hit a partial
    //     // match on the argument, then try to parse the argument and call the suggestion
    //     // generation function for the appropriate successor.
    //     let branches = successors
    //         .iter()
    //         .copied()
    //         .flat_map(|node| {
    //             match node {
    //                 Node::Argument { greedy, ty: Some(ty), default, .. } => {
    //                     let unique_ident = node.unique_ident();
    //                     let get_suggestions_fn = format_ident!("get_suggestions_{}_{}", root_name, unique_ident);

    //                     let var_wrapper = if default.is_some() {
    //                         None
    //                     } else {
    //                         Some(quote! { ::core::option::Option::Some })
    //                     };

    //                     let parser = if greedy.is_some() {
    //                         quote! {
    //                             __data.#unique_ident = #var_wrapper (__args.gobble_remaining().into());
    //                         }
    //                     } else {
    //                         quote! {
    //                             if let ::core::result::Result::Ok(value) =
    //                                 <#ty as ::quartz_commands::FromArgument<'cmd, _>>
    //                                 ::from_arg(__arg, &#context_ident)
    //                             {
    //                                 __data.#unique_ident = #var_wrapper (value);
    //                             }
    //                         }
    //                     };

    //                     Some(quote! {
    //                         if <#ty as ::quartz_commands::FromArgument<'cmd, #context_type>>
    //                             ::partial_matches(__arg)
    //                         {
    //                             #parser
    //                             __suggestions.extend(
    //                                 Self::#get_suggestions_fn(__args, #context_ident, __arg, __data)
    //                             );
    //                         }
    //                     })
    //                 },

    //                 Node::Literal { lit, .. } => {
    //                     let get_suggestions_fn = format_ident!("get_suggestions_{}_{}", root_name, node.unique_ident());
    //                     Some(quote! {
    //                         if #lit.starts_with(__arg) {
    //                             __suggestions.extend(Self::#get_suggestions_fn(__args, #context_ident, __arg, __data));
    //                         }
    //                     })
    //                 }

    //                 _ => None
    //             }
    //         });

    //     quote! {
    //         if let ::core::option::Option::Some(__arg) = __args.next() {
    //             #( #branches )*
    //             #default_some_arm
    //         } else {
    //             #default_arm
    //         }
    //     }
    // }
}

struct NodeData<'a> {
    definition: &'a Node,
    executes: Option<&'a HandlerBinding>,
    suggests: Option<&'a HandlerBinding>,
    successors: Vec<&'a Node>,
    defined_arguments: DefinedArguments<'a>,
}

impl<'a> NodeData<'a> {
    pub fn with_successors(definition: &'a Node, successors: Vec<&'a Node>) -> Self {
        NodeData {
            definition,
            executes: None,
            suggests: None,
            successors,
            defined_arguments: DefinedArguments::new(),
        }
    }
}

/// When traversing the graph, we need to differentiate first visits from subsequent visits while
/// also keeping track of the implied defined arguments for every non-default argument node. We
/// use this struct to help out with that process, and make it interiorly-mutable for convenience.
struct DefinedArguments<'a> {
    args: RefCell<Vec<&'a Node>>,
    initialized: Cell<bool>
}

impl<'a> DefinedArguments<'a> {
    const fn new() -> Self {
        DefinedArguments {
            args: RefCell::new(Vec::new()),
            initialized: Cell::new(false)
        }
    }

    // Update which arguments we considered well-defined based on the given set of arguments
    // defined for a certain path. If the `initialized` field is false, then we adopt the entire set
    // of provided arguments and consider them to be defined. If this node was already initialized,
    // then we take the intersection of the current defined arguments with the provided defined
    // arguments.
    // fn update(&self, defined_args: &[&'a Node]) {
    //     if self.initialized.replace(true) {
    //         let mut args = self.args.borrow_mut();
    //         let mut i = 0;
    //         while i < args.len() {
    //             let cur_arg = args[i];
    //             if !defined_args.iter().any(|&arg| node_name_eq(arg, cur_arg)) {
    //                 args.remove(i);
    //             } else {
    //                 i += 1;
    //             }
    //         }
    //     } else {
    //         self.args.borrow_mut().extend(defined_args.into_iter().copied());
    //     }
    // }
}

// fn ident_dispatch_node<T: quote::IdentFragment>(name: &T) -> Ident {
//     format_ident!("dispatch_{}", name)
// }

// fn ident_get_suggestions_node<T: quote::IdentFragment>(name: &T) -> Ident {
//     format_ident!("get_suggestions_{}", name)
// }

// fn ident_data_struct<T: quote::IdentFragment>(name: &T) -> Ident {
//     format_ident!("CommandData{}", name)
// }

// /// Returns whether or not the two nodes share the same name in a way that would cause a name conflict.
// fn node_name_eq(a: &Node, b: &Node) -> bool {
//     match &(a, b) {
//         (Node::Argument { name: name0, .. }, Node::Argument { name: name1, .. }) =>
//             name0 == name1,
//         (Node::Literal { lit: lit0, .. }, Node::Literal { lit: lit1, .. }) => lit0 == lit1,
//         (
//             Node::Argument { name: arg_name, .. },
//             Node::Literal {
//                 renamed: Some(lit_name),
//                 ..
//             },
//         ) => arg_name == lit_name,
//         (
//             Node::Literal {
//                 renamed: Some(lit_name),
//                 ..
//             },
//             Node::Argument { name: arg_name, .. },
//         ) => lit_name == arg_name,
//         (Node::CommandRoot(_), Node::CommandRoot(_)) => true,
//         _ => false,
//     }
// }
