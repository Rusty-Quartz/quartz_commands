use crate::{keyword, path_matches};
use once_cell::unsync::OnceCell;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, IdentFragment, ToTokens, TokenStreamExt};
use std::{
    collections::hash_map::DefaultHasher,
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
};
use syn::{
    braced,
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Brace,
    Block,
    Error,
    Expr,
    GenericParam,
    Ident,
    ItemType,
    Lifetime,
    Lit,
    LitStr,
    Result,
    Token,
    Type,
    TypeReference,
    Visibility,
};

/// Contains the entirety of a command module definition.
pub struct CommandModule {
    /// Visibility to be applied to the generated module.
    pub vis: Visibility,
    /// The name of the module.
    pub name: StrIdent,
    /// The context type of the module.
    pub context_type: Box<Type>,
    /// The lifetime associated with the context (if any).
    pub context_lifetime: Option<Lifetime>,
    /// A list of command definitions associated with the module.
    pub definitions: Vec<Command>,
}

impl Parse for CommandModule {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse `[vis] mod <name>;`
        let vis: Visibility = input.parse()?;
        let _mod_token: Token![mod] = input.parse()?;
        let name: StrIdent = input.parse()?;
        let _semicolon: Token![;] = input.parse()?;

        // Parse `type Context[<'ctx>] = <context_type>;`

        // Parse the whole type definition
        let context_type_defn: ItemType = input.parse()?;
        let mut context_lifetime = None;

        // Ensure the type is named `Context`
        if context_type_defn.ident != "Context" {
            return Err(Error::new_spanned(
                context_type_defn,
                "Expected statement of the form `type Context = ContextType;`",
            ));
        }

        // If there are generics, ensure it's just <'ctx>
        if !context_type_defn.generics.params.is_empty() {
            if context_type_defn.generics.params.len() > 1
                || !matches!(
                    context_type_defn.generics.params.first(),
                    Some(GenericParam::Lifetime(_))
                )
            {
                return Err(Error::new_spanned(
                    context_type_defn.generics,
                    "The context type can only have a single lifetime parameter.",
                ));
            }

            // Extract the lifetime
            let context_lifetime_def = match context_type_defn.generics.params.first() {
                Some(GenericParam::Lifetime(lifetime)) => lifetime,
                // Guaranteed by the check above this one
                _ => unreachable!(),
            };

            // Make sure 'ctx is not bounded by anything
            if context_lifetime_def.colon_token.is_some() {
                return Err(Error::new(
                    context_lifetime_def
                        .colon_token
                        .span()
                        .join(context_lifetime_def.bounds.span())
                        .unwrap(),
                    "Context lifetime parameter cannot have bounds.",
                ));
            }

            // Make sure the lifetime is actually called 'ctx
            if context_lifetime_def.lifetime.ident != "ctx" {
                return Err(Error::new(
                    context_lifetime_def.span(),
                    "Context lifetime parameter must be named 'ctx.",
                ));
            }

            context_lifetime = Some(context_lifetime_def.lifetime.clone());
        }

        // Parse the command definitions while there are tokens remaining
        let mut definitions = Vec::new();
        while !input.is_empty() {
            definitions.push(input.parse::<Command>()?);
        }

        Ok(CommandModule {
            vis,
            name,
            context_type: context_type_defn.ty,
            context_lifetime,
            definitions,
        })
    }
}

/// A complete command definition, including aliases, branch definitions, and handler bindings.
pub struct Command {
    /// The aliases for the command. This vec is guaranteed to have at least one item, with the
    /// first item being the name of the command.
    pub aliases: Vec<StrIdent>,
    /// The argument branches.
    pub branches: Vec<Branch>,
    /// The handler bindings for arguments (currently only execution and suggestion blocks).
    pub handler_bindings: Punctuated<HandlerBinding, Token![;]>,
}

impl Command {
    pub fn name(&self) -> &StrIdent {
        &self.aliases[0]
    }
}

impl Parse for Command {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse `command <name>`
        let _command_token: keyword::command = input.parse()?;
        let name: StrIdent = input.parse()?;

        // Aliases are specified with the pattern <name> | <alias1> | <alias2>...
        let mut aliases = vec![name];
        while input.peek(Token![|]) {
            // All aliases must be prefixed by `|`
            let _or: Token![|] = input.parse()?;
            let alias: StrIdent = input.parse()?;

            aliases.push(alias);
        }

        // We exploit the where clause syntax to define node branches, however commands
        // are not required to have branches.
        let branches = if input.peek(Token![where]) {
            // If there's a `where` token, there must be at least one branch
            let _where: Token![where] = input.parse()?;
            let mut branches = vec![input.parse::<Branch>()?];

            loop {
                // If we found a brace, then we move on to handler bindings
                if input.peek(Brace) {
                    break;
                }

                // A comma indicates the start of a new branch
                if input.peek(Token![,]) {
                    let _comma: Token![,] = input.parse()?;
                    // Continue because trailing commas are allowed before a brace
                    continue;
                }

                // Parse a branch
                branches.push(input.parse()?);
            }

            branches
        } else {
            Vec::new()
        };

        // Grab the handler binding tokens between the braces
        let content;
        braced!(content in input);

        Ok(Command {
            aliases,
            branches,
            handler_bindings: Punctuated::<HandlerBinding, Token![;]>::parse_terminated(&content)?,
        })
    }
}

// A node branch.
pub struct Branch {
    pub nodes: Vec<Node>,
}

impl Parse for Branch {
    fn parse(input: ParseStream) -> Result<Self> {
        // All branches must have at least one node
        let mut nodes = vec![input.parse()?];

        // Nodes are separated by fat arrows, such as "a" => "b" => "c"
        while input.peek(Token![=>]) {
            let _fat_arrow: Token![=>] = input.parse()?;
            nodes.push(input.parse()?);
        }

        Ok(Branch { nodes })
    }
}

/// All valid node types.
pub enum Node {
    /// An argument node, or a node which can take on a value. Argument nodes can be of any
    /// type which implements FromArgument, and they can also have a default value. Nodes of
    /// the type String and &'cmd str can be declared as `greedy`, meaning they consume the
    /// rest of the command buffer.
    Argument {
        name: StrIdent,
        colon: Option<Token![:]>,
        greedy: Option<keyword::greedy>,
        ty: Option<Type>,
        eq: Option<Token![=]>,
        default: Option<Lit>,
    },
    /// A literal node, which is represented by a string literal. These can be renamed and
    /// bound to an identifier. Examples include `"foo"`, and `"foo" as foo_rebound`.
    Literal {
        lit: LitStrRef,
        as_token: Option<Token![as]>,
        renamed: Option<StrIdent>,
    },
    /// The root of the command, represented by the keyword `root`.
    CommandRoot(keyword::root),
    /// A list of nodes using the syntax `any[<node1>, <node2>, ...]`. This node type will
    /// be flattened out into its elements during generation.
    Any {
        any: keyword::any,
        name: Option<StrIdent>,
        nodes: Punctuated<Self, Token![,]>,
    },
}

impl Node {
    /// Flattens out this node into an iterator over one or nodes. The nodes produced by this
    /// iterator are guaranteed to not contain any `Any` nodes.
    pub fn flatten(&self) -> FlattenNodeIter<'_> {
        match self {
            Node::Argument { .. } | Node::Literal { .. } | Node::CommandRoot(_) =>
                FlattenNodeIter::Singleton(Some(self)),
            Node::Any { nodes, .. } => FlattenNodeIter::Multiple(nodes.into_iter()),
        }
    }

    /// Simply returns the string form of this node's unique ident.
    pub fn unique_name(&self) -> String {
        self.unique_ident().to_string()
    }

    /// Returns a unique, valid identifier for this node. Arguments will simply return their name,
    /// whereas literals will return an identifier with a hash of the literal incorporated.
    ///
    /// # Panics
    /// Panics if this node is not an argument or literal.
    pub fn unique_ident(&self) -> Ident {
        match self {
            Node::Argument { name, .. } => name.ident.clone(),
            Node::Literal { lit, renamed, .. } => match renamed {
                Some(name) => name.ident.clone(),
                None => unique_ident_for_lit_str(lit)
            },
            _ => panic!("Only `Argument` and `Literal` node types have names."),
        }
    }
}

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        // See if we're the root node
        if lookahead.peek(keyword::root) {
            Ok(Node::CommandRoot(input.parse()?))
        }
        // Parse an `Any` node
        else if lookahead.peek(keyword::any) {
            let any: keyword::any = input.parse()?;

            let name = if input.peek(Ident) {
                Some(input.parse()?)
            } else {
                None
            };

            // Grab the content between the square brackets
            let content;
            let bracket = bracketed!(content in input);

            // Parse the content between the brackets as a comma-separated list of nodes
            let nodes = Punctuated::<Self, Token![,]>::parse_terminated(&content)?;

            // `Any` nodes must have at least one node
            if nodes.is_empty() {
                Err(Error::new(
                    bracket.span,
                    "`any` clause must specify at least one node.",
                ))
            }
            // `Any` nodes cannot be nested within each other
            else {
                for node in &nodes {
                    if matches!(node, Node::Any { .. }) {
                        return Err(Error::new_spanned(
                            node,
                            "Nested `any` clauses are not allowed.",
                        ));
                    }
                }

                Ok(Node::Any { any, name, nodes })
            }
        }
        // Any other identifier signals the start of an argument node
        else if lookahead.peek(Ident) {
            // All argument nodes have a name
            let name = input.parse()?;

            // Argument nodes only need a type if they are being defined for the first time
            let (colon, ty, greedy) = if input.peek(Token![:]) {
                // Parse the pattern `: [greedy] <ty>`

                let colon: Token![:] = input.parse()?;
                let greedy: Option<keyword::greedy> = if input.peek(keyword::greedy) {
                    Some(input.parse()?)
                } else {
                    None
                };
                let ty = input.parse()?;

                // If the node is declared to be greedy, we need to validate the type
                if greedy.is_some() {
                    // Make sure it matches String or &str
                    let is_valid = match &ty {
                        Type::Reference(TypeReference { elem, .. }) => match &**elem {
                            Type::Path(type_path) => path_matches(type_path, "str"),
                            _ => false,
                        },
                        Type::Path(type_path) => path_matches(type_path, "String"),
                        _ => false,
                    };

                    if !is_valid {
                        return Err(Error::new(
                            ty.span(),
                            "Greedy arguments must be of type String or &str.",
                        ));
                    }
                }

                (Some(colon), Some(ty), greedy)
            } else {
                (None, None, None)
            };

            // Optionally parse a default value for the token
            let (eq, default) = if input.peek(Token![=]) && !input.peek(Token![=>]) {
                // Parse the pattern `= <default>`
                let eq: Token![=] = input.parse()?;
                let default: Lit = input.parse()?;

                // Enforce that default values are specified on the original node definition
                if colon.is_none() || ty.is_none() {
                    return Err(Error::new(
                        eq.span.join(default.span()).unwrap(),
                        "Default values not allowed without a type specifier.",
                    ));
                }

                (Some(eq), Some(default))
            } else {
                (None, None)
            };

            Ok(Node::Argument {
                name,
                colon,
                greedy,
                ty,
                eq,
                default,
            })
        }
        // Parse a literal node
        else if lookahead.peek(LitStr) {
            // Grab the string literal
            let lit: LitStrRef = input.parse()?;

            // Make sure the literal is ASCII excluding quotes and backslashes
            if lit.as_str().chars().any(|ch| {
                (ch as u32) < 0x21 || (ch as u32) > 0x7E || ch == '"' || ch == '\'' || ch == '\\'
            }) {
                return Err(Error::new_spanned(
                    lit,
                    "String literal must contain non-whitespace ASCII excluding '\"', '\\'', and \
                     '\\'",
                ));
            }

            // If the literal is renamed, parse those tokens too
            let (as_token, renamed) = if input.peek(Token![as]) {
                (Some(input.parse()?), Some(input.parse()?))
            } else {
                (None, None)
            };

            Ok(Node::Literal {
                lit,
                as_token,
                renamed,
            })
        }
        // Not a valid node
        else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tks = match self {
            Node::Argument {
                name,
                colon,
                greedy,
                ty,
                eq,
                default,
            } => quote! {
                #name #colon #greedy #ty #eq #default
            },
            Node::Literal {
                lit,
                as_token,
                renamed,
            } => quote! { #lit #as_token #renamed },
            Node::CommandRoot(root) => quote! { #root },
            Node::Any { any, name, nodes } => quote! {
                #any #name [ #nodes ]
            },
        };
        tokens.append_all(tks);
    }
}

/// Iterator type for flattening nodes
pub enum FlattenNodeIter<'a> {
    Singleton(Option<&'a Node>),
    Multiple(<&'a Punctuated<Node, Token![,]> as IntoIterator>::IntoIter),
}

impl<'a> Iterator for FlattenNodeIter<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FlattenNodeIter::Singleton(node) => node.take(),
            FlattenNodeIter::Multiple(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            FlattenNodeIter::Singleton(node) => node.into_iter().size_hint(),
            FlattenNodeIter::Multiple(iter) => iter.size_hint(),
        }
    }
}

/// Similar to `Node`, however this type on represents node references, not node definitions.
/// Argument nodes are referenced by their name, literals by their string literal or their
/// renamed identifier, and the command root by the keyword `root`.
pub enum NodeRef {
    Ident(StrIdent),
    Literal(LitStrRef),
    Root(keyword::root),
}

impl NodeRef {
    /// Returns the result of `unique_ident` as a string.
    pub fn unique_name(&self) -> String {
        self.unique_ident().to_string()
    }

    /// Same behavior as `Node::unique_ident`. Note that renamed literals are represented with
    /// the `Ident` variant.
    pub fn unique_ident(&self) -> Ident {
        match self {
            NodeRef::Ident(name) => name.ident.clone(),
            NodeRef::Literal(lit) => unique_ident_for_lit_str(lit),
            _ => panic!("Only `Argument` and `Literal` node types have names."),
        }
    }
}

impl Parse for NodeRef {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keyword::root) {
            Ok(NodeRef::Root(input.parse()?))
        } else if lookahead.peek(Ident) {
            Ok(NodeRef::Ident(input.parse()?))
        } else if lookahead.peek(LitStr) {
            Ok(NodeRef::Literal(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for NodeRef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            NodeRef::Ident(ident) => ident.to_tokens(tokens),
            NodeRef::Literal(lit) => lit.to_tokens(tokens),
            NodeRef::Root(root) => root.to_tokens(tokens),
        }
    }
}

/// A wrapper around the different ways of expression handlers, either as a single expression
/// or a block.
pub enum Handler {
    Expression(Expr),
    Block(Block),
}

impl Parse for Handler {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Brace) {
            let content;
            let brace = braced!(content in input);
            let statements = content.call(Block::parse_within)?;

            Ok(Handler::Block(Block {
                brace_token: brace,
                stmts: statements,
            }))
        } else {
            Ok(Handler::Expression(input.parse()?))
        }
    }
}

impl ToTokens for Handler {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match self {
            Handler::Expression(expr) => quote! { #expr },
            Handler::Block(block) => quote! { #block },
        });
    }
}

/// Either the keyword `executes` or `suggests`.
pub enum HandlerType {
    Executor(keyword::executes),
    Suggester(keyword::suggests),
}

impl ToTokens for HandlerType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match self {
            HandlerType::Executor(executes) => quote! { #executes },
            HandlerType::Suggester(suggests) => quote! { #suggests },
        });
    }
}

/// A handler binding for a node. Bindings are in the form `<node-ref> [executes|suggests]
/// |<context-ident> [, <arg_ident>]| <handler>;` This type represents both executor
/// and suggester bindings, and guarantees that all appropriate fields will be present
/// depending on the binding type.
pub struct HandlerBinding {
    pub node_ref: NodeRef,
    pub handler_type: HandlerType,
    pub or1_token: Token![|],
    pub context: StrIdent,
    pub comma: Option<Token![,]>,
    pub arg: Option<StrIdent>,
    pub or2_token: Token![|],
    pub handler: Handler,
}

impl Parse for HandlerBinding {
    fn parse(input: ParseStream) -> Result<Self> {
        let node_ref = input.parse()?;

        // Determine whether we are an executor or suggester.
        let lookahead = input.lookahead1();
        let handler_type = if lookahead.peek(keyword::executes) {
            HandlerType::Executor(input.parse()?)
        } else if lookahead.peek(keyword::suggests) {
            HandlerType::Suggester(input.parse()?)
        } else {
            return Err(lookahead.error());
        };

        // Parse the beginning of the closure-like structure
        let or1_token: Token![|] = input.parse()?;
        let context: StrIdent = match input.parse() {
            Ok(context) => context,
            Err(error) => return Err(Error::new(error.span(), "Expected context identifier.")),
        };

        // Enforce that suggesters accept an argument
        let (comma, arg) = if matches!(handler_type, HandlerType::Suggester(_)) {
            let comma: Token![,] = input.parse()?;
            let arg: StrIdent = input.parse()?;
            (Some(comma), Some(arg))
        } else {
            (None, None)
        };

        // Parse the end of the closure-like structure
        let or2_token: Token![|] = input.parse()?;

        // Parse the handler
        let handler: Handler = input.parse()?;

        Ok(HandlerBinding {
            node_ref,
            handler_type,
            or1_token,
            context,
            comma,
            arg,
            or2_token,
            handler,
        })
    }
}

impl ToTokens for HandlerBinding {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let HandlerBinding {
            node_ref,
            handler_type,
            or1_token,
            context,
            comma,
            arg,
            or2_token,
            handler,
        } = self;

        tokens.append_all(quote! {
            #node_ref #handler_type #or1_token #context #comma #arg #or2_token #handler
        });
    }
}

pub struct StrIdent {
    ident: Ident,
    as_string: OnceCell<String>,
}

impl StrIdent {
    pub fn inner(&self) -> &Ident {
        &self.ident
    }

    pub fn as_str(&self) -> &str {
        self.as_string.get_or_init(|| self.ident.to_string())
    }
}

impl PartialEq for StrIdent {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Eq for StrIdent {}

impl Parse for StrIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(StrIdent {
            ident,
            as_string: OnceCell::new(),
        })
    }
}

impl ToTokens for StrIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens)
    }
}

impl IdentFragment for StrIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }

    fn span(&self) -> Option<Span> {
        Some(self.ident.span())
    }
}

impl Display for StrIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

pub struct LitStrRef {
    lit_str: LitStr,
    as_string: String,
}

impl LitStrRef {
    pub fn as_str(&self) -> &str {
        &self.as_string
    }
}

impl PartialEq for LitStrRef {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for LitStrRef {}

impl Parse for LitStrRef {
    fn parse(input: ParseStream) -> Result<Self> {
        let lit_str: LitStr = input.parse()?;
        let as_string = lit_str.value();
        Ok(LitStrRef { lit_str, as_string })
    }
}

impl ToTokens for LitStrRef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lit_str.to_tokens(tokens)
    }
}

impl IdentFragment for LitStrRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }

    fn span(&self) -> Option<Span> {
        Some(self.lit_str.span())
    }
}

impl Display for LitStrRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

fn unique_ident_for_lit_str(lit_str: &LitStrRef) -> Ident {
    // According to the documentation on DefaultHasher, all instances of DefaultHasher
    // created via `new` are guaranteed to be equivalent, so the generated unique ident
    // is invariant across function calls
    let mut state = DefaultHasher::new();
    lit_str.as_str().hash(&mut state);
    format_ident!("lit{:x}", state.finish())
}
