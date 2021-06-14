use crate::keyword;
use once_cell::unsync::OnceCell;
use proc_macro2::{Span, TokenStream};
use quote::{quote, IdentFragment, ToTokens, TokenStreamExt};
use std::fmt::{self, Display, Formatter};
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
    Visibility,
};

pub struct CommandModule {
    pub vis: Visibility,
    pub name: StrIdent,
    pub context_type: Box<Type>,
    pub context_lifetime: Option<Lifetime>,
    pub definitions: Vec<Command>,
}

impl Parse for CommandModule {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let _mod_token: Token![mod] = input.parse()?;
        let name: StrIdent = input.parse()?;
        let _semicolon: Token![;] = input.parse()?;

        let context_type: ItemType = input.parse()?;
        let mut context_lifetime = None;
        if context_type.ident != "Context" {
            return Err(Error::new_spanned(
                context_type,
                "Expected statement of the form `type Context = ContextType;`",
            ));
        }
        if !context_type.generics.params.is_empty() {
            if context_type.generics.params.len() > 1
                || !matches!(
                    context_type.generics.params.first(),
                    Some(GenericParam::Lifetime(_))
                )
            {
                return Err(Error::new_spanned(
                    context_type.generics,
                    "The context type can only have a single lifetime parameter.",
                ));
            }

            let context_lifetime_def = match context_type.generics.params.first() {
                Some(GenericParam::Lifetime(lifetime)) => lifetime,
                _ => unreachable!(),
            };

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

            if context_lifetime_def.lifetime.ident.to_string() != "ctx" {
                return Err(Error::new(
                    context_lifetime_def.span(),
                    "Context lifetime parameter must be named 'ctx.",
                ));
            }

            context_lifetime = Some(context_lifetime_def.lifetime.clone());
        }

        let mut definitions = Vec::new();
        while !input.is_empty() {
            definitions.push(input.parse::<Command>()?);
        }

        Ok(CommandModule {
            vis,
            name,
            context_type: context_type.ty,
            context_lifetime,
            definitions,
        })
    }
}

pub struct Command {
    pub aliases: Vec<StrIdent>,
    pub branches: Vec<Branch>,
    pub handler_bindings: Punctuated<HandlerBinding, Token![;]>,
}

impl Command {
    pub fn name(&self) -> &StrIdent {
        &self.aliases[0]
    }
}

impl Parse for Command {
    fn parse(input: ParseStream) -> Result<Self> {
        let _command_token: keyword::command = input.parse()?;
        let name: StrIdent = input.parse()?;

        let mut aliases = vec![name];
        while input.peek(Token![|]) {
            let _or: Token![|] = input.parse()?;
            let alias: StrIdent = input.parse()?;
            aliases.push(alias);
        }

        let branches = if input.peek(Token![where]) {
            let _where: Token![where] = input.parse()?;
            let mut branches = vec![input.parse::<Branch>()?];

            loop {
                if input.peek(Brace) {
                    break;
                }

                if input.peek(Token![,]) {
                    let _comma: Token![,] = input.parse()?;
                    continue;
                }

                branches.push(input.parse()?);
            }

            branches
        } else {
            Vec::new()
        };

        let content;
        braced!(content in input);

        Ok(Command {
            aliases,
            branches,
            handler_bindings: Punctuated::<HandlerBinding, Token![;]>::parse_terminated(&content)?,
        })
    }
}

pub struct Branch {
    pub nodes: Vec<Node>,
}

impl Parse for Branch {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut nodes = vec![input.parse()?];

        while input.peek(Token![=>]) {
            let _fat_arrow: Token![=>] = input.parse()?;
            nodes.push(input.parse()?);
        }

        Ok(Branch { nodes })
    }
}

pub enum Node {
    Argument {
        name: StrIdent,
        colon: Option<Token![:]>,
        ty: Option<Type>,
        eq: Option<Token![=]>,
        default: Option<Lit>,
    },
    Literal(LitStrRef),
    CommandRoot(keyword::root),
    Any {
        any: keyword::any,
        nodes: Punctuated<Self, Token![,]>,
    },
}

impl Node {
    pub fn flatten(&self) -> FlattenNodeIter<'_> {
        match self {
            Node::Argument { .. } | Node::Literal(_) | Node::CommandRoot(_) =>
                FlattenNodeIter::Singleton(Some(self)),
            Node::Any { nodes, .. } => FlattenNodeIter::Multiple(nodes.into_iter()),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Node::Argument { name, .. } => name.as_str(),
            Node::Literal(lit) => lit.as_str(),
            _ => panic!("Only `Argument` and `Literal` node types have names."),
        }
    }
}

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keyword::root) {
            Ok(Node::CommandRoot(input.parse()?))
        } else if lookahead.peek(keyword::any) {
            let any: keyword::any = input.parse()?;
            let content;
            let bracket = bracketed!(content in input);
            let nodes = Punctuated::<Self, Token![,]>::parse_terminated(&content)?;
            if nodes.is_empty() {
                Err(Error::new(
                    bracket.span,
                    "`any` clause must specify at least one node.",
                ))
            } else {
                for node in &nodes {
                    if matches!(node, Node::Any { .. }) {
                        return Err(Error::new_spanned(
                            node,
                            "Nested `any` clauses are not allowed.",
                        ));
                    }
                }

                Ok(Node::Any { any, nodes })
            }
        } else if lookahead.peek(Ident) {
            let name = input.parse()?;
            let (colon, ty) = if input.peek(Token![:]) {
                let colon: Token![:] = input.parse()?;
                (Some(colon), Some(input.parse()?))
            } else {
                (None, None)
            };

            let (eq, default) = if input.peek(Token![=]) && !input.peek(Token![=>]) {
                let eq: Token![=] = input.parse()?;
                let default: Lit = input.parse()?;

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
                ty,
                eq,
                default,
            })
        } else if lookahead.peek(LitStr) {
            let lit: LitStrRef = input.parse()?;
            if lit.as_str().chars().any(|ch| {
                (ch as u32) < 0x21 || (ch as u32) > 0x7E || ch == '"' || ch == '\'' || ch == '\\'
            }) {
                Err(Error::new_spanned(
                    lit,
                    "String literal must contain non-whitespace ASCII excluding '\"', '\\'', and \
                     '\\'",
                ))
            } else {
                Ok(Node::Literal(lit))
            }
        } else {
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
                ty,
                eq,
                default,
            } => quote! {
                #name #colon #ty #eq #default
            },
            Node::Literal(lit) => quote! { #lit },
            Node::CommandRoot(root) => quote! { #root },
            Node::Any { any, nodes } => quote! {
                #any [ #nodes ]
            },
        };
        tokens.append_all(tks);
    }
}

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

pub enum NodeRef {
    Argument(StrIdent),
    Literal(LitStrRef),
    Root(keyword::root),
}

impl NodeRef {
    pub fn name(&self) -> &str {
        match self {
            NodeRef::Argument(name) => name.as_str(),
            NodeRef::Literal(lit) => lit.as_str(),
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
            Ok(NodeRef::Argument(input.parse()?))
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
            NodeRef::Argument(ident) => ident.to_tokens(tokens),
            NodeRef::Literal(lit) => lit.to_tokens(tokens),
            NodeRef::Root(root) => root.to_tokens(tokens),
        }
    }
}

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

        let lookahead = input.lookahead1();
        let handler_type = if lookahead.peek(keyword::executes) {
            HandlerType::Executor(input.parse()?)
        } else if lookahead.peek(keyword::suggests) {
            HandlerType::Suggester(input.parse()?)
        } else {
            return Err(lookahead.error());
        };

        let or1_token: Token![|] = input.parse()?;
        let context: StrIdent = match input.parse() {
            Ok(context) => context,
            Err(error) => return Err(Error::new(error.span(), "Expected context identifier.")),
        };

        let (comma, arg) = if matches!(handler_type, HandlerType::Suggester(_)) {
            let comma: Token![,] = input.parse()?;
            let arg: StrIdent = input.parse()?;
            (Some(comma), Some(arg))
        } else {
            (None, None)
        };

        let or2_token: Token![|] = input.parse()?;

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
    pub fn inner(&self) -> &LitStr {
        &self.lit_str
    }

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
