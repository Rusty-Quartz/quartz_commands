use crate::keyword;
use syn::{
    braced,
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Brace,
    Block,
    Error,
    Expr,
    Ident,
    LitStr,
    Result,
    Token,
    Type,
    Visibility,
};

pub struct CommandModule {
    pub vis: Visibility,
    pub name: Ident,
    pub definitions: Vec<Command>,
}

impl Parse for CommandModule {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let _mod_token: Token![mod] = input.parse()?;
        let name: Ident = input.parse()?;
        let _semicolon: Token![;] = input.parse()?;

        let mut definitions = Vec::new();
        while !input.is_empty() {
            definitions.push(input.parse::<Command>()?);
        }

        Ok(CommandModule {
            vis,
            name,
            definitions,
        })
    }
}

pub struct Command {
    pub name: Ident,
    pub aliases: Vec<Ident>,
    pub branches: Vec<Branch>,
    pub handler_bindings: Punctuated<HandlerBinding, Token![;]>,
}

impl Parse for Command {
    fn parse(input: ParseStream) -> Result<Self> {
        let _command_token: keyword::command = input.parse()?;
        let name: Ident = input.parse()?;

        let mut aliases = Vec::new();
        while input.peek(Token![|]) {
            let _or: Token![|] = input.parse()?;
            let alias: Ident = input.parse()?;
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
            name,
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
    Argument { name: Ident, ty: Option<Type> },
    Literal(LitStr),
    CommandRoot,
    ModuleRoot,
    GlobalRoot,
    Any(Punctuated<Self, Token![,]>),
}

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keyword::root) {
            let _root: keyword::root = input.parse()?;
            Ok(Node::CommandRoot)
        } else if lookahead.peek(keyword::module_root) {
            let _module_root: keyword::module_root = input.parse()?;
            Ok(Node::ModuleRoot)
        } else if lookahead.peek(keyword::global_root) {
            let _global_root: keyword::global_root = input.parse()?;
            Ok(Node::GlobalRoot)
        } else if lookahead.peek(keyword::any) {
            let _any: keyword::any = input.parse()?;
            let content;
            let bracket = bracketed!(content in input);
            let nodes = Punctuated::<Self, Token![,]>::parse_terminated(&content)?;
            if nodes.is_empty() {
                Err(Error::new(
                    bracket.span,
                    "`any` clause must specify at least one node.",
                ))
            } else {
                Ok(Node::Any(nodes))
            }
        } else if lookahead.peek(Ident) {
            let name = input.parse()?;
            let ty = if input.peek(Token![:]) {
                let _colon: Token![:] = input.parse()?;
                Some(input.parse()?)
            } else {
                None
            };

            Ok(Node::Argument { name, ty })
        } else if lookahead.peek(LitStr) {
            let lit: LitStr = input.parse()?;
            if lit.value().chars().any(|ch| {
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

pub enum NodeRef {
    Argument(Ident),
    Literal(LitStr),
    Root,
}

impl Parse for NodeRef {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keyword::root) {
            Ok(NodeRef::Root)
        } else if lookahead.peek(Ident) {
            Ok(NodeRef::Argument(input.parse()?))
        } else if lookahead.peek(LitStr) {
            Ok(NodeRef::Literal(input.parse()?))
        } else {
            Err(lookahead.error())
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

pub enum HandlerBinding {
    Executes {
        node_ref: NodeRef,
        context: Ident,
        handler: Handler,
    },
    Suggests {
        node_ref: NodeRef,
        context: Ident,
        handler: Handler,
    },
}

impl Parse for HandlerBinding {
    fn parse(input: ParseStream) -> Result<Self> {
        let node_ref = input.parse()?;

        let lookahead = input.lookahead1();
        let is_executes = if lookahead.peek(keyword::executes) {
            let _executes: keyword::executes = input.parse()?;
            true
        } else if lookahead.peek(keyword::suggests) {
            let _suggests: keyword::suggests = input.parse()?;
            false
        } else {
            return Err(lookahead.error());
        };

        let _or: Token![|] = input.parse()?;
        let context: Ident = match input.parse() {
            Ok(context) => context,
            Err(error) => return Err(Error::new(error.span(), "Expected context identifier.")),
        };
        let _or: Token![|] = input.parse()?;

        let handler: Handler = input.parse()?;

        Ok(if is_executes {
            HandlerBinding::Executes {
                node_ref,
                context,
                handler,
            }
        } else {
            HandlerBinding::Suggests {
                node_ref,
                context,
                handler,
            }
        })
    }
}
