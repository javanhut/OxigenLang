use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let { name: Identifier, value: Expression },
    Expr(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(Identifier),
    Int {
        token: Token,
        value: i64,
    },
    Str {
        token: Token,
        value: String,
    },
    Float {
        token: Token,
        value: f64,
    },

    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },

    Infix {
        token: Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },

    Postfix {
        token: Token,
        left: Box<Expression>,
        operator: String,
    },

    Call {
        token: Token,
        function: Box<Expression>,
        args: Vec<Expression>,
    },

    Grouped(Box<Expression>),
}
