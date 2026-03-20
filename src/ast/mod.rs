use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Str,
    Float,
    Char,
    Bool,
    Array,
}

impl TypeAnnotation {
    pub fn from_str(s: &str) -> Option<TypeAnnotation> {
        match s {
            "int" => Some(TypeAnnotation::Int),
            "str" => Some(TypeAnnotation::Str),
            "float" => Some(TypeAnnotation::Float),
            "char" => Some(TypeAnnotation::Char),
            "bool" => Some(TypeAnnotation::Bool),
            "array" => Some(TypeAnnotation::Array),
            _ => None,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            TypeAnnotation::Int => "INTEGER",
            TypeAnnotation::Str => "STRING",
            TypeAnnotation::Float => "FLOAT",
            TypeAnnotation::Char => "CHAR",
            TypeAnnotation::Bool => "BOOLEAN",
            TypeAnnotation::Array => "ARRAY",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let { name: Identifier, value: Expression },
    Expr(Expression),
    Each {
        token: Token,
        variable: Identifier,
        iterable: Expression,
        body: Vec<Statement>,
    },
    Repeat {
        token: Token,
        condition: Expression,
        body: Vec<Statement>,
    },
    Pattern {
        token: Token,
        name: Identifier,
        params: Vec<Identifier>,
        condition: Expression,
    },
    Choose {
        token: Token,
        subject: Expression,
        arms: Vec<ChooseArm>,
    },
    If {
        token: Token,
        condition: Expression,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    },
    Skip,
    Stop,
    Give {
        token: Token,
        value: Expression,
    },
    TypedLet {
        name: Identifier,
        type_ann: TypeAnnotation,
        value: Expression,
        walrus: bool,
    },
    TypedDeclare {
        name: Identifier,
        type_ann: TypeAnnotation,
    },
    Assign {
        name: Identifier,
        value: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChooseArm {
    pub pattern_name: String, // "ten", "eleven", or "else"
    pub body: Expression,
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
    Char {
        token: Token,
        value: char,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    NoneExpr {
        token: Token,
    },
    Array {
        token: Token,
        elements: Vec<Expression>,
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

    Index {
        token: Token,
        left: Box<Expression>,
        index: Box<Expression>,
    },

    Grouped(Box<Expression>),

    FunctionLiteral {
        token: Token,
        parameters: Vec<Identifier>,
        body: Vec<Statement>,
    },
}
