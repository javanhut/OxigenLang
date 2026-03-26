use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Str,
    Float,
    Char,
    Bool,
    Array,
    Byte,
    Uint,
    Tuple,
    Map,
    Set,
    Generic,
    NoneType,
    ErrorType(Option<String>),
    ValueType,
    Union(Vec<TypeAnnotation>),
    Struct(String),
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
            "byte" => Some(TypeAnnotation::Byte),
            "uint" => Some(TypeAnnotation::Uint),
            "tuple" => Some(TypeAnnotation::Tuple),
            "map" => Some(TypeAnnotation::Map),
            "set" => Some(TypeAnnotation::Set),
            "generic" => Some(TypeAnnotation::Generic),
            "None" => Some(TypeAnnotation::NoneType),
            "Error" => Some(TypeAnnotation::ErrorType(None)),
            "Value" => Some(TypeAnnotation::ValueType),
            _ => None,
        }
    }

    pub fn from_str_or_struct(s: &str) -> TypeAnnotation {
        match Self::from_str(s) {
            Some(ta) => ta,
            None => TypeAnnotation::Struct(s.to_string()),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            TypeAnnotation::Int => "INTEGER".to_string(),
            TypeAnnotation::Str => "STRING".to_string(),
            TypeAnnotation::Float => "FLOAT".to_string(),
            TypeAnnotation::Char => "CHAR".to_string(),
            TypeAnnotation::Bool => "BOOLEAN".to_string(),
            TypeAnnotation::Array => "ARRAY".to_string(),
            TypeAnnotation::Byte => "BYTE".to_string(),
            TypeAnnotation::Uint => "UINT".to_string(),
            TypeAnnotation::Tuple => "TUPLE".to_string(),
            TypeAnnotation::Map => "MAP".to_string(),
            TypeAnnotation::Set => "SET".to_string(),
            TypeAnnotation::Generic => "GENERIC".to_string(),
            TypeAnnotation::NoneType => "NONE".to_string(),
            TypeAnnotation::ErrorType(tag) => match tag {
                Some(tag) => format!("ERROR<{}>", tag),
                None => "ERROR".to_string(),
            },
            TypeAnnotation::ValueType => "VALUE".to_string(),
            TypeAnnotation::Union(types) => {
                types.iter().map(|t| t.type_name()).collect::<Vec<_>>().join(" || ")
            }
            TypeAnnotation::Struct(name) => name.clone(),
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
    StructDef {
        token: Token,
        name: Identifier,
        parent: Option<Identifier>,
        fields: Vec<StructField>,
    },
    ContainsDef {
        token: Token,
        struct_name: Identifier,
        methods: Vec<(Identifier, Expression)>,
    },
    DotAssign {
        token: Token,
        object: Expression,
        field: Identifier,
        value: Expression,
    },
    Introduce {
        token: Token,
        path: ModulePath,
        selective: Option<Vec<Identifier>>,
    },
    Unpack {
        names: Vec<Identifier>,
        value: Expression,
    },
    IndexAssign {
        token: Token,
        object: Expression,
        index: Expression,
        value: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModulePath {
    pub segments: Vec<String>,
    pub is_relative: bool,
    pub parent_levels: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OptionArm {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChooseArm {
    pub pattern_name: String, // "ten", "eleven", or "else"
    pub inline_params: Option<Vec<Identifier>>,
    pub inline_condition: Option<Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Identifier,
    pub type_ann: TypeAnnotation,
    pub hidden: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedParam {
    pub ident: Identifier,
    pub type_ann: Option<TypeAnnotation>,
    pub default: Option<Expression>,
    pub optional: bool,
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
        named_args: Vec<(String, Expression)>,
    },

    Index {
        token: Token,
        left: Box<Expression>,
        index: Box<Expression>,
    },

    Grouped(Box<Expression>),

    FunctionLiteral {
        token: Token,
        parameters: Vec<TypedParam>,
        body: Vec<Statement>,
    },
    StructLiteral {
        token: Token,
        struct_name: String,
        field_values: Vec<(String, Expression)>,
    },
    DotAccess {
        token: Token,
        left: Box<Expression>,
        field: Identifier,
    },
    Slice {
        token: Token,
        left: Box<Expression>,
        start: Option<Box<Expression>>,
        end: Option<Box<Expression>>,
    },
    TupleLiteral {
        token: Token,
        elements: Vec<Expression>,
    },
    MapLiteral {
        token: Token,
        entries: Vec<(Expression, Expression)>,
    },
    Option {
        token: Token,
        arms: Vec<OptionArm>,
        default: Option<Vec<Statement>>,
        error_default: Option<Vec<Statement>>,
    },
    Guard {
        token: Token,
        value: Box<Expression>,
        binding: Identifier,
        error_tag: Option<String>,
        fallback: Box<Expression>,
    },
    Log {
        token: Token,
        value: Box<Expression>,
        binding: Identifier,
        error_tag: Option<String>,
        handler: Box<Expression>,
    },
    ErrorConstruct {
        token: Token,
        tag: Option<String>,
        value: Box<Expression>,
    },
    ValueConstruct {
        token: Token,
        value: Box<Expression>,
    },
    TypeWrap {
        token: Token,
        target: TypeAnnotation,
        value: Box<Expression>,
    },
    Fail {
        token: Token,
        value: Box<Expression>,
    },
    Unless {
        token: Token,
        consequence: Box<Expression>,
        condition: Box<Expression>,
        alternative: Box<Expression>,
    },
    StringInterp {
        token: Token,
        parts: Vec<StringInterpPart>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringInterpPart {
    Literal(String),
    Expr(Expression),
}
