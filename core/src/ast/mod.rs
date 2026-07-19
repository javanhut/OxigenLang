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
    EnumGeneric,
}

impl TypeAnnotation {
    #[allow(clippy::should_implement_trait)]
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
            "Enum" => Some(TypeAnnotation::EnumGeneric),
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
            TypeAnnotation::Union(types) => types
                .iter()
                .map(|t| t.type_name())
                .collect::<Vec<_>>()
                .join(" || "),
            TypeAnnotation::Struct(name) => name.clone(),
            TypeAnnotation::EnumGeneric => "ENUM".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: Identifier,
        value: Expression,
    },
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
    EnumDef {
        token: Token,
        name: Identifier,
        variants: Vec<EnumVariant>,
    },
    IncludesDef {
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
        values: Option<Vec<Expression>>,
        reassign: bool,
    },
    IndexAssign {
        token: Token,
        object: Expression,
        index: Expression,
        value: Expression,
    },
    Main {
        token: Token,
        body: Vec<Statement>,
    },
    Test {
        token: Token,
        name: Expression,
        body: Vec<Statement>,
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
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Identifier,
    pub type_ann: TypeAnnotation,
    pub hidden: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub kind: VariantKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantKind {
    Unit(Option<Expression>),
    Tuple(Vec<(Identifier, TypeAnnotation)>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumConstructKind {
    Tuple(Vec<Expression>),
    Struct(Vec<(String, Expression)>),
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
    EnumVariantConstruct {
        token: Token,
        enum_name: String,
        variant_name: String,
        kind: EnumConstructKind,
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
        tag: Option<String>,
        sub_tag: Option<String>,
        message: Option<Box<Expression>>,
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

    // Concurrency surface syntax. Kept as first-class nodes (rather than
    // desugared at parse time) so tooling — the formatter especially — can
    // round-trip `diverge`/`converge` instead of seeing the lowered builtins.
    // The compiler and evaluator lower them via `desugar_*` below; there is no
    // new runtime concept.
    Diverge {
        token: Token,
        body: Vec<Statement>,
    },
    DivergeEach {
        token: Token,
        variable: Identifier,
        iterable: Box<Expression>,
        body: Vec<Statement>,
    },
    Converge {
        token: Token,
        task: Box<Expression>,
        timeout: Option<Box<Expression>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringInterpPart {
    Literal(String),
    Expr(Expression),
}

// ── diverge / converge lowering ─────────────────────────────────────────────
// `diverge`/`converge` are surface syntax for fork-join. They lower to the
// `__spawn`/`__join_task` builtins — the compiler and VM never need a new
// concept. The lowering lives here (not in the parser) so the parser can keep
// the high-level node for the formatter while the compiler and evaluator share
// one desugaring:
//   diverge { B }              -> __spawn(fun() { B })
//   diverge each X in XS { B } -> (fun(){ spawn each, then join each in order })()
//   converge T                 -> __join_task(T)
//   converge T within ms       -> __join_task(T, ms)

fn syn_ident(tok: &Token, name: &str) -> Expression {
    Expression::Ident(Identifier {
        token: tok.clone(),
        value: name.to_string(),
    })
}

fn syn_call(tok: &Token, name: &str, args: Vec<Expression>) -> Expression {
    Expression::Call {
        token: tok.clone(),
        function: Box::new(syn_ident(tok, name)),
        args,
        named_args: Vec::new(),
    }
}

fn syn_thunk(tok: &Token, body: Vec<Statement>) -> Expression {
    Expression::FunctionLiteral {
        token: tok.clone(),
        parameters: Vec::new(),
        body,
    }
}

pub fn desugar_diverge(tok: &Token, body: &[Statement]) -> Expression {
    syn_call(tok, "__spawn", vec![syn_thunk(tok, body.to_vec())])
}

pub fn desugar_diverge_each(
    tok: &Token,
    variable: &Identifier,
    iterable: &Expression,
    body: &[Statement],
) -> Expression {
    let empty_arr = || Expression::Array {
        token: tok.clone(),
        elements: Vec::new(),
    };
    let id = |n: &str| Identifier {
        token: tok.clone(),
        value: n.to_string(),
    };

    // each X in XS { push(__dv_hs, __spawn(fun(){ B })) }
    let spawn_call = syn_call(tok, "__spawn", vec![syn_thunk(tok, body.to_vec())]);
    let spawn_loop = Statement::Each {
        token: tok.clone(),
        variable: variable.clone(),
        iterable: iterable.clone(),
        body: vec![Statement::Expr(syn_call(
            tok,
            "push",
            vec![syn_ident(tok, "__dv_hs"), spawn_call],
        ))],
    };

    // each __dv_h in __dv_hs { push(__dv_rs, __join_task(__dv_h)) }
    let join_call = syn_call(tok, "__join_task", vec![syn_ident(tok, "__dv_h")]);
    let join_loop = Statement::Each {
        token: tok.clone(),
        variable: id("__dv_h"),
        iterable: syn_ident(tok, "__dv_hs"),
        body: vec![Statement::Expr(syn_call(
            tok,
            "push",
            vec![syn_ident(tok, "__dv_rs"), join_call],
        ))],
    };

    // (fun() { spawn all, then join all in order; return results })()
    let lowered_body = vec![
        Statement::Let {
            name: id("__dv_hs"),
            value: empty_arr(),
        },
        spawn_loop,
        Statement::Let {
            name: id("__dv_rs"),
            value: empty_arr(),
        },
        join_loop,
        Statement::Expr(syn_ident(tok, "__dv_rs")),
    ];
    Expression::Call {
        token: tok.clone(),
        function: Box::new(syn_thunk(tok, lowered_body)),
        args: Vec::new(),
        named_args: Vec::new(),
    }
}

pub fn desugar_converge(
    tok: &Token,
    task: &Expression,
    timeout: Option<&Expression>,
) -> Expression {
    let mut args = vec![task.clone()];
    if let Some(ms) = timeout {
        args.push(ms.clone());
    }
    syn_call(tok, "__join_task", args)
}
