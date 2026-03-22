use std::collections::HashMap;

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Eof,
    Illegal,
    Newline,
    Ident,

    // Primative Types
    Integer,
    String,
    Float,
    Char,

    //Operators
    Assign,     // =
    Walrus,     // :=
    Plus,       // +
    Minus,      // -
    Asterisk,   // *
    Ampersand,  // &
    At,         // @
    Mod,        // %
    Eq,         // ==
    NotEq,      // !=
    Shebang,    // !
    DollarSign, // $
    Hash,       // #
    Increment,  // ++
    Decrement,  // --
    Gt,         // >
    Lt,         // <
    Gte,        // >=
    Lte,        // <=
    Arrow,      // ->
    // Delimiters
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    LParen,    // (
    RParen,    // )
    FullStop,  // .
    FSlash,    // /
    BSlash,    // \

    // Bitwise Operators
    LShift, // <<
    RShift, // >>
    Caret,  // ^
    Pipe,       // |
    DoublePipe, // ||
    Tilde,  // ~

    // Keywords
    If,
    Function,
    True,
    False,
    None,
    Give,
    Skip,
    Stop,
    Choose,
    As,
    Repeat,
    Each,
    When,
    Pattern,
    In,
    Not,
    Struct,
    Contains,
    OptionKw,
    Unless,
    Hide,
    SelfKw,
    And,
    Or,

    // String interpolation
    InterpStart,  // marks beginning of an interpolated string
    InterpEnd,    // marks end of an interpolated string
    InterpExprStart, // marks start of an interpolation expression {
    InterpExprEnd,   // marks end of an interpolation expression }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn token_map() -> HashMap<&'static str, TokenType> {
    use TokenType::*;
    HashMap::from([
        ("option", OptionKw),
        ("unless", Unless),
        ("fun", Function),
        ("repeat", Repeat),
        ("each", Each),
        ("when", When),
        ("choose", Choose),
        ("pattern", Pattern),
        ("in", In),
        ("not", Not),
        ("True", True),
        ("False", False),
        ("give", Give),
        ("skip", Skip),
        ("stop", Stop),
        ("None", None),
        ("as", As),
        ("struct", Struct),
        ("contains", Contains),
        ("hide", Hide),
        ("self", SelfKw),
        ("and", And),
        ("or", Or),
    ])
}
