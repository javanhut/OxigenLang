use std::collections::HashMap;



#[derive(Debug, Clone, PartialEq, Eq)]
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
    Boolean,

    // Indentation Handling 
    Indent, 
    Dedent,

    //Operators
    Assign, // =
    Walrus, // :=
    Plus, // +
    Minus, // -
    Astrisk, // *
    Ampersand, // &
    At, // @
    Mod, // %
    Eq, // ==
    NotEq, // != 
    Shebang, // !
    DollarSign, // $
    Hash, // #
    Increment, // ++
    Decrement,  // --
    Gt, // >
    Lt, // <
    Gteq, // >=
    Lteq, // <= 
    Arrow, // ->
    // Delimiters
    Comma, // ,
    Colon, // :
    Semicolon, // ;
    LBrace, // [
    RBrace, // ]
    LBracket, // {
    RBracket, // }
    LParen, // (
    RParen, // )
    FullStop, // .
    FSlash, // /
    BSlash, // \
    

    // Bitwise Operators
    LShift, // <<
    RShift, // >>
    Xor, // ^
    Pipe, // |
    Tilde, // ~
    
    // Keywords
    If,
    Else,
    Function,
    True,
    False,
    None,
    Skip,
    Stop,
    Choose,
    Case,
    As,
    Super,
    Repeat,
    Each,
    When,
    Pattern,
}


#[derive(Debug,Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}


pub fn token_map() -> HashMap<&'static str, TokenType> {
    use TokenType::*;
    HashMap::from([
        ("if", If),
        ("else", Else),
        ("fun", Function),
        ("repeat", Repeat),
        ("each", Each),
        ("when", When),
        ("choose", Choose),
        ("pattern", Pattern),
    ])
}
