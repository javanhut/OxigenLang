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
    MultilineString, // triple-quoted string literal (`"""..."""` / `'''...'''`)
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
    LShift,     // <<
    RShift,     // >>
    Caret,      // ^
    Pipe,       // |
    DoublePipe, // ||
    Tilde,      // ~

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
    Diverge,
    Converge,
    Within,
    When,
    Pattern,
    In,
    Not,
    Then,
    Guard,
    Fail,
    Struct,
    Enum,
    Includes,
    OptionKw,
    Unless,
    Hide,
    SelfKw,
    And,
    Or,
    Introduce,
    From,

    // String interpolation
    InterpStart,          // marks beginning of an interpolated string
    MultilineInterpStart, // beginning of an interpolated triple-quoted string
    InterpEnd,            // marks end of an interpolated string
    InterpExprStart,      // marks start of an interpolation expression {
    InterpExprEnd,        // marks end of an interpolation expression }
}

/// A single source position.
///
/// `line`/`column` are 1-based and count characters (what humans and the LSP
/// report); `offset` is a 0-based *byte* index into the original source, which
/// is what lets a renderer slice the text directly and what a machine-
/// applicable edit splices against.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Pos {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }
}

impl Default for Pos {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            offset: 0,
        }
    }
}

/// A half-open source range, `start..end`.
///
/// Spans used to be single points, which capped every diagnostic at one caret
/// and made secondary labels and machine-applicable edits impossible to
/// express — both need to name a region, not a character.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    /// A zero-width span at `line`/`column`, with no byte offset.
    ///
    /// For synthetic spans that were never tied to real source text (desugared
    /// AST nodes, tests). Prefer `range` wherever the producer knows both ends.
    pub fn new(line: usize, column: usize) -> Self {
        let p = Pos::new(line, column, 0);
        Self { start: p, end: p }
    }

    pub fn point(p: Pos) -> Self {
        Self { start: p, end: p }
    }

    pub fn range(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    /// Covers both spans: `self.start` to `other.end`. Used to build a
    /// composite span for an AST node from its leftmost and rightmost tokens.
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Line of the span's start — what a one-caret renderer reports.
    pub fn line(&self) -> usize {
        self.start.line
    }

    /// Column of the span's start.
    pub fn column(&self) -> usize {
        self.start.column
    }

    /// Width in bytes, or 0 when the span is a point.
    pub fn len_bytes(&self) -> usize {
        self.end.offset.saturating_sub(self.start.offset)
    }

    pub fn is_point(&self) -> bool {
        self.len_bytes() == 0
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::new(1, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub span: Span,
}

pub fn token_map() -> HashMap<&'static str, TokenType> {
    use TokenType::*;
    HashMap::from([
        ("option", OptionKw),
        ("unless", Unless),
        ("fun", Function),
        ("repeat", Repeat),
        ("each", Each),
        ("diverge", Diverge),
        ("converge", Converge),
        ("within", Within),
        ("when", When),
        ("choose", Choose),
        ("pattern", Pattern),
        ("in", In),
        ("not", Not),
        ("then", Then),
        ("guard", Guard),
        ("fail", Fail),
        ("True", True),
        ("False", False),
        ("give", Give),
        ("skip", Skip),
        ("stop", Stop),
        ("None", None),
        ("as", As),
        ("struct", Struct),
        ("enum", Enum),
        // `includes` is a CONTEXTUAL keyword: it only acts as a keyword in the
        // `StructName includes { ... }` position (detected in the parser). Lexing
        // it as a plain identifier lets it be used as a function/method name
        // (e.g. `fun includes(...)`, `arr.includes(x)`).
        ("hide", Hide),
        ("self", SelfKw),
        ("and", And),
        ("or", Or),
        ("introduce", Introduce),
        ("intro", Introduce),
        ("from", From),
    ])
}
