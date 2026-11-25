use std::collections::HashMap;

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Eof,
    Illegal,
    Newline,
    Ident,

    Integer,
    String,
    MultilineString,
    Float,
    Char,

    Assign,
    Walrus,
    Plus,
    Minus,
    Asterisk,
    Ampersand,
    At,
    Mod,
    Eq,
    NotEq,
    Shebang,
    DollarSign,
    Hash,
    Increment,
    Decrement,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    Gt,
    Lt,
    Gte,
    Lte,
    Arrow,
    Comma,
    Colon,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    FullStop,
    FSlash,
    BSlash,

    LShift,
    RShift,
    Caret,
    Pipe,
    DoublePipe,
    Tilde,

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

    InterpStart,
    MultilineInterpStart,
    InterpEnd,
    InterpExprStart,
    InterpExprEnd,
}

/// A single source position.
///
/// `line`/`column` are 1-based and count characters; `offset` is a 0-based byte index into the original source.
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    /// A zero-width span at `line`/`column`, with no byte offset.
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

    /// Covers both spans: `self.start` to `other.end`.
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Line of the span's start.
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
        ("hide", Hide),
        ("self", SelfKw),
        ("and", And),
        ("or", Or),
        ("introduce", Introduce),
        ("intro", Introduce),
        ("from", From),
    ])
}
