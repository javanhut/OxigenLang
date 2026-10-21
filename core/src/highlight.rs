//! Syntax highlighting driven by the real lexer, for the website, the browser
//! playground and any host that shows Oxigen source.
//!
//! Classes come from the token stream, so highlighting agrees with what the
//! language accepts. The lexer does not emit comments, so the text between
//! tokens is scanned for them here.

use crate::lexer::Lexer;
use crate::token::TokenType;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Class {
    Keyword,
    /// `True`, `False`, `None`, `self`.
    Constant,
    String,
    Number,
    Comment,
    /// An identifier being called.
    Function,
    /// A type annotation (`<int>`) or a capitalised name (`Person`).
    Type,
    /// A shebang or `#[...]` directive.
    Directive,
}

impl Class {
    /// The CSS class `to_html` gives this class.
    pub fn css(self) -> &'static str {
        match self {
            Class::Keyword => "tk-kw",
            Class::Constant => "tk-const",
            Class::String => "tk-str",
            Class::Number => "tk-num",
            Class::Comment => "tk-com",
            Class::Function => "tk-fn",
            Class::Type => "tk-type",
            Class::Directive => "tk-dir",
        }
    }
}

/// The classified byte ranges of `source`, in order and not overlapping.
/// Text in no range (whitespace, operators, plain names) is unstyled.
pub fn highlight(source: &str) -> Vec<(Range<usize>, Class)> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token.token_type == TokenType::Eof {
            break;
        }
        let (start, end) = (token.span.start.offset, token.span.end.offset);
        tokens.push((token.token_type, start..end.min(source.len())));
    }

    let mut out: Vec<(Range<usize>, Class)> = Vec::new();
    let mut push = |range: Range<usize>, class: Class| {
        if range.is_empty() {
            return;
        }
        match out.last_mut() {
            Some((last, c)) if *c == class && last.end == range.start => last.end = range.end,
            _ => out.push((range, class)),
        }
    };

    let mut cursor = 0;
    let mut i = 0;
    while i < tokens.len() {
        let (kind, range) = &tokens[i];
        // Zero-width tokens (newlines, indent mode's braces, the pieces of an
        // interpolated string) have no text of their own to colour.
        if range.start < cursor || range.is_empty() {
            i += 1;
            continue;
        }
        gap(source, cursor..range.start, &mut push);
        cursor = range.end;
        if matches!(
            kind,
            TokenType::InterpStart | TokenType::MultilineInterpStart
        ) {
            i = interpolated(&tokens, i, &mut push);
            continue;
        }
        if let Some(class) = classify(source, &tokens, i) {
            push(range.clone(), class);
        }
        i += 1;
    }
    gap(source, cursor..source.len(), &mut push);
    out
}

/// An interpolated string, whose token spans the whole literal while the
/// pieces inside are zero-width markers at their start. The literal is string
/// except its `{...}` holes, which are left unstyled. Returns the index after
/// the string's last token.
fn interpolated(
    tokens: &[(TokenType, Range<usize>)],
    start: usize,
    push: &mut impl FnMut(Range<usize>, Class),
) -> usize {
    let whole = tokens[start].1.clone();
    let (mut strings, mut exprs) = (0, 0);
    let mut piece = whole.start;
    let mut i = start;
    while i < tokens.len() {
        let (kind, range) = &tokens[i];
        i += 1;
        match kind {
            TokenType::InterpStart | TokenType::MultilineInterpStart => strings += 1,
            TokenType::InterpEnd => {
                strings -= 1;
                if strings == 0 {
                    break;
                }
            }
            TokenType::InterpExprStart => {
                exprs += 1;
                if strings == 1 && exprs == 1 {
                    push(piece..range.start.clamp(piece, whole.end), Class::String);
                }
            }
            TokenType::InterpExprEnd => {
                exprs -= 1;
                if strings == 1 && exprs == 0 {
                    piece = (range.start + 1).clamp(piece, whole.end);
                }
            }
            _ => {}
        }
    }
    push(piece..whole.end, Class::String);
    i
}

fn classify(source: &str, tokens: &[(TokenType, Range<usize>)], i: usize) -> Option<Class> {
    use TokenType as T;
    let (kind, range) = &tokens[i];
    let next = tokens.get(i + 1).map(|t| &t.0);
    let prev = i.checked_sub(1).map(|p| &tokens[p].0);
    Some(match kind {
        T::True | T::False | T::None | T::SelfKw => Class::Constant,
        T::If
        | T::Function
        | T::Give
        | T::Skip
        | T::Stop
        | T::Choose
        | T::As
        | T::Repeat
        | T::Each
        | T::Diverge
        | T::Converge
        | T::Within
        | T::When
        | T::Pattern
        | T::In
        | T::Not
        | T::Then
        | T::Guard
        | T::Fail
        | T::Struct
        | T::Enum
        | T::Includes
        | T::OptionKw
        | T::Unless
        | T::Hide
        | T::And
        | T::Or
        | T::Introduce
        | T::From => Class::Keyword,
        T::String | T::MultilineString | T::Char => Class::String,
        T::Integer | T::Float => Class::Number,
        T::Ident if source[range.clone()].starts_with(|c: char| c.is_ascii_uppercase()) => {
            Class::Type
        }
        T::Ident if next == Some(&T::LParen) => Class::Function,
        T::Ident
            if prev == Some(&T::Lt)
                && matches!(
                    next,
                    Some(T::Gt | T::Lt | T::RShift | T::DoublePipe | T::Pipe | T::Comma)
                ) =>
        {
            Class::Type
        }
        _ => return None,
    })
}

/// Classify text the lexer skipped: whitespace, comments and directives.
fn gap(source: &str, range: Range<usize>, push: &mut impl FnMut(Range<usize>, Class)) {
    let text = &source[range.clone()];
    let mut at = 0;
    while at < text.len() {
        let rest = &text[at..];
        let start = range.start + at;
        if rest.starts_with("//") || (rest.starts_with("#!") && start == 0) {
            let len = rest.find('\n').unwrap_or(rest.len());
            let class = if rest.starts_with("//") {
                Class::Comment
            } else {
                Class::Directive
            };
            push(start..start + len, class);
            at += len;
        } else if rest.starts_with("/*") {
            let len = rest[2..].find("*/").map_or(rest.len(), |e| e + 4);
            push(start..start + len, Class::Comment);
            at += len;
        } else if rest.starts_with("#[") {
            let len = rest.find(']').map_or(rest.len(), |e| e + 1);
            push(start..start + len, Class::Directive);
            at += len;
        } else {
            at += rest.chars().next().map_or(1, char::len_utf8);
        }
    }
}

/// `source` as HTML, each classified range in a `<span>` with `Class::css`.
pub fn to_html(source: &str) -> String {
    let mut html = String::with_capacity(source.len() * 2);
    let mut cursor = 0;
    for (range, class) in highlight(source) {
        escape_into(&mut html, &source[cursor..range.start]);
        html.push_str("<span class=\"");
        html.push_str(class.css());
        html.push_str("\">");
        escape_into(&mut html, &source[range.clone()]);
        html.push_str("</span>");
        cursor = range.end;
    }
    escape_into(&mut html, &source[cursor..]);
    html
}

fn escape_into(html: &mut String, text: &str) {
    for c in text.chars() {
        match c {
            '&' => html.push_str("&amp;"),
            '<' => html.push_str("&lt;"),
            '>' => html.push_str("&gt;"),
            '"' => html.push_str("&quot;"),
            _ => html.push(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn classes(source: &str) -> Vec<(&str, Class)> {
        highlight(source)
            .into_iter()
            .map(|(r, c)| (&source[r], c))
            .collect()
    }

    #[test]
    fn classifies_tokens_comments_and_directives() {
        let src = "#[indent]\n// hi\nfun greet(name <str>) /* b */ give 'c'\nPerson(True, 2.5)\n";
        assert_eq!(
            classes(src),
            vec![
                ("#[indent]", Class::Directive),
                ("// hi", Class::Comment),
                ("fun", Class::Keyword),
                ("greet", Class::Function),
                ("str", Class::Type),
                ("/* b */", Class::Comment),
                ("give", Class::Keyword),
                ("'c'", Class::String),
                ("Person", Class::Type),
                ("True", Class::Constant),
                ("2.5", Class::Number),
            ]
        );
    }

    #[test]
    fn interpolation_holes_are_unstyled() {
        let src = "x := \"Hi {name}, {1 + 2}!\"\ny := \"\"\"m\n{name}\"\"\"\n";
        assert_eq!(
            classes(src),
            vec![
                ("\"Hi ", Class::String),
                (", ", Class::String),
                ("!\"", Class::String),
                ("\"\"\"m\n", Class::String),
                ("\"\"\"", Class::String),
            ]
        );
    }

    #[test]
    fn html_escapes_and_keeps_every_byte() {
        let src = "a < b // <&>\n\"unterminated";
        let html = to_html(src);
        assert!(html.contains("<span class=\"tk-com\">// &lt;&amp;&gt;</span>"));
        let text = html
            .split('<')
            .map(|part| part.split_once('>').map_or(part, |(_, t)| t))
            .collect::<String>();
        assert_eq!(
            text.replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&amp;", "&")
                .replace("&quot;", "\""),
            src
        );
    }
}
