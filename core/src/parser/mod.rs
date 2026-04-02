use crate::ast::{
    ChooseArm, Expression, Identifier, ModulePath, OptionArm, Program, Statement, StringInterpPart,
    TypeAnnotation, TypedParam,
};
use crate::lexer::Lexer;
use crate::token::{Span, Token, TokenType};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Conditional, // unless ... then ...
    Recovery,    // guard err -> ...
    LogicalOr,   // or
    LogicalAnd,  // and
    Equals,      // == !=
    LessGreater, // <,>,<=,>=
    Sum,         // + -
    Product,     // *, / , %
    Prefix,      // -x, !x
    Postfix,     // x--, x++
    Call,        // f(x)
    Index,       // array[index]
}

fn precedence_of(tt: &TokenType) -> Precedence {
    use Precedence::*;
    use TokenType::*;

    match tt {
        Unless => Conditional,
        Guard => Recovery,
        Or => LogicalOr,
        And => LogicalAnd,
        Eq | NotEq => Equals,
        Lt | Lte | Gt | Gte => LessGreater,
        Plus | Minus => Sum,
        Asterisk | FSlash | Mod => Product,
        Increment | Decrement => Postfix,
        LParen => Call,
        LBracket => Index,
        FullStop => Index,
        _ => Lowest,
    }
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Severity {
    Error,
    Warning,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
    pub suggestion: Option<String>,
    pub severity: Severity,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} [{}:{}]",
            self.severity, self.message, self.span.line, self.span.column
        )?;
        if let Some(ref hint) = self.suggestion {
            write!(f, "\n  hint: {}", hint)?;
        }
        Ok(())
    }
}

impl Diagnostic {
    fn error(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            suggestion: None,
            severity: Severity::Error,
        }
    }

    fn error_with_hint(
        span: Span,
        message: impl Into<String>,
        suggestion: impl Into<String>,
    ) -> Self {
        Self {
            span,
            message: message.into(),
            suggestion: Some(suggestion.into()),
            severity: Severity::Error,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<Diagnostic>,
    source: String,
    prefix_fns: HashMap<TokenType, PrefixParseFn>,
    infix_fns: HashMap<TokenType, InfixParseFn>,
    lookahead_buffer: VecDeque<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer, source: &str) -> Self {
        let dummy = Token {
            token_type: TokenType::Illegal,
            literal: "".into(),
            span: Span::default(),
        };

        let mut p = Parser {
            lexer,
            curr_token: dummy.clone(),
            peek_token: dummy,
            errors: Vec::new(),
            source: source.to_string(),
            prefix_fns: HashMap::new(),
            infix_fns: HashMap::new(),
            lookahead_buffer: VecDeque::new(),
        };

        // Prefix Parse Functions
        p.register_prefix(TokenType::Ident, Parser::parse_identifier);
        p.register_prefix(TokenType::Integer, Parser::parse_integer);
        p.register_prefix(TokenType::Float, Parser::parse_float);
        p.register_prefix(TokenType::String, Parser::parse_string);
        p.register_prefix(TokenType::Char, Parser::parse_char);
        p.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::True, Parser::parse_boolean);
        p.register_prefix(TokenType::False, Parser::parse_boolean);
        p.register_prefix(TokenType::None, Parser::parse_none);
        p.register_prefix(TokenType::LBracket, Parser::parse_array);
        p.register_prefix(TokenType::Lt, Parser::parse_angle_effect_expression);
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Shebang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Not, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Fail, Parser::parse_fail_expression);
        p.register_prefix(TokenType::Function, Parser::parse_function_expression);
        p.register_prefix(TokenType::LBrace, Parser::parse_map_literal);
        p.register_prefix(TokenType::OptionKw, Parser::parse_option_expression);
        p.register_prefix(TokenType::SelfKw, Parser::parse_self_expression);
        p.register_prefix(TokenType::InterpStart, Parser::parse_string_interp);

        // Infix Parse Functions
        for tt in [TokenType::Plus, TokenType::Minus] {
            p.register_infix(tt, Parser::parse_infix_expression);
        }

        // Logical operators
        p.register_infix(TokenType::And, Parser::parse_infix_expression);
        p.register_infix(TokenType::Or, Parser::parse_infix_expression);
        p.register_infix(TokenType::Unless, Parser::parse_unless_expression);
        p.register_infix(TokenType::Guard, Parser::parse_guard_expression);

        // Comparison operators
        p.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        p.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        p.register_infix(TokenType::Lt, Parser::parse_lt_expression);
        p.register_infix(TokenType::Lte, Parser::parse_infix_expression);
        p.register_infix(TokenType::Gt, Parser::parse_infix_expression);
        p.register_infix(TokenType::Gte, Parser::parse_infix_expression);

        // Arithmetic
        p.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        p.register_infix(TokenType::FSlash, Parser::parse_infix_expression);
        p.register_infix(TokenType::Mod, Parser::parse_infix_expression);

        // Postfix
        p.register_infix(TokenType::Increment, Parser::parse_postfix_expression);
        p.register_infix(TokenType::Decrement, Parser::parse_postfix_expression);

        // Call
        p.register_infix(TokenType::LParen, Parser::parse_call_expression);

        // Index
        p.register_infix(TokenType::LBracket, Parser::parse_index_expression);

        // Dot access
        p.register_infix(TokenType::FullStop, Parser::parse_dot_expression);

        p.next_token();
        p.next_token();
        p
    }

    pub fn errors(&self) -> &[Diagnostic] {
        &self.errors
    }

    pub fn format_errors(&self) -> String {
        let displayed: Vec<_> = self
            .errors
            .iter()
            .map(|d| self.format_diagnostic(d))
            .collect();
        if displayed.len() > 1 {
            let mut out = displayed[0].clone();
            // Show up to 3 additional errors, skip noise
            let rest: Vec<_> = displayed[1..].iter().take(2).collect();
            for d in rest {
                out.push_str("\n\n");
                out.push_str(d);
            }
            if displayed.len() > 3 {
                out.push_str(&format!(
                    "\n\n... and {} more error(s)",
                    displayed.len() - 3
                ));
            }
            out
        } else {
            displayed.join("\n\n")
        }
    }

    fn format_diagnostic(&self, diag: &Diagnostic) -> String {
        let mut out = String::new();
        let line_num = diag.span.line;
        let col = diag.span.column;

        // Header
        out.push_str(&format!("{}: {}\n", diag.severity, diag.message));
        out.push_str(&format!("  --> line {}:{}\n", line_num, col));

        // Source context
        if let Some(source_line) = self.source.lines().nth(line_num.saturating_sub(1)) {
            let line_str = format!("{}", line_num);
            let padding = " ".repeat(line_str.len());
            out.push_str(&format!("{} |\n", padding));
            out.push_str(&format!("{} | {}\n", line_str, source_line));
            if col > 0 {
                let caret_padding = " ".repeat(col.saturating_sub(1));
                out.push_str(&format!("{} | {}^", padding, caret_padding));
            }
        }

        // Suggestion
        if let Some(ref hint) = diag.suggestion {
            out.push_str(&format!("\n  = hint: {}", hint));
        }

        out
    }

    fn negate_expression(expr: Expression) -> Expression {
        Expression::Prefix {
            token: Token {
                token_type: TokenType::Not,
                literal: "not".to_string(),
                span: Span::default(),
            },
            operator: "not".to_string(),
            right: Box::new(expr),
        }
    }

    fn is_type_name_token(tt: &TokenType) -> bool {
        matches!(tt, TokenType::Ident | TokenType::None)
    }

    fn parse_type_name_from_current(&mut self) -> Option<TypeAnnotation> {
        if self.curr_token.token_type == TokenType::Ident && self.curr_token.literal == "Error" {
            if self.peek_token.token_type == TokenType::Lt {
                self.next_token(); // consume inner '<'
                if self.peek_token.token_type != TokenType::Ident {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.peek_token.span,
                        format!("expected error tag name, got {:?}", self.peek_token.literal),
                        "provide a tag name like Error<MyTag>",
                    ));
                    return None;
                }
                self.next_token(); // move to tag
                let tag = self.curr_token.literal.clone();
                if self.peek_token.token_type == TokenType::RShift {
                    self.next_token(); // consume both closing '>' tokens
                    return Some(TypeAnnotation::ErrorType(Some(tag)));
                }
                self.expect_peek(TokenType::Gt)?; // close inner '>'
                return Some(TypeAnnotation::ErrorType(Some(tag)));
            }
            return Some(TypeAnnotation::ErrorType(None));
        }

        if self.curr_token.token_type == TokenType::Ident && self.curr_token.literal == "Value" {
            return Some(TypeAnnotation::ValueType);
        }

        Some(TypeAnnotation::from_str_or_struct(&self.curr_token.literal))
    }

    fn parse_type_union_member(&mut self) -> Option<TypeAnnotation> {
        if !Self::is_type_name_token(&self.curr_token.token_type) {
            self.errors.push(Diagnostic::error_with_hint(
                self.curr_token.span,
                format!("expected type name, got {:?}", self.curr_token.literal),
                "valid types: int, str, float, char, bool, array, byte, uint, tuple, map, set",
            ));
            return None;
        }
        self.parse_type_name_from_current()
    }

    /// Parses a type annotation after `<` has already been consumed.
    /// Supports both `<A || B>` and legacy `<A> || <B>` unions.
    fn parse_type_annotation(&mut self) -> Option<TypeAnnotation> {
        if !Self::is_type_name_token(&self.peek_token.token_type) {
            self.errors.push(Diagnostic::error_with_hint(
                self.peek_token.span,
                format!(
                    "expected type name after `<`, got {:?}",
                    self.peek_token.literal
                ),
                "valid types: int, str, float, char, bool, array, byte, uint, tuple, map, set",
            ));
            return None;
        }
        self.next_token();
        let mut types = vec![self.parse_type_union_member()?];

        loop {
            let peek_type = self.peek_token.token_type.clone();
            let next_type = self.peek_nth(1).token_type.clone();
            match peek_type {
                TokenType::Gt if next_type == TokenType::DoublePipe => {
                    self.next_token(); // consume legacy member-closing '>'
                }
                TokenType::DoublePipe => {
                    self.next_token(); // consume '||'
                    if self.peek_token.token_type == TokenType::Lt {
                        self.next_token(); // consume legacy '<'
                    }
                    if !Self::is_type_name_token(&self.peek_token.token_type) {
                        self.errors.push(Diagnostic::error_with_hint(
                            self.peek_token.span,
                            format!(
                                "expected type name in union, got {:?}",
                                self.peek_token.literal
                            ),
                            "use `<TypeA || TypeB>` for union types",
                        ));
                        return None;
                    }
                    self.next_token();
                    types.push(self.parse_type_union_member()?);
                }
                TokenType::Gt | TokenType::RShift => {
                    self.next_token(); // consume closing '>' or '>>'
                    break;
                }
                _ => {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.peek_token.span,
                        format!(
                            "expected `>` or `||` in type annotation, got {:?}",
                            self.peek_token.literal
                        ),
                        "close the type annotation with `>` or add `||` for a union type",
                    ));
                    return None;
                }
            }
        }

        if types.len() == 1 {
            Some(types.remove(0))
        } else {
            Some(TypeAnnotation::Union(types))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.curr_token.token_type != TokenType::Eof {
            if self.curr_token.token_type == TokenType::Newline {
                self.next_token();
                continue;
            }

            let err_count = self.errors.len();
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            } else if self.errors.len() > err_count {
                // A new error was added — synchronize to recover and continue parsing
                self.synchronize();
                continue;
            }
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        let stmt = self.parse_statement_inner()?;
        self.try_postfix_guard(stmt)
    }

    fn parse_statement_inner(&mut self) -> Option<Statement> {
        if self.curr_token.token_type == TokenType::Ident {
            // Check for unpacking: x, y := expr
            if self.peek_token.token_type == TokenType::Comma && self.is_unpack_statement() {
                return self.parse_unpack_statement();
            }
            if self.peek_token.token_type == TokenType::Walrus {
                return self.parse_let_statement();
            }
            if self.peek_token.token_type == TokenType::Lt && self.is_typed_declaration() {
                return self.parse_typed_let_statement();
            }
            if self.peek_token.token_type == TokenType::Lt && self.is_typed_declare_shorthand() {
                return self.parse_typed_declare_shorthand();
            }
            if self.peek_token.token_type == TokenType::As && self.is_as_typed_declare() {
                return self.parse_typed_declare_statement();
            }
            if self.peek_token.token_type == TokenType::Assign {
                return self.parse_assign_statement();
            }
            if self.peek_token.token_type == TokenType::Contains {
                return self.parse_contains_statement();
            }
        }
        if self.curr_token.token_type == TokenType::Ident
            && self.curr_token.literal == "main"
            && self.peek_token.token_type == TokenType::LBrace
        {
            return self.parse_main_block();
        }

        match self.curr_token.token_type {
            TokenType::Struct => self.parse_struct_definition(),
            TokenType::Each => self.parse_each_statement(),
            TokenType::Repeat => self.parse_repeat_statement(),
            TokenType::Pattern => self.parse_pattern_statement(),
            TokenType::Choose => self.parse_choose_statement(),
            TokenType::Unless => self.parse_unless_statement(),
            TokenType::OptionKw => self.parse_expression_statement(),
            TokenType::Skip => Some(Statement::Skip),
            TokenType::Stop => Some(Statement::Stop),
            TokenType::Function => {
                if self.peek_token.token_type == TokenType::Ident {
                    self.parse_named_function_statement()
                } else {
                    self.parse_expression_statement()
                }
            }
            TokenType::Give => self.parse_give_statement(),
            TokenType::Introduce => self.parse_introduce_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume ':='
        self.expect_peek(TokenType::Walrus)?;

        // move to expression start
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Let { name, value })
    }

    /// Lookahead to check if this is an unpack: ident, ident, ... :=
    fn is_unpack_statement(&mut self) -> bool {
        let mut idx = 0; // peek_nth(0) is peek_token
        loop {
            let tt = self.peek_nth(idx).token_type.clone();
            match tt {
                TokenType::Comma => {
                    // Next should be an ident
                    let next = self.peek_nth(idx + 1).token_type.clone();
                    if next != TokenType::Ident {
                        return false;
                    }
                    idx += 2; // skip comma + ident
                }
                TokenType::Walrus => return true,
                _ => return false,
            }
        }
    }

    fn parse_unpack_statement(&mut self) -> Option<Statement> {
        let mut names = Vec::new();

        // First identifier (curr_token)
        names.push(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        });

        // Parse remaining: , ident, ident, ...
        while self.peek_token.token_type == TokenType::Comma {
            self.next_token(); // consume ','
            self.next_token(); // move to next ident
            if self.curr_token.token_type != TokenType::Ident {
                self.errors.push(Diagnostic::error_with_hint(
                    self.curr_token.span,
                    format!(
                        "expected identifier in unpack, got {:?}",
                        self.curr_token.literal
                    ),
                    "unpack syntax: x, y := [1, 2]",
                ));
                return None;
            }
            names.push(Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            });
        }

        // consume ':='
        self.expect_peek(TokenType::Walrus)?;

        // move to expression
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Unpack { names, value })
    }

    fn parse_typed_let_statement(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume '<type>' (and any || continuations)
        self.expect_peek(TokenType::Lt)?;
        let type_ann = self.parse_type_annotation()?;

        // determine walrus or strict assign
        let walrus = self.peek_token.token_type == TokenType::Walrus;
        if walrus {
            self.expect_peek(TokenType::Walrus)?;
        } else {
            self.expect_peek(TokenType::Assign)?;
        }

        // move to expression start
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::TypedLet {
            name,
            type_ann,
            value,
            walrus,
        })
    }

    fn parse_typed_declare_statement(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume 'as'
        self.expect_peek(TokenType::As)?;

        // consume '<type>' (and any || continuations)
        self.expect_peek(TokenType::Lt)?;
        let type_ann = self.parse_type_annotation()?;

        Some(Statement::TypedDeclare { name, type_ann })
    }

    /// Parses `x <Type>` — shorthand for `x as <Type>`
    fn parse_typed_declare_shorthand(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume '<type>' (and any || continuations)
        self.expect_peek(TokenType::Lt)?;
        let type_ann = self.parse_type_annotation()?;

        Some(Statement::TypedDeclare { name, type_ann })
    }

    fn parse_assign_statement(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume '='
        self.expect_peek(TokenType::Assign)?;

        // move to expression start
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Assign { name, value })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Conditional)?;

        // Check for dot-assign: expr.field = value
        if let Expression::DotAccess { token, left, field } = &expr {
            if self.peek_token.token_type == TokenType::Assign {
                let tok = token.clone();
                let obj = *left.clone();
                let fld = field.clone();
                self.next_token(); // consume '='
                self.next_token(); // move to value expression
                let value = self.parse_expression(Precedence::Lowest)?;
                return Some(Statement::DotAssign {
                    token: tok,
                    object: obj,
                    field: fld,
                    value,
                });
            }
        }

        // Check for index-assign: expr[key] = value  or  expr[key] := value
        if let Expression::Index { token, left, index } = &expr {
            if self.peek_token.token_type == TokenType::Assign
                || self.peek_token.token_type == TokenType::Walrus
            {
                let tok = token.clone();
                let obj = *left.clone();
                let idx = *index.clone();
                self.next_token(); // consume '=' or ':='
                self.next_token(); // move to value expression
                let value = self.parse_expression(Precedence::Lowest)?;
                return Some(Statement::IndexAssign {
                    token: tok,
                    object: obj,
                    index: idx,
                    value,
                });
            }
        }

        Some(Statement::Expr(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_fns.get(&self.curr_token.token_type).copied();

        let mut left = match prefix {
            Some(f) => f(self)?,
            None => {
                self.errors.push(Diagnostic::error(
                    self.curr_token.span,
                    format!("unexpected token {:?}", self.curr_token.literal),
                ));
                return None;
            }
        };

        // Pratt loop: while next operator binds tighter, consume it
        while self.peek_token.token_type != TokenType::Eof
            && self.peek_token.token_type != TokenType::Newline
        {
            if self.peek_token.token_type == TokenType::Unless && !self.peek_unless_has_then() {
                break;
            }
            if precedence >= self.peek_precedence() {
                break;
            }
            let infix = self.infix_fns.get(&self.peek_token.token_type).copied();
            let Some(infix_fn) = infix else { break };

            self.next_token(); // advance to operator token
            left = infix_fn(self, left)?;
        }

        Some(left)
    }

    // ---------------- Prefix parsers ----------------

    fn parse_identifier(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let ident = Identifier {
            token: tok.clone(),
            value: tok.literal,
        };

        // Check for struct literal: Name { field: value, ... }
        if self.peek_token.token_type == TokenType::LBrace {
            // Lookahead past any newlines after '{' to see if it's Ident ':' or '}'
            let mut lookahead_idx = 1;
            loop {
                let t = self.peek_nth(lookahead_idx).token_type.clone();
                if t == TokenType::Newline {
                    lookahead_idx += 1;
                    continue;
                }
                if t == TokenType::RBrace {
                    // Empty struct literal: Name {}
                    self.next_token(); // consume '{'
                    self.next_token(); // consume '}'
                    return Some(Expression::StructLiteral {
                        token: self.curr_token.clone(),
                        struct_name: ident.value,
                        field_values: Vec::new(),
                    });
                }
                if t == TokenType::Ident {
                    let t2 = self.peek_nth(lookahead_idx + 1).token_type.clone();
                    if t2 == TokenType::Colon {
                        // This is a struct literal
                        self.next_token(); // consume to '{'
                        return self.parse_struct_literal_from_ident(ident);
                    }
                }
                break;
            }
        }

        Some(Expression::Ident(ident))
    }

    fn parse_self_expression(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        Some(Expression::Ident(Identifier {
            token: tok,
            value: "self".to_string(),
        }))
    }

    fn parse_integer(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let value = tok.literal.parse::<i64>().ok()?;
        Some(Expression::Int { token: tok, value })
    }

    fn parse_float(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let value = tok.literal.parse::<f64>().ok()?;
        Some(Expression::Float { token: tok, value })
    }

    fn parse_string(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        Some(Expression::Str {
            token: tok.clone(),
            value: tok.literal,
        })
    }

    fn parse_string_interp(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone();
        let mut parts: Vec<StringInterpPart> = Vec::new();

        // curr_token is InterpStart, advance past it
        self.next_token();

        loop {
            match self.curr_token.token_type {
                TokenType::InterpEnd => break,
                TokenType::String => {
                    parts.push(StringInterpPart::Literal(self.curr_token.literal.clone()));
                    self.next_token();
                }
                TokenType::InterpExprStart => {
                    // Advance past InterpExprStart
                    self.next_token();
                    // Parse the expression inside {}
                    if let Some(expr) = self.parse_expression(Precedence::Lowest) {
                        parts.push(StringInterpPart::Expr(expr));
                    }
                    // After parse_expression, curr is last expr token, peek should be InterpExprEnd
                    if self.peek_token.token_type == TokenType::InterpExprEnd {
                        self.next_token(); // move to InterpExprEnd
                    }
                    self.next_token(); // move past InterpExprEnd
                }
                TokenType::Eof => {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.curr_token.span,
                        "unterminated string interpolation",
                        "make sure the string is properly closed with a matching quote",
                    ));
                    break;
                }
                _ => {
                    self.errors.push(Diagnostic::error(
                        self.curr_token.span,
                        format!(
                            "unexpected token in string interpolation: {:?}",
                            self.curr_token.literal
                        ),
                    ));
                    self.next_token();
                }
            }
        }

        Some(Expression::StringInterp { token, parts })
    }

    fn parse_char(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let value = tok.literal.chars().next()?;
        Some(Expression::Char { token: tok, value })
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let value = tok.token_type == TokenType::True;
        Some(Expression::Boolean { token: tok, value })
    }

    fn parse_none(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        Some(Expression::NoneExpr { token: tok })
    }

    fn parse_array(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '['
        let elements = self.parse_expression_list(TokenType::RBracket)?;
        Some(Expression::Array {
            token: tok,
            elements,
        })
    }

    fn is_angle_effect_name_token(tok: &Token) -> bool {
        matches!(tok.token_type, TokenType::Guard | TokenType::Fail)
            || (tok.token_type == TokenType::Ident
                && matches!(tok.literal.as_str(), "log" | "type" | "Error" | "Value"))
    }

    fn parse_effect_header(&mut self) -> Option<(Token, String, Option<String>)> {
        self.next_token(); // move to effect name after '<'
        if !Self::is_angle_effect_name_token(&self.curr_token) {
            self.errors.push(Diagnostic::error_with_hint(
                self.curr_token.span,
                format!(
                    "expected effect name after `<`, got {:?}",
                    self.curr_token.literal
                ),
                "valid effects: guard, fail, log, type, Error, Value",
            ));
            return None;
        }

        let effect_token = self.curr_token.clone();
        let effect_name = self.curr_token.literal.clone();
        let mut filter = None;

        if self.peek_token.token_type == TokenType::Lt {
            self.next_token(); // consume inner '<'
            self.next_token(); // move to filter name
            if self.curr_token.token_type != TokenType::Ident
                && self.curr_token.token_type != TokenType::None
            {
                self.errors.push(Diagnostic::error(
                    self.curr_token.span,
                    format!(
                        "expected effect filter name, got {:?}",
                        self.curr_token.literal
                    ),
                ));
                return None;
            }
            if self.curr_token.literal == "Error" && self.peek_token.token_type == TokenType::Lt {
                self.next_token(); // consume tag '<'
                if self.peek_token.token_type != TokenType::Ident {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.peek_token.span,
                        format!("expected error tag name, got {:?}", self.peek_token.literal),
                        "provide a tag name like Error<MyTag>",
                    ));
                    return None;
                }
                self.next_token(); // move to tag
                let tag = self.curr_token.literal.clone();
                filter = Some(tag);
                if self.peek_token.token_type == TokenType::RShift {
                    self.next_token(); // consume inner+outer closers
                    if self.peek_token.token_type == TokenType::Gt {
                        self.next_token(); // consume trailing outer '>' in nested forms like <log<Error<tag>>>
                    }
                    return Some((effect_token, effect_name, filter));
                }
                self.expect_peek(TokenType::Gt)?; // close Error<tag>
            } else {
                if self.curr_token.literal != "Error" {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.curr_token.span,
                        format!("unsupported effect filter '{}'", self.curr_token.literal),
                        "only `Error` is supported as a filter for guard/fail",
                    ));
                    return None;
                }
                filter = None;
            }
            if self.peek_token.token_type == TokenType::RShift {
                self.next_token(); // consume both inner and outer closers as '>>'
                return Some((effect_token, effect_name, filter));
            }
            self.expect_peek(TokenType::Gt)?; // close inner '>'
        }

        self.expect_peek(TokenType::Gt)?; // close outer '>'
        Some((effect_token, effect_name, filter))
    }

    fn parse_single_parenthesized_expression(&mut self) -> Option<Expression> {
        self.expect_peek(TokenType::LParen)?;
        self.next_token(); // move to inner expression
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenType::RParen)?;
        Some(expr)
    }

    fn synthetic_error_binding() -> Identifier {
        Identifier {
            token: Token {
                token_type: TokenType::Ident,
                literal: "_err".to_string(),
                span: Span::default(),
            },
            value: "_err".to_string(),
        }
    }

    /// Parse `<log>`, `<log>("msg")`, `<log<tag>>`, `<log<tag>>("msg")`,
    /// `<log<tag<sub>>>`, `<log<tag<sub>>>("msg")`.
    /// Called when curr_token is `<` and peek_token is `log`.
    fn parse_log_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone(); // the '<'
        self.next_token(); // move to 'log'

        let mut tag: Option<String> = None;
        let mut sub_tag: Option<String> = None;

        if self.peek_token.token_type == TokenType::Lt {
            // <log<tag...>>
            self.next_token(); // consume inner '<'
            self.next_token(); // move to tag name
            if self.curr_token.token_type != TokenType::Ident {
                self.errors.push(Diagnostic::error_with_hint(
                    self.curr_token.span,
                    format!(
                        "expected tag name after <log<, got {:?}",
                        self.curr_token.literal
                    ),
                    "use like: <log<debug>>(\"message\")",
                ));
                return None;
            }
            tag = Some(self.curr_token.literal.clone());

            if self.peek_token.token_type == TokenType::Lt {
                // <log<tag<sub>>>
                self.next_token(); // consume sub '<'
                self.next_token(); // move to sub_tag name
                if self.curr_token.token_type != TokenType::Ident {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.curr_token.span,
                        format!("expected sub-tag name, got {:?}", self.curr_token.literal),
                        "use like: <log<Error<network>>>(\"message\")",
                    ));
                    return None;
                }
                sub_tag = Some(self.curr_token.literal.clone());

                // Close the nested brackets: expect >>> or >> >
                if self.peek_token.token_type == TokenType::RShift {
                    self.next_token(); // consume '>>' (closes sub and tag)
                    if self.peek_token.token_type == TokenType::Gt {
                        self.next_token(); // consume trailing '>' (closes log)
                    }
                } else {
                    self.expect_peek(TokenType::Gt)?; // close sub '>'
                    if self.peek_token.token_type == TokenType::RShift {
                        self.next_token(); // consume '>>' (closes tag and log)
                    } else {
                        self.expect_peek(TokenType::Gt)?; // close tag '>'
                        self.expect_peek(TokenType::Gt)?; // close log '>'
                    }
                }
            } else if self.peek_token.token_type == TokenType::RShift {
                // <log<tag>> — >> closes both tag and log
                self.next_token(); // consume '>>'
            } else {
                self.expect_peek(TokenType::Gt)?; // close tag '>'
                self.expect_peek(TokenType::Gt)?; // close log '>'
            }
        } else {
            // <log> — no tags
            self.expect_peek(TokenType::Gt)?; // close '>'
        }

        // Optional parenthesized message
        let message = if self.peek_token.token_type == TokenType::LParen {
            self.next_token(); // consume '('
            if self.peek_token.token_type == TokenType::RParen {
                // Empty parens: <log<tag>>()
                self.next_token(); // consume ')'
                None
            } else {
                self.next_token(); // move to message expression
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(TokenType::RParen)?;
                Some(Box::new(expr))
            }
        } else {
            None
        };

        Some(Expression::Log {
            token,
            tag,
            sub_tag,
            message,
        })
    }

    fn parse_angle_effect_with_target(&mut self, value: Option<Expression>) -> Option<Expression> {
        let (token, effect_name, filter) = self.parse_effect_header()?;

        match effect_name.as_str() {
            "guard" => {
                if self.peek_token.token_type == TokenType::LParen {
                    let fallback = self.parse_single_parenthesized_expression()?;
                    let Some(value) = value else {
                        self.errors.push(Diagnostic::error_with_hint(
                            token.span,
                            "<guard>(...) requires a target expression",
                            "use like: value <guard>(fallback)",
                        ));
                        return None;
                    };
                    Some(Expression::Guard {
                        token,
                        value: Box::new(value),
                        binding: Self::synthetic_error_binding(),
                        error_tag: filter,
                        fallback: Box::new(fallback),
                    })
                } else {
                    self.next_token(); // move to binding identifier
                    if self.curr_token.token_type != TokenType::Ident {
                        self.errors.push(Diagnostic::error_with_hint(
                            self.curr_token.span,
                            format!(
                                "expected identifier after <guard>, got {:?}",
                                self.curr_token.literal
                            ),
                            "use like: value <guard> err -> fallback",
                        ));
                        return None;
                    }
                    let binding = Identifier {
                        token: self.curr_token.clone(),
                        value: self.curr_token.literal.clone(),
                    };
                    self.expect_peek(TokenType::Arrow)?;
                    self.next_token(); // move to fallback
                    let fallback = self.parse_expression(Precedence::Lowest)?;
                    let Some(value) = value else {
                        self.errors.push(Diagnostic::error_with_hint(
                            token.span,
                            "<guard> err -> ... requires a target expression",
                            "use like: value <guard> err -> fallback",
                        ));
                        return None;
                    };
                    Some(Expression::Guard {
                        token,
                        value: Box::new(value),
                        binding,
                        error_tag: filter,
                        fallback: Box::new(fallback),
                    })
                }
            }
            "fail" => {
                let fail_arg = self.parse_single_parenthesized_expression()?;
                match value {
                    Some(value) => Some(Expression::Guard {
                        token: token.clone(),
                        value: Box::new(value),
                        binding: Self::synthetic_error_binding(),
                        error_tag: filter,
                        fallback: Box::new(Expression::Fail {
                            token,
                            value: Box::new(fail_arg),
                        }),
                    }),
                    None => Some(Expression::Fail {
                        token,
                        value: Box::new(fail_arg),
                    }),
                }
            }
            "log" => {
                self.errors.push(Diagnostic::error_with_hint(
                    token.span,
                    "<log> is no longer a postfix effect",
                    "use <log>(\"message\") or <log<tag>>(\"message\") as a standalone expression",
                ));
                None
            }
            _ => {
                self.errors.push(Diagnostic::error_with_hint(
                    token.span,
                    format!("unknown angle effect '{}'", effect_name),
                    "valid effects: guard, fail, log, type, Error, Value",
                ));
                None
            }
        }
    }

    fn parse_angle_effect_expression(&mut self) -> Option<Expression> {
        if self.peek_token.token_type == TokenType::Ident && self.peek_token.literal == "log" {
            return self.parse_log_expression();
        }

        if self.peek_token.token_type == TokenType::Ident && self.peek_token.literal == "type" {
            self.next_token(); // move to 'type'
            let token = self.curr_token.clone();
            self.expect_peek(TokenType::Lt)?;
            let target = self.parse_type_annotation()?;
            self.expect_peek(TokenType::LParen)?;
            self.next_token(); // move to wrapped expression
            let value = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::RParen)?;
            return Some(Expression::TypeWrap {
                token,
                target,
                value: Box::new(value),
            });
        }

        if self.peek_token.token_type == TokenType::Ident
            && (self.peek_token.literal == "Error" || self.peek_token.literal == "Value")
        {
            self.next_token(); // move to constructor name
            let token = self.curr_token.clone();
            if self.curr_token.literal == "Error" {
                let tag = if self.peek_token.token_type == TokenType::Lt {
                    self.next_token(); // consume inner '<'
                    if self.peek_token.token_type != TokenType::Ident {
                        self.errors.push(Diagnostic::error_with_hint(
                            self.peek_token.span,
                            format!("expected error tag name, got {:?}", self.peek_token.literal),
                            "provide a tag name like Error<MyTag>",
                        ));
                        return None;
                    }
                    self.next_token(); // move to tag
                    let tag = self.curr_token.literal.clone();
                    if self.peek_token.token_type == TokenType::RShift {
                        self.next_token(); // consume both closing '>' tokens
                    } else {
                        self.expect_peek(TokenType::Gt)?; // close inner '>'
                        self.expect_peek(TokenType::Gt)?; // close outer '>'
                    }
                    Some(tag)
                } else {
                    self.expect_peek(TokenType::Gt)?;
                    None
                };
                let value = self.parse_single_parenthesized_expression()?;
                return Some(Expression::ErrorConstruct {
                    token,
                    tag,
                    value: Box::new(value),
                });
            }

            self.expect_peek(TokenType::Gt)?;
            let value = self.parse_single_parenthesized_expression()?;
            return Some(Expression::ValueConstruct {
                token,
                value: Box::new(value),
            });
        }

        self.parse_angle_effect_with_target(None)
    }

    fn parse_fail_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone(); // 'fail'
        self.next_token(); // move to failure payload expression
        let value = self.parse_expression(Precedence::Prefix)?;
        Some(Expression::Fail {
            token,
            value: Box::new(value),
        })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        let operator = tok.literal.clone();

        self.next_token(); // move to right expression
        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix {
            token: tok,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '('
        self.next_token(); // enter group

        // Empty tuple: ()
        if self.curr_token.token_type == TokenType::RParen {
            return Some(Expression::TupleLiteral {
                token: tok,
                elements: Vec::new(),
            });
        }

        let first = self.parse_expression(Precedence::Lowest)?;

        // If comma follows, it's a tuple
        if self.peek_token.token_type == TokenType::Comma {
            let mut elements = vec![first];
            while self.peek_token.token_type == TokenType::Comma {
                self.next_token(); // consume ','
                                   // Trailing comma: (a,)
                if self.peek_token.token_type == TokenType::RParen {
                    break;
                }
                self.next_token(); // move to next element
                elements.push(self.parse_expression(Precedence::Lowest)?);
            }
            self.expect_peek(TokenType::RParen)?;
            return Some(Expression::TupleLiteral {
                token: tok,
                elements,
            });
        }

        self.expect_peek(TokenType::RParen)?;
        Some(Expression::Grouped(Box::new(first)))
    }

    fn parse_map_literal(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '{'
        let mut entries = Vec::new();

        // Skip newlines
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        // Empty map: {}
        if self.peek_token.token_type == TokenType::RBrace {
            self.next_token();
            return Some(Expression::MapLiteral {
                token: tok,
                entries,
            });
        }

        loop {
            self.next_token(); // move to key

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                break;
            }

            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::Colon)?;
            self.next_token(); // move to value
            let value = self.parse_expression(Precedence::Lowest)?;
            entries.push((key, value));

            // Skip comma if present
            if self.peek_token.token_type == TokenType::Comma {
                self.next_token();
            }

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.peek_token.token_type == TokenType::RBrace {
                self.next_token();
                break;
            }
        }

        Some(Expression::MapLiteral {
            token: tok,
            entries,
        })
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone(); // 'fun'

        self.expect_peek(TokenType::LParen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::LBrace)?;
        let body = self.parse_block()?;

        Some(Expression::FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<TypedParam>> {
        let mut params = Vec::new();
        let mut seen_optional = false;

        if self.peek_token.token_type == TokenType::RParen {
            self.next_token();
            return Some(params);
        }

        self.next_token();
        let param = self.parse_single_param(&mut seen_optional)?;
        params.push(param);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token(); // ','
            self.next_token(); // next param
            let param = self.parse_single_param(&mut seen_optional)?;
            params.push(param);
        }

        self.expect_peek(TokenType::RParen)?;
        Some(params)
    }

    /// Parse a single function parameter: name, name? <type>, name <type> = default
    fn parse_single_param(&mut self, seen_optional: &mut bool) -> Option<TypedParam> {
        let ident = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        // Check for '?' (optional marker) — '?' is lexed as Illegal
        let optional =
            if self.peek_token.token_type == TokenType::Illegal && self.peek_token.literal == "?" {
                self.next_token(); // consume '?'
                true
            } else {
                false
            };

        let type_ann = if self.peek_token.token_type == TokenType::Lt {
            self.next_token(); // move to '<'
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        // Check for default value: = expr
        let default = if self.peek_token.token_type == TokenType::Assign {
            self.next_token(); // consume '='
            self.next_token(); // move to default expression
            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };

        // Validate ordering: once we see an optional/default param, all following must be too
        let has_default = optional || default.is_some();
        if *seen_optional && !has_default {
            self.errors.push(Diagnostic::error_with_hint(
                ident.token.span,
                format!(
                    "required parameter '{}' cannot follow optional/default parameters",
                    ident.value
                ),
                "move required parameters before optional ones",
            ));
            return None;
        }
        if has_default {
            *seen_optional = true;
        }

        Some(TypedParam {
            ident,
            type_ann,
            default,
            optional,
        })
    }

    fn parse_named_function_statement(&mut self) -> Option<Statement> {
        let fun_token = self.curr_token.clone(); // 'fun'

        self.next_token(); // move to function name
        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_peek(TokenType::LParen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::LBrace)?;
        let body = self.parse_block()?;

        Some(Statement::Let {
            name,
            value: Expression::FunctionLiteral {
                token: fun_token,
                parameters,
                body,
            },
        })
    }

    fn parse_give_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'give'
        self.next_token(); // move to expression
        let value = self.parse_expression(Precedence::Lowest)?;
        Some(Statement::Give { token, value })
    }

    fn parse_introduce_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'introduce' or 'intro'

        if self.peek_token.token_type == TokenType::LBrace {
            // Selective import: introduce {name1, name2} from module
            self.next_token(); // consume '{'
            let mut names = Vec::new();
            // Parse comma-separated identifiers
            if self.peek_token.token_type != TokenType::RBrace {
                self.next_token(); // move to first name
                names.push(Identifier {
                    token: self.curr_token.clone(),
                    value: self.curr_token.literal.clone(),
                });
                while self.peek_token.token_type == TokenType::Comma {
                    self.next_token(); // consume ','
                    self.next_token(); // move to next name
                    names.push(Identifier {
                        token: self.curr_token.clone(),
                        value: self.curr_token.literal.clone(),
                    });
                }
            }
            self.expect_peek(TokenType::RBrace)?; // consume '}'
            self.expect_peek(TokenType::From)?; // consume 'from'
            let path = self.parse_module_path()?;
            Some(Statement::Introduce {
                token,
                path,
                selective: Some(names),
            })
        } else {
            // Whole-module import: introduce math / introduce .utils
            let path = self.parse_module_path()?;
            Some(Statement::Introduce {
                token,
                path,
                selective: None,
            })
        }
    }

    fn parse_module_path(&mut self) -> Option<ModulePath> {
        let mut is_relative = false;
        let mut parent_levels: usize = 0;
        let mut segments = Vec::new();

        // Count leading dots
        if self.peek_token.token_type == TokenType::FullStop {
            is_relative = true;
            // Count dots: first dot means current dir (parent_levels=0)
            // additional dots increment parent_levels
            let mut dot_count = 0;
            while self.peek_token.token_type == TokenType::FullStop {
                self.next_token(); // consume '.'
                dot_count += 1;
                // Check if next is also a dot (consecutive dots)
                // But we need to distinguish ".name" from "..name"
                // After consuming a dot, if peek is Ident, we stop counting dots
                if self.peek_token.token_type == TokenType::Ident {
                    break;
                }
            }
            if dot_count > 1 {
                parent_levels = dot_count - 1;
            }
        }

        // Now consume ident segments separated by dots
        if self.peek_token.token_type == TokenType::Ident {
            self.next_token(); // consume first ident
            segments.push(self.curr_token.literal.clone());

            while self.peek_token.token_type == TokenType::FullStop {
                self.next_token(); // consume '.'
                if self.peek_token.token_type == TokenType::Ident {
                    self.next_token(); // consume next ident
                    segments.push(self.curr_token.literal.clone());
                } else {
                    self.errors.push(Diagnostic::error(
                        self.peek_token.span,
                        "expected identifier after `.` in module path",
                    ));
                    return None;
                }
            }
        } else {
            self.errors.push(Diagnostic::error_with_hint(
                self.peek_token.span,
                format!("expected module name, got {:?}", self.peek_token.literal),
                "use like: introduce math or introduce .utils",
            ));
            return None;
        }

        Some(ModulePath {
            segments,
            is_relative,
            parent_levels,
        })
    }

    // ---------------- Infix parsers ----------------

    fn parse_guard_expression(&mut self, value: Expression) -> Option<Expression> {
        let token = self.curr_token.clone(); // 'guard'

        self.next_token(); // move to binding identifier
        if self.curr_token.token_type != TokenType::Ident {
            self.errors.push(Diagnostic::error_with_hint(
                self.curr_token.span,
                format!(
                    "expected identifier after guard, got {:?}",
                    self.curr_token.literal
                ),
                "use like: value guard err -> fallback",
            ));
            return None;
        }
        let binding = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_peek(TokenType::Arrow)?;
        self.next_token(); // move to fallback expression
        let fallback = self.parse_expression(Precedence::Lowest)?;

        Some(Expression::Guard {
            token,
            value: Box::new(value),
            binding,
            error_tag: None,
            fallback: Box::new(fallback),
        })
    }

    fn parse_lt_expression(&mut self, left: Expression) -> Option<Expression> {
        if Self::is_angle_effect_name_token(&self.peek_token) {
            return self.parse_angle_effect_with_target(Some(left));
        }
        self.parse_infix_expression(left)
    }

    fn parse_unless_expression(&mut self, consequence: Expression) -> Option<Expression> {
        let token = self.curr_token.clone(); // 'unless'

        self.next_token(); // move to condition
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Then)?;
        self.next_token(); // move to fallback expression
        let alternative = self.parse_expression(Precedence::Lowest)?;

        Some(Expression::Unless {
            token,
            consequence: Box::new(consequence),
            condition: Box::new(condition),
            alternative: Box::new(alternative),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // operator
        let operator = tok.literal.clone();
        let precedence = self.curr_precedence();

        self.next_token(); // move to right
        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix {
            token: tok,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    // Postfix: x++
    //
    // Pratt trick: register it as an "infix" function with high precedence,
    // but it doesn't parse a right-hand expression.
    fn parse_postfix_expression(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '++'
        Some(Expression::Postfix {
            token: tok.clone(),
            left: Box::new(left),
            operator: tok.literal,
        })
    }

    // Call: f(arg1, arg2, ...) or f(name=val, ...)
    // Trigger token is '(' *after* the function expression.
    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '('
        let (args, named_args) = self.parse_call_args()?;

        Some(Expression::Call {
            token: tok,
            function: Box::new(function),
            args,
            named_args,
        })
    }

    /// Parse call arguments, handling both positional and named: f(a, b, name=val)
    fn parse_call_args(&mut self) -> Option<(Vec<Expression>, Vec<(String, Expression)>)> {
        let mut args = Vec::new();
        let mut named_args: Vec<(String, Expression)> = Vec::new();
        let mut in_named = false;

        // Handle empty args: f()
        if self.peek_token.token_type == TokenType::RParen {
            self.next_token();
            return Some((args, named_args));
        }

        loop {
            self.next_token(); // move to arg

            // Check if this is a named arg: ident = expr
            if self.curr_token.token_type == TokenType::Ident
                && self.peek_token.token_type == TokenType::Assign
            {
                let name = self.curr_token.literal.clone();
                self.next_token(); // consume '='
                self.next_token(); // move to value
                let value = self.parse_expression(Precedence::Lowest)?;
                named_args.push((name, value));
                in_named = true;
            } else {
                if in_named {
                    self.errors.push(Diagnostic::error_with_hint(
                        self.curr_token.span,
                        "positional argument cannot follow named arguments",
                        "put all positional arguments before named ones",
                    ));
                    return None;
                }
                let expr = self.parse_expression(Precedence::Lowest)?;
                args.push(expr);
            }

            if self.peek_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // consume comma
        }

        self.expect_peek(TokenType::RParen)?;
        Some((args, named_args))
    }

    // Index: arr[index]
    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '['
        self.next_token(); // move past '['

        // Check for slice: [:end], [:]
        if self.curr_token.token_type == TokenType::Colon {
            // No start — [:end] or [:]
            if self.peek_token.token_type == TokenType::RBracket {
                self.next_token(); // consume ']'
                return Some(Expression::Slice {
                    token: tok,
                    left: Box::new(left),
                    start: None,
                    end: None,
                });
            }
            self.next_token(); // move to end expression
            let end = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::RBracket)?;
            return Some(Expression::Slice {
                token: tok,
                left: Box::new(left),
                start: None,
                end: Some(Box::new(end)),
            });
        }

        let index = self.parse_expression(Precedence::Lowest)?;

        // Check for slice: [start:end] or [start:]
        if self.peek_token.token_type == TokenType::Colon {
            self.next_token(); // consume ':'
            if self.peek_token.token_type == TokenType::RBracket {
                self.next_token(); // consume ']'
                return Some(Expression::Slice {
                    token: tok,
                    left: Box::new(left),
                    start: Some(Box::new(index)),
                    end: None,
                });
            }
            self.next_token(); // move to end expression
            let end = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::RBracket)?;
            return Some(Expression::Slice {
                token: tok,
                left: Box::new(left),
                start: Some(Box::new(index)),
                end: Some(Box::new(end)),
            });
        }

        self.expect_peek(TokenType::RBracket)?;

        Some(Expression::Index {
            token: tok,
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_dot_expression(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '.'
        self.next_token(); // move to field name
        let field = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };
        Some(Expression::DotAccess {
            token: tok,
            left: Box::new(left),
            field,
        })
    }

    fn parse_struct_literal_from_ident(&mut self, ident: Identifier) -> Option<Expression> {
        let struct_name = ident.value;
        let tok = self.curr_token.clone(); // '{'

        let mut field_values = Vec::new();

        // Skip newlines after '{'
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        if self.peek_token.token_type == TokenType::RBrace {
            self.next_token(); // consume '}'
            return Some(Expression::StructLiteral {
                token: tok,
                struct_name,
                field_values,
            });
        }

        // Parse field: value pairs
        loop {
            self.next_token(); // move to field name

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                break;
            }

            let field_name = self.curr_token.literal.clone();
            self.expect_peek(TokenType::Colon)?; // ':'
            self.next_token(); // move to value expression
            let value = self.parse_expression(Precedence::Lowest)?;
            field_values.push((field_name, value));

            // Skip comma if present
            if self.peek_token.token_type == TokenType::Comma {
                self.next_token();
            }

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.peek_token.token_type == TokenType::RBrace {
                self.next_token(); // consume '}'
                break;
            }
        }

        Some(Expression::StructLiteral {
            token: tok,
            struct_name,
            field_values,
        })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut args = Vec::new();

        // Handle empty args: f()
        if self.peek_token.token_type == end {
            self.next_token(); // consume end
            return Some(args);
        }

        self.next_token(); // first arg
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token(); // consume comma
            self.next_token(); // next arg
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;
        Some(args)
    }

    // ---------------- Statement parsers ----------------

    // each num in x { ... }
    fn parse_main_block(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'main'
        self.expect_peek(TokenType::LBrace)?; // curr is now '{'
        let body = self.parse_block()?;
        Some(Statement::Main { token, body })
    }

    fn parse_each_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'each'

        self.next_token(); // move to variable name
        let variable = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_peek(TokenType::In)?; // 'in'
        self.next_token(); // move to iterable

        let iterable = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::LBrace)?; // '{'
        let body = self.parse_block()?;

        Some(Statement::Each {
            token,
            variable,
            iterable,
            body,
        })
    }

    // repeat when condition { ... }
    fn parse_repeat_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'repeat'

        let negate = self.peek_token.token_type == TokenType::Unless;
        if negate {
            self.expect_peek(TokenType::Unless)?;
        } else {
            self.expect_peek(TokenType::When)?;
        }
        self.next_token(); // move to condition

        let raw_condition = self.parse_expression(Precedence::Lowest)?;

        let condition = if negate {
            Self::negate_expression(raw_condition)
        } else {
            raw_condition
        };

        self.expect_peek(TokenType::LBrace)?; // '{'
        let body = self.parse_block()?;

        Some(Statement::Repeat {
            token,
            condition,
            body,
        })
    }

    // pattern ten(x) when x == 10
    fn parse_pattern_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'pattern'

        self.next_token(); // move to pattern name
        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_peek(TokenType::LParen)?; // '('

        // Parse parameters
        let mut params = Vec::new();
        if self.peek_token.token_type != TokenType::RParen {
            self.next_token();
            params.push(Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            });

            while self.peek_token.token_type == TokenType::Comma {
                self.next_token(); // ','
                self.next_token(); // next param
                params.push(Identifier {
                    token: self.curr_token.clone(),
                    value: self.curr_token.literal.clone(),
                });
            }
        }
        self.expect_peek(TokenType::RParen)?; // ')'

        self.expect_peek(TokenType::When)?; // 'when'
        self.next_token(); // move to condition

        let condition = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Pattern {
            token,
            name,
            params,
            condition,
        })
    }

    // choose y { ten -> print("10"), else -> ... }
    fn parse_choose_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'choose'

        self.next_token(); // move to subject
        let subject = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::LBrace)?; // '{'

        let mut arms = Vec::new();

        // Skip newlines after opening brace
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        // Parse arms until we hit '}'
        while self.peek_token.token_type != TokenType::RBrace
            && self.peek_token.token_type != TokenType::Eof
        {
            self.next_token(); // move to pattern name or 'else'

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                break;
            }

            if self.curr_token.token_type == TokenType::Pattern {
                // Inline pattern definition
                self.next_token(); // move to pattern name
                let pattern_name = self.curr_token.literal.clone();

                self.expect_peek(TokenType::LParen)?; // '('

                let mut params = Vec::new();
                if self.peek_token.token_type != TokenType::RParen {
                    self.next_token();
                    params.push(Identifier {
                        token: self.curr_token.clone(),
                        value: self.curr_token.literal.clone(),
                    });
                    while self.peek_token.token_type == TokenType::Comma {
                        self.next_token(); // ','
                        self.next_token(); // next param
                        params.push(Identifier {
                            token: self.curr_token.clone(),
                            value: self.curr_token.literal.clone(),
                        });
                    }
                }
                self.expect_peek(TokenType::RParen)?; // ')'

                self.expect_peek(TokenType::When)?; // 'when'
                self.next_token(); // move to condition

                let condition = self.parse_expression(Precedence::Lowest)?;

                self.expect_peek(TokenType::Arrow)?; // '->'
                self.next_token(); // move to body expression

                let body = self.parse_expression(Precedence::Lowest)?;

                arms.push(ChooseArm {
                    pattern_name,
                    inline_params: Some(params),
                    inline_condition: Some(condition),
                    body,
                });
            } else {
                // Reference to pre-defined pattern
                let pattern_name = self.curr_token.literal.clone();

                self.expect_peek(TokenType::Arrow)?; // '->'
                self.next_token(); // move to body expression

                let body = self.parse_expression(Precedence::Lowest)?;

                arms.push(ChooseArm {
                    pattern_name,
                    inline_params: None,
                    inline_condition: None,
                    body,
                });
            }

            // Skip comma if present
            if self.peek_token.token_type == TokenType::Comma {
                self.next_token();
            }

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }
        }

        self.expect_peek(TokenType::RBrace)?; // '}'

        Some(Statement::Choose {
            token,
            subject,
            arms,
        })
    }

    fn parse_option_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone(); // 'option'
        self.expect_peek(TokenType::LBrace)?; // '{'

        let mut arms = Vec::new();
        let mut default: Option<Vec<Statement>> = None;
        let mut error_default: Option<Vec<Statement>> = None;

        // Skip newlines after '{'
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        while self.peek_token.token_type != TokenType::RBrace
            && self.peek_token.token_type != TokenType::Eof
        {
            self.next_token(); // move to condition/default expression

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                break;
            }

            if self.curr_token.token_type == TokenType::Lt
                && self.peek_token.token_type == TokenType::Ident
                && self.peek_token.literal == "Error"
                && self.peek_nth(1).token_type == TokenType::Gt
                && self.peek_nth(2).token_type == TokenType::Arrow
            {
                self.next_token(); // move to Error
                self.expect_peek(TokenType::Gt)?; // close '>'
                self.expect_peek(TokenType::Arrow)?; // '->'
                self.next_token(); // move to body

                let body = if self.curr_token.token_type == TokenType::LBrace {
                    self.parse_block()?
                } else {
                    let stmt = self.parse_statement()?;
                    vec![stmt]
                };

                error_default = Some(body);

                if self.peek_token.token_type == TokenType::Comma {
                    self.next_token();
                }
                while self.peek_token.token_type == TokenType::Newline {
                    self.next_token();
                }
                continue;
            }

            // If current token is '{', treat as a block default arm
            // (not a map literal) since bare '{' in option position is a block.
            if self.curr_token.token_type == TokenType::LBrace {
                default = Some(self.parse_block()?);
                // Skip comma if present
                if self.peek_token.token_type == TokenType::Comma {
                    self.next_token();
                }
                // Skip newlines
                while self.peek_token.token_type == TokenType::Newline {
                    self.next_token();
                }
                break;
            }

            let expr = self.parse_expression(Precedence::Lowest)?;

            if self.peek_token.token_type == TokenType::Arrow {
                // condition arm: expr -> body
                self.next_token(); // consume '->'
                self.next_token(); // move to body

                let body = if self.curr_token.token_type == TokenType::LBrace {
                    self.parse_block()?
                } else {
                    let stmt = self.parse_statement()?;
                    vec![stmt]
                };

                arms.push(OptionArm {
                    condition: expr,
                    body,
                });
            } else if self.peek_token.token_type == TokenType::LBrace {
                // User likely forgot '->' before the block
                self.errors.push(Diagnostic::error_with_hint(
                    self.peek_token.span,
                    "missing `->` before block in option arm",
                    "add `->` between the condition and its body: condition -> { ... }",
                ));
                // Recovery: skip this malformed arm by jumping over the block
                self.next_token(); // consume '{'
                let mut depth = 1i32;
                while depth > 0 && self.curr_token.token_type != TokenType::Eof {
                    self.next_token();
                    match self.curr_token.token_type {
                        TokenType::LBrace => depth += 1,
                        TokenType::RBrace => depth -= 1,
                        _ => {}
                    }
                }
                // Skip comma if present
                if self.peek_token.token_type == TokenType::Comma {
                    self.next_token();
                }
                // Skip newlines
                while self.peek_token.token_type == TokenType::Newline {
                    self.next_token();
                }
                continue;
            } else if arms.is_empty() && self.peek_token.token_type == TokenType::Comma {
                // Ternary form: option { cond, true_val, false_val }
                self.next_token(); // consume ','
                self.next_token(); // move to true value
                let true_expr = self.parse_expression(Precedence::Lowest)?;

                self.expect_peek(TokenType::Comma)?; // ','
                self.next_token(); // move to false value
                let false_expr = self.parse_expression(Precedence::Lowest)?;

                arms.push(OptionArm {
                    condition: expr,
                    body: vec![Statement::Expr(true_expr)],
                });
                default = Some(vec![Statement::Expr(false_expr)]);
                break;
            } else {
                // bare default (no ->)
                default = Some(vec![Statement::Expr(expr)]);
                // Skip comma if present
                if self.peek_token.token_type == TokenType::Comma {
                    self.next_token();
                }
                // Skip newlines
                while self.peek_token.token_type == TokenType::Newline {
                    self.next_token();
                }
                break;
            }

            // Skip comma if present
            if self.peek_token.token_type == TokenType::Comma {
                self.next_token();
            }

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }
        }

        self.expect_peek(TokenType::RBrace)?; // '}'

        Some(Expression::Option {
            token,
            arms,
            default,
            error_default,
        })
    }

    fn parse_unless_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'unless'

        self.next_token(); // move to condition
        let condition = self.parse_expression(Precedence::Lowest)?;

        let negated = Self::negate_expression(condition);

        self.expect_peek(TokenType::LBrace)?; // '{'
        let consequence = self.parse_block()?;

        Some(Statement::If {
            token,
            condition: negated,
            consequence,
            alternative: None,
        })
    }

    fn try_postfix_guard(&mut self, stmt: Statement) -> Option<Statement> {
        // Check if the statement is eligible for postfix guards
        let eligible = matches!(
            &stmt,
            Statement::Expr(_)
                | Statement::Give { .. }
                | Statement::Skip
                | Statement::Stop
                | Statement::Assign { .. }
                | Statement::DotAssign { .. }
                | Statement::IndexAssign { .. }
        );

        if !eligible {
            return Some(stmt);
        }

        if self.peek_token.token_type == TokenType::When {
            self.next_token(); // consume 'when'
            self.next_token(); // move to condition
            let condition = self.parse_expression(Precedence::Lowest)?;
            return Some(Statement::If {
                token: Token {
                    token_type: TokenType::If,
                    literal: "if".to_string(),
                    span: Span::default(),
                },
                condition,
                consequence: vec![stmt],
                alternative: None,
            });
        }

        if self.peek_token.token_type == TokenType::Unless {
            self.next_token(); // consume 'unless'
            self.next_token(); // move to condition
            let condition = self.parse_expression(Precedence::Lowest)?;
            let alternative = if self.peek_token.token_type == TokenType::Then {
                self.next_token(); // consume 'then'
                self.next_token(); // move to alternative statement
                let stmt = self.parse_statement_inner()?;
                Some(vec![self.try_postfix_guard(stmt)?])
            } else {
                None
            };
            let negated = Self::negate_expression(condition);
            return Some(Statement::If {
                token: Token {
                    token_type: TokenType::If,
                    literal: "if".to_string(),
                    span: Span::default(),
                },
                condition: negated,
                consequence: vec![stmt],
                alternative,
            });
        }

        Some(stmt)
    }

    // Parse a block of statements between { and }
    fn parse_block(&mut self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();

        self.next_token(); // move past '{'

        // Skip leading newlines
        while self.curr_token.token_type == TokenType::Newline {
            self.next_token();
        }

        while self.curr_token.token_type != TokenType::RBrace
            && self.curr_token.token_type != TokenType::Eof
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }
        }

        Some(statements)
    }

    // ---------------- Struct parsers ----------------

    fn parse_struct_definition(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'struct'

        self.next_token(); // move to struct name
        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        // Optional parent: struct American(Person)
        let parent = if self.peek_token.token_type == TokenType::LParen {
            self.next_token(); // consume '('
            self.next_token(); // move to parent name
            let parent_ident = Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            };
            self.expect_peek(TokenType::RParen)?; // consume ')'
            Some(parent_ident)
        } else {
            None
        };

        self.expect_peek(TokenType::LBrace)?; // '{'

        let mut fields = Vec::new();

        // Skip newlines after '{'
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        // Parse fields: name <type>
        while self.peek_token.token_type != TokenType::RBrace
            && self.peek_token.token_type != TokenType::Eof
        {
            self.next_token(); // move to field name

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                return Some(Statement::StructDef {
                    token,
                    name,
                    parent,
                    fields,
                });
            }

            let hidden = if self.curr_token.token_type == TokenType::Hide {
                self.next_token(); // consume 'hide', now at field name
                true
            } else {
                false
            };

            let field_name = Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            };

            self.expect_peek(TokenType::Lt)?; // '<'
            let type_ann = self.parse_type_annotation()?;

            fields.push(crate::ast::StructField {
                name: field_name,
                type_ann,
                hidden,
            });

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }
        }

        self.expect_peek(TokenType::RBrace)?; // '}'

        Some(Statement::StructDef {
            token,
            name,
            parent,
            fields,
        })
    }

    fn parse_contains_statement(&mut self) -> Option<Statement> {
        let struct_name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };
        let token = self.curr_token.clone();

        self.expect_peek(TokenType::Contains)?; // 'contains'
        self.expect_peek(TokenType::LBrace)?; // '{'

        let mut methods = Vec::new();

        // Skip newlines after '{'
        while self.peek_token.token_type == TokenType::Newline {
            self.next_token();
        }

        // Parse methods: fun name(params) { body }
        while self.peek_token.token_type != TokenType::RBrace
            && self.peek_token.token_type != TokenType::Eof
        {
            self.next_token(); // move to 'fun'

            // Skip newlines
            while self.curr_token.token_type == TokenType::Newline {
                self.next_token();
            }

            if self.curr_token.token_type == TokenType::RBrace {
                break;
            }

            if self.curr_token.token_type != TokenType::Function {
                self.errors.push(Diagnostic::error_with_hint(
                    self.curr_token.span,
                    format!(
                        "expected `fun` inside contains block, got {:?}",
                        self.curr_token.literal
                    ),
                    "contains blocks should only contain method definitions: fun name() { ... }",
                ));
                return None;
            }

            let fun_token = self.curr_token.clone();

            self.next_token(); // move to method name
            let method_name = Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            };

            self.expect_peek(TokenType::LParen)?;
            let parameters = self.parse_function_parameters()?;

            self.expect_peek(TokenType::LBrace)?;
            let body = self.parse_block()?;

            let func_expr = Expression::FunctionLiteral {
                token: fun_token,
                parameters,
                body,
            };

            methods.push((method_name, func_expr));

            // Skip newlines
            while self.peek_token.token_type == TokenType::Newline {
                self.next_token();
            }
        }

        self.expect_peek(TokenType::RBrace)?; // '}'

        Some(Statement::ContainsDef {
            token,
            struct_name,
            methods,
        })
    }

    // ---------------- Helpers ----------------

    /// Skip tokens until we find a likely statement boundary for error recovery.
    /// This lets us report multiple errors instead of stopping at the first one.
    fn synchronize(&mut self) {
        // Track brace depth so we can skip over nested blocks
        let mut brace_depth: i32 = 0;
        loop {
            match self.curr_token.token_type {
                TokenType::Eof => break,
                TokenType::LBrace => {
                    brace_depth += 1;
                    self.next_token();
                    continue;
                }
                TokenType::RBrace => {
                    if brace_depth > 0 {
                        brace_depth -= 1;
                        self.next_token();
                        continue;
                    }
                    // At depth 0, a '}' ends the enclosing context
                    self.next_token();
                    break;
                }
                _ => {}
            }
            if brace_depth == 0 {
                // Statement-starting keywords at the top level are sync points
                match self.curr_token.token_type {
                    TokenType::Each
                    | TokenType::Repeat
                    | TokenType::Pattern
                    | TokenType::Choose
                    | TokenType::Struct
                    | TokenType::Function
                    | TokenType::Give
                    | TokenType::Introduce
                    | TokenType::Unless
                    | TokenType::OptionKw => break,
                    _ => {}
                }
                // A newline followed by a non-trivial token at depth 0 is likely a new statement
                if self.curr_token.token_type == TokenType::Newline
                    && !matches!(
                        self.peek_token.token_type,
                        TokenType::Newline | TokenType::Eof
                    )
                {
                    self.next_token();
                    break;
                }
            }
            self.next_token();
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        if let Some(buffered) = self.lookahead_buffer.pop_front() {
            self.peek_token = buffered;
        } else {
            self.peek_token = self.lexer.next_token();
        }
    }

    fn peek_nth(&mut self, n: usize) -> &Token {
        // peek_nth(0) is peek_token, peek_nth(1) is one past peek_token, etc.
        if n == 0 {
            return &self.peek_token;
        }
        let needed = n; // how many tokens past peek_token
        while self.lookahead_buffer.len() < needed {
            let tok = self.lexer.next_token();
            self.lookahead_buffer.push_back(tok);
        }
        &self.lookahead_buffer[needed - 1]
    }

    fn peek_unless_has_then(&mut self) -> bool {
        if self.peek_token.token_type != TokenType::Unless {
            return false;
        }

        let mut idx = 1;
        loop {
            match self.peek_nth(idx).token_type {
                TokenType::Then => return true,
                TokenType::Newline | TokenType::Eof | TokenType::Comma | TokenType::RBrace => {
                    return false;
                }
                _ => idx += 1,
            }
        }
    }

    fn skip_type_member_at(&mut self, mut pos: usize) -> Option<usize> {
        let token_type = self.peek_nth(pos).token_type.clone();
        let token_literal = self.peek_nth(pos).literal.clone();
        if !Self::is_type_name_token(&token_type) {
            return None;
        }
        let next_type = self.peek_nth(pos + 1).token_type.clone();
        let next_next_type = self.peek_nth(pos + 2).token_type.clone();
        if token_type == TokenType::Ident
            && token_literal == "Error"
            && next_type == TokenType::Lt
            && next_next_type == TokenType::Ident
        {
            let closing_type = self.peek_nth(pos + 3).token_type.clone();
            if matches!(closing_type, TokenType::Gt | TokenType::RShift) {
                return Some(pos + 4);
            }
            return None;
        }
        pos += 1;
        Some(pos)
    }

    /// Scans past a type annotation starting at peek_nth(offset).
    /// Supports both `<A || B>` and legacy `<A> || <B>` unions.
    /// Returns the offset of the token AFTER the full type annotation.
    fn skip_type_annotation(&mut self, start: usize) -> Option<usize> {
        if self.peek_nth(start).token_type != TokenType::Lt {
            return None;
        }
        let mut pos = start + 1;
        pos = self.skip_type_member_at(pos)?;

        loop {
            let current_type = self.peek_nth(pos).token_type.clone();
            let next_type = self.peek_nth(pos + 1).token_type.clone();
            match current_type {
                TokenType::Gt if next_type == TokenType::DoublePipe => {
                    pos += 1;
                }
                TokenType::Gt | TokenType::RShift => return Some(pos + 1),
                TokenType::DoublePipe => {
                    pos += 1;
                    if self.peek_nth(pos).token_type == TokenType::Lt {
                        pos += 1;
                    }
                    pos = self.skip_type_member_at(pos)?;
                }
                _ => return None,
            }
        }
    }

    fn is_typed_declaration(&mut self) -> bool {
        if let Some(after) = self.skip_type_annotation(0) {
            let t = self.peek_nth(after).token_type.clone();
            t == TokenType::Assign || t == TokenType::Walrus
        } else {
            false
        }
    }

    fn is_typed_declare_shorthand(&mut self) -> bool {
        if let Some(after) = self.skip_type_annotation(0) {
            let t = self.peek_nth(after).token_type.clone();
            t != TokenType::Assign && t != TokenType::Walrus
        } else {
            false
        }
    }

    fn is_as_typed_declare(&mut self) -> bool {
        self.skip_type_annotation(1).is_some()
    }

    fn register_prefix(&mut self, tt: TokenType, f: PrefixParseFn) {
        self.prefix_fns.insert(tt, f);
    }

    fn register_infix(&mut self, tt: TokenType, f: InfixParseFn) {
        self.infix_fns.insert(tt, f);
    }

    fn expect_peek(&mut self, tt: TokenType) -> Option<()> {
        if self.peek_token.token_type == tt {
            self.next_token();
            Some(())
        } else {
            let suggestion = Self::suggest_for_expected(&tt, &self.peek_token);
            self.errors.push(Diagnostic {
                span: self.peek_token.span,
                message: format!(
                    "expected {:?}, got {:?} ({:?})",
                    tt, self.peek_token.token_type, self.peek_token.literal
                ),
                suggestion,
                severity: Severity::Error,
            });
            None
        }
    }

    fn suggest_for_expected(expected: &TokenType, got: &Token) -> Option<String> {
        match expected {
            TokenType::RBrace => Some("you may be missing a closing `}`".to_string()),
            TokenType::RParen => Some("you may be missing a closing `)`".to_string()),
            TokenType::RBracket => Some("you may be missing a closing `]`".to_string()),
            TokenType::LBrace => Some("expected a `{` to open a block".to_string()),
            TokenType::LParen => Some("expected `(` for function parameters".to_string()),
            TokenType::In => Some("did you mean `each <var> in <iterable>`?".to_string()),
            TokenType::When => Some("did you mean `repeat when <condition>`?".to_string()),
            TokenType::Arrow => Some("expected `->` to separate pattern from body".to_string()),
            TokenType::Then => Some("did you mean `unless <cond> then <fallback>`?".to_string()),
            TokenType::Lt => {
                // Expected '<' for a type annotation but got something else
                match got.token_type {
                    TokenType::Contains
                    | TokenType::Function
                    | TokenType::Struct
                    | TokenType::Each
                    | TokenType::Repeat
                    | TokenType::Introduce => {
                        Some("you may be missing a closing `}` on a previous block".to_string())
                    }
                    TokenType::Assign => {
                        Some("use `<type>` for type annotations, e.g. `name <str>`".to_string())
                    }
                    _ => Some("expected `<type>` annotation, e.g. `<str>`, `<int>`".to_string()),
                }
            }
            TokenType::Gt => {
                Some("you may be missing a closing `>` on a type annotation".to_string())
            }
            TokenType::Colon => {
                if got.token_type == TokenType::Assign {
                    Some("use `:` not `=` to separate key from value".to_string())
                } else {
                    None
                }
            }
            TokenType::Walrus | TokenType::Assign => {
                if got.token_type == TokenType::Assign {
                    Some("use `:=` for initial assignment or `=` for reassignment".to_string())
                } else {
                    None
                }
            }
            TokenType::Contains => Some("did you mean `StructName contains { ... }`?".to_string()),
            TokenType::From => Some("did you mean `introduce {name} from module`?".to_string()),
            _ => None,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        precedence_of(&self.peek_token.token_type)
    }

    fn curr_precedence(&self) -> Precedence {
        precedence_of(&self.curr_token.token_type)
    }
}

#[cfg(test)]
mod tests;
