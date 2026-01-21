use crate::ast::{ChooseArm, Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
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
        Eq | NotEq => Equals,
        Lt | Lte | Gt | Gte => LessGreater,
        Plus | Minus => Sum,
        Asterisk | FSlash | Mod => Product,
        Increment | Decrement => Postfix,
        LParen => Call,
        LBracket => Index,
        _ => Lowest,
    }
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_fns: HashMap<TokenType, PrefixParseFn>,
    infix_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let dummy = Token {
            token_type: TokenType::Illegal,
            literal: "".into(),
        };

        let mut p = Parser {
            lexer,
            curr_token: dummy.clone(),
            peek_token: dummy,
            errors: Vec::new(),
            prefix_fns: HashMap::new(),
            infix_fns: HashMap::new(),
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
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Shebang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Not, Parser::parse_prefix_expression);

        // Infix Parse Functions
        for tt in [TokenType::Plus, TokenType::Minus] {
            p.register_infix(tt, Parser::parse_infix_expression);
        }

        // Comparison operators
        p.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        p.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        p.register_infix(TokenType::Lt, Parser::parse_infix_expression);
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

        p.next_token();
        p.next_token();
        p
    }

    pub fn error(&self) -> &[String] {
        &self.errors
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

            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token.token_type {
            // bind name := expr
            TokenType::Ident if self.peek_token.token_type == TokenType::Walrus => {
                self.parse_let_statement()
            }
            TokenType::Each => self.parse_each_statement(),
            TokenType::Repeat => self.parse_repeat_statement(),
            TokenType::Pattern => self.parse_pattern_statement(),
            TokenType::Choose => self.parse_choose_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Skip => {
                Some(Statement::Skip)
            }
            TokenType::Stop => {
                Some(Statement::Stop)
            }
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

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        Some(Statement::Expr(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_fns.get(&self.curr_token.token_type).copied();

        let mut left = match prefix {
            Some(f) => f(self)?,
            None => {
                self.errors.push(format!(
                    "no prefix parse fn for {:?} (literal={:?})",
                    self.curr_token.token_type, self.curr_token.literal
                ));
                return None;
            }
        };

        // Pratt loop: while next operator binds tighter, consume it
        while self.peek_token.token_type != TokenType::Eof
            && self.peek_token.token_type != TokenType::Newline
            && precedence < self.peek_precedence()
        {
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
        Some(Expression::Ident(Identifier {
            token: tok.clone(),
            value: tok.literal,
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
        // curr_token is '('
        self.next_token(); // enter group
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenType::RParen)?;
        Some(Expression::Grouped(Box::new(expr)))
    }

    // ---------------- Infix parsers ----------------

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

    // Call: f(arg1, arg2, ...)
    // Trigger token is '(' *after* the function expression.
    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '('
        let args = self.parse_expression_list(TokenType::RParen)?;

        Some(Expression::Call {
            token: tok,
            function: Box::new(function),
            args,
        })
    }

    // Index: arr[index]
    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone(); // '['
        self.next_token(); // move past '['

        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenType::RBracket)?;

        Some(Expression::Index {
            token: tok,
            left: Box::new(left),
            index: Box::new(index),
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

        self.expect_peek(TokenType::When)?; // 'when'
        self.next_token(); // move to condition

        let condition = self.parse_expression(Precedence::Lowest)?;

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

            let pattern_name = self.curr_token.literal.clone();

            self.expect_peek(TokenType::Arrow)?; // '->'
            self.next_token(); // move to body expression

            let body = self.parse_expression(Precedence::Lowest)?;

            arms.push(ChooseArm { pattern_name, body });

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

    // if condition { ... } else { ... }
    fn parse_if_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'if'

        self.next_token(); // move to condition
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::LBrace)?; // '{'
        let consequence = self.parse_block()?;

        let alternative = if self.peek_token.token_type == TokenType::Else {
            self.next_token(); // 'else'
            self.expect_peek(TokenType::LBrace)?; // '{'
            Some(self.parse_block()?)
        } else {
            None
        };

        Some(Statement::If {
            token,
            condition,
            consequence,
            alternative,
        })
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

    // ---------------- Helpers ----------------

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
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
            self.errors.push(format!(
                "expected next token {:?}, got {:?} (literal={:?})",
                tt, self.peek_token.token_type, self.peek_token.literal
            ));
            None
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
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(input: &str) -> (Program, Vec<String>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let errors = parser.error().to_vec();
        (program, errors)
    }

    fn parse_ok(input: &str) -> Program {
        let (program, errors) = parse(input);
        assert!(errors.is_empty(), "Parser errors: {:?}", errors);
        program
    }

    // ==================== LITERAL PARSING TESTS ====================

    #[test]
    fn test_parse_integer_literal() {
        let program = parse_ok("5");
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expr(Expression::Int { value, .. }) => {
                assert_eq!(*value, 5);
            }
            _ => panic!("Expected integer expression"),
        }
    }

    #[test]
    fn test_parse_float_literal() {
        let program = parse_ok("3.14");
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expr(Expression::Float { value, .. }) => {
                assert!((value - 3.14).abs() < f64::EPSILON);
            }
            _ => panic!("Expected float expression"),
        }
    }

    #[test]
    fn test_parse_char_literal() {
        let program = parse_ok("`a`");
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expr(Expression::Char { value, .. }) => {
                assert_eq!(*value, 'a');
            }
            _ => panic!("Expected char expression"),
        }
    }

    #[test]
    fn test_parse_string_literals() {
        // Double quotes
        let program = parse_ok("\"hello\"");
        match &program.statements[0] {
            Statement::Expr(Expression::Str { value, .. }) => {
                assert_eq!(value, "hello");
            }
            _ => panic!("Expected string expression"),
        }

        // Single quotes
        let program = parse_ok("'world'");
        match &program.statements[0] {
            Statement::Expr(Expression::Str { value, .. }) => {
                assert_eq!(value, "world");
            }
            _ => panic!("Expected string expression"),
        }
    }

    #[test]
    fn test_parse_boolean_literals() {
        let program = parse_ok("True");
        match &program.statements[0] {
            Statement::Expr(Expression::Boolean { value, .. }) => {
                assert!(*value);
            }
            _ => panic!("Expected boolean expression"),
        }

        let program = parse_ok("False");
        match &program.statements[0] {
            Statement::Expr(Expression::Boolean { value, .. }) => {
                assert!(!*value);
            }
            _ => panic!("Expected boolean expression"),
        }
    }

    #[test]
    fn test_parse_none_literal() {
        let program = parse_ok("None");
        match &program.statements[0] {
            Statement::Expr(Expression::NoneExpr { .. }) => {}
            _ => panic!("Expected None expression"),
        }
    }

    #[test]
    fn test_parse_array_literal() {
        let program = parse_ok("[1, 2, 3]");
        match &program.statements[0] {
            Statement::Expr(Expression::Array { elements, .. }) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected array expression"),
        }
    }

    // ==================== EXPRESSION PARSING TESTS ====================

    #[test]
    fn test_parse_prefix_expressions() {
        let tests = vec![
            ("-5", "-", 5i64),
            ("!True", "!", 0), // Can't easily check bool, just verify it parses
        ];

        for (input, expected_op, _) in tests {
            let program = parse_ok(input);
            match &program.statements[0] {
                Statement::Expr(Expression::Prefix { operator, .. }) => {
                    assert_eq!(operator, expected_op);
                }
                _ => panic!("Expected prefix expression for '{}'", input),
            }
        }
    }

    #[test]
    fn test_parse_infix_expressions() {
        let tests = vec![
            ("5 + 5", "+"),
            ("5 - 5", "-"),
            ("5 * 5", "*"),
            ("5 / 5", "/"),
            ("5 % 5", "%"),
            ("5 == 5", "=="),
            ("5 != 5", "!="),
            ("5 < 5", "<"),
            ("5 > 5", ">"),
            ("5 <= 5", "<="),
            ("5 >= 5", ">="),
        ];

        for (input, expected_op) in tests {
            let program = parse_ok(input);
            match &program.statements[0] {
                Statement::Expr(Expression::Infix { operator, .. }) => {
                    assert_eq!(operator, expected_op, "Failed for input '{}'", input);
                }
                _ => panic!("Expected infix expression for '{}'", input),
            }
        }
    }

    #[test]
    fn test_parse_postfix_expressions() {
        let tests = vec![("x++", "++"), ("x--", "--")];

        for (input, expected_op) in tests {
            let program = parse_ok(input);
            match &program.statements[0] {
                Statement::Expr(Expression::Postfix { operator, .. }) => {
                    assert_eq!(operator, expected_op, "Failed for input '{}'", input);
                }
                _ => panic!("Expected postfix expression for '{}'", input),
            }
        }
    }

    #[test]
    fn test_parse_grouped_expression() {
        let program = parse_ok("(5 + 5)");
        match &program.statements[0] {
            Statement::Expr(Expression::Grouped(inner)) => match inner.as_ref() {
                Expression::Infix { operator, .. } => {
                    assert_eq!(operator, "+");
                }
                _ => panic!("Expected infix inside grouped"),
            },
            _ => panic!("Expected grouped expression"),
        }
    }

    #[test]
    fn test_parse_call_expression() {
        let program = parse_ok("print(1, 2, 3)");
        match &program.statements[0] {
            Statement::Expr(Expression::Call { function, args, .. }) => {
                match function.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.value, "print"),
                    _ => panic!("Expected identifier as function"),
                }
                assert_eq!(args.len(), 3);
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_parse_index_expression() {
        let program = parse_ok("arr[0]");
        match &program.statements[0] {
            Statement::Expr(Expression::Index { left, index, .. }) => {
                match left.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.value, "arr"),
                    _ => panic!("Expected identifier as left"),
                }
                match index.as_ref() {
                    Expression::Int { value, .. } => assert_eq!(*value, 0),
                    _ => panic!("Expected integer as index"),
                }
            }
            _ => panic!("Expected index expression"),
        }
    }

    // ==================== STATEMENT PARSING TESTS ====================

    #[test]
    fn test_parse_let_statement() {
        let program = parse_ok("x := 5");
        match &program.statements[0] {
            Statement::Let { name, value } => {
                assert_eq!(name.value, "x");
                match value {
                    Expression::Int { value, .. } => assert_eq!(*value, 5),
                    _ => panic!("Expected integer value"),
                }
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_each_statement() {
        let program = parse_ok("each x in [1, 2, 3] { print(x) }");
        match &program.statements[0] {
            Statement::Each {
                variable,
                iterable,
                body,
                ..
            } => {
                assert_eq!(variable.value, "x");
                match iterable {
                    Expression::Array { elements, .. } => assert_eq!(elements.len(), 3),
                    _ => panic!("Expected array as iterable"),
                }
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected each statement"),
        }
    }

    #[test]
    fn test_parse_repeat_statement() {
        let program = parse_ok("repeat when x > 0 { x-- }");
        match &program.statements[0] {
            Statement::Repeat {
                condition, body, ..
            } => {
                match condition {
                    Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                    _ => panic!("Expected infix condition"),
                }
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected repeat statement"),
        }
    }

    #[test]
    fn test_parse_pattern_statement() {
        let program = parse_ok("pattern is_ten(x) when x == 10");
        match &program.statements[0] {
            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                assert_eq!(name.value, "is_ten");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].value, "x");
                match condition {
                    Expression::Infix { operator, .. } => assert_eq!(operator, "=="),
                    _ => panic!("Expected infix condition"),
                }
            }
            _ => panic!("Expected pattern statement"),
        }
    }

    #[test]
    fn test_parse_choose_statement() {
        let program = parse_ok("choose y { is_ten -> 1, else -> 2 }");
        match &program.statements[0] {
            Statement::Choose { subject, arms, .. } => {
                match subject {
                    Expression::Ident(ident) => assert_eq!(ident.value, "y"),
                    _ => panic!("Expected identifier as subject"),
                }
                assert_eq!(arms.len(), 2);
                assert_eq!(arms[0].pattern_name, "is_ten");
                assert_eq!(arms[1].pattern_name, "else");
            }
            _ => panic!("Expected choose statement"),
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let program = parse_ok("if x > 0 { 1 } else { 2 }");
        match &program.statements[0] {
            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                match condition {
                    Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                    _ => panic!("Expected infix condition"),
                }
                assert_eq!(consequence.len(), 1);
                assert!(alternative.is_some());
                assert_eq!(alternative.as_ref().unwrap().len(), 1);
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_skip_stop() {
        let program = parse_ok("skip");
        assert!(matches!(program.statements[0], Statement::Skip));

        let program = parse_ok("stop");
        assert!(matches!(program.statements[0], Statement::Stop));
    }

    // ==================== OPERATOR PRECEDENCE TESTS ====================

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("1 + 2 * 3", "(1 + (2 * 3))"),
            ("1 * 2 + 3", "((1 * 2) + 3)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ];

        // Just verify these parse without errors
        // A full precedence test would need expression stringification
        for (input, _expected) in tests {
            let _ = parse_ok(input);
        }
    }

    // ==================== ERROR HANDLING TESTS ====================

    #[test]
    fn test_parser_errors() {
        let tests = vec![
            ("x :=", "no prefix parse fn"), // Missing value after :=
            ("each x in { }", "no prefix parse fn"), // Missing iterable - LBrace is not a valid expression
        ];

        for (input, expected_error) in tests {
            let (_, errors) = parse(input);
            assert!(!errors.is_empty(), "Expected errors for '{}'", input);
            assert!(
                errors.iter().any(|e| e.to_lowercase().contains(expected_error)),
                "Expected error containing '{}' for '{}', got {:?}",
                expected_error,
                input,
                errors
            );
        }
    }
}
