use crate::ast::{Expression, Identifier, Program, Statement};
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
        _ => Lowest,
    }
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_fns: HashMap<TokenType, PrefixParseFn>,
    infix_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
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

        for tt in [TokenType::Plus, TokenType::Minus] {
            p.register_infix(tt, Parser::parse_infix_expression);
        }

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
        //bind name := expr
        if self.curr_token.token_type == TokenType::Ident
            && self.peek_token.token_type == TokenType::Walrus
        {
            return self.parse_let_statement();
        }
        self.parse_expression_statement()
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name_token = self.curr_token.clone();
        let name = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        // consume ':='
        self.expect_peek(TokenType::Assign)?;

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

    fn parse_string(&mut self) -> Option<Expression> {
        let tok = self.curr_token.clone();
        Some(Expression::Str {
            token: tok.clone(),
            value: tok.literal,
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
