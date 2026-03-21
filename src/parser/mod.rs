use crate::ast::{ChooseArm, Expression, Identifier, OptionArm, Program, Statement, TypeAnnotation, TypedParam};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;
use std::collections::VecDeque;

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
        FullStop => Index,
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
    lookahead_buffer: VecDeque<Token>,
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
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Shebang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Not, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Function, Parser::parse_function_expression);
        p.register_prefix(TokenType::LBrace, Parser::parse_map_literal);
        p.register_prefix(TokenType::OptionKw, Parser::parse_option_expression);
        p.register_prefix(TokenType::SelfKw, Parser::parse_self_expression);

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

        // Dot access
        p.register_infix(TokenType::FullStop, Parser::parse_dot_expression);


        p.next_token();
        p.next_token();
        p
    }

    pub fn error(&self) -> &[String] {
        &self.errors
    }

    fn is_type_name_token(tt: &TokenType) -> bool {
        matches!(tt, TokenType::Ident | TokenType::None)
    }

    /// Parses a type annotation after `<` has already been consumed.
    /// Expects curr_token to be `<`. Consumes through closing `>` and any `|| <type>` continuations.
    fn parse_type_annotation(&mut self) -> Option<TypeAnnotation> {
        // curr_token is '<', consume type name (Ident or None keyword)
        if !Self::is_type_name_token(&self.peek_token.token_type) {
            self.errors.push(format!(
                "expected type name, got {:?} (literal={:?})",
                self.peek_token.token_type, self.peek_token.literal
            ));
            return None;
        }
        self.next_token();
        let first = TypeAnnotation::from_str_or_struct(&self.curr_token.literal);
        self.expect_peek(TokenType::Gt)?; // '>'

        // Check for || <type> continuations
        if self.peek_token.token_type == TokenType::DoublePipe {
            let mut types = vec![first];
            while self.peek_token.token_type == TokenType::DoublePipe {
                self.next_token(); // consume '||'
                self.expect_peek(TokenType::Lt)?; // '<'
                if !Self::is_type_name_token(&self.peek_token.token_type) {
                    self.errors.push(format!(
                        "expected type name, got {:?} (literal={:?})",
                        self.peek_token.token_type, self.peek_token.literal
                    ));
                    return None;
                }
                self.next_token();
                types.push(TypeAnnotation::from_str_or_struct(&self.curr_token.literal));
                self.expect_peek(TokenType::Gt)?; // '>'
            }
            Some(TypeAnnotation::Union(types))
        } else {
            Some(first)
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

            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
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
        let expr = self.parse_expression(Precedence::Lowest)?;

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

        if self.peek_token.token_type == TokenType::RParen {
            self.next_token();
            return Some(params);
        }

        self.next_token();
        let ident = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };
        let type_ann = if self.peek_token.token_type == TokenType::Lt {
            self.next_token(); // move to '<'
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        params.push(TypedParam { ident, type_ann });

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token(); // ','
            self.next_token(); // next param
            let ident = Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            };
            let type_ann = if self.peek_token.token_type == TokenType::Lt {
                self.next_token(); // move to '<'
                Some(self.parse_type_annotation()?)
            } else {
                None
            };
            params.push(TypedParam { ident, type_ann });
        }

        self.expect_peek(TokenType::RParen)?;
        Some(params)
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
            Expression::Prefix {
                token: Token { token_type: TokenType::Not, literal: "not".into() },
                operator: "not".to_string(),
                right: Box::new(raw_condition),
            }
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
        })
    }

    fn parse_unless_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone(); // 'unless'

        self.next_token(); // move to condition
        let condition = self.parse_expression(Precedence::Lowest)?;

        // Negate the condition
        let negated = Expression::Prefix {
            token: Token {
                token_type: TokenType::Shebang,
                literal: "!".to_string(),
            },
            operator: "!".to_string(),
            right: Box::new(condition),
        };

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
            let negated = Expression::Prefix {
                token: Token {
                    token_type: TokenType::Shebang,
                    literal: "!".to_string(),
                },
                operator: "!".to_string(),
                right: Box::new(condition),
            };
            return Some(Statement::If {
                token: Token {
                    token_type: TokenType::If,
                    literal: "if".to_string(),
                },
                condition: negated,
                consequence: vec![stmt],
                alternative: None,
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
                self.errors.push(format!(
                    "expected 'fun' inside contains block, got {:?}",
                    self.curr_token.token_type
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

    /// Scans past a type annotation starting at peek_nth(offset).
    /// Expects: Lt Ident Gt [|| Lt Ident Gt]*
    /// Returns the offset of the token AFTER the full type annotation.
    fn skip_type_annotation(&mut self, start: usize) -> Option<usize> {
        if self.peek_nth(start).token_type != TokenType::Lt {
            return None;
        }
        if !Self::is_type_name_token(&self.peek_nth(start + 1).token_type) {
            return None;
        }
        if self.peek_nth(start + 2).token_type != TokenType::Gt {
            return None;
        }
        let mut pos = start + 3;
        while self.peek_nth(pos).token_type == TokenType::DoublePipe {
            if self.peek_nth(pos + 1).token_type != TokenType::Lt
                || !Self::is_type_name_token(&self.peek_nth(pos + 2).token_type)
                || self.peek_nth(pos + 3).token_type != TokenType::Gt
            {
                break;
            }
            pos += 4;
        }
        Some(pos)
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
    fn test_parse_option_expression() {
        let program = parse_ok("option { x > 0 -> 1, 2 }");
        match &program.statements[0] {
            Statement::Expr(Expression::Option { arms, default, .. }) => {
                assert_eq!(arms.len(), 1);
                match &arms[0].condition {
                    Expression::Infix { operator, .. } => assert_eq!(operator, ">"),
                    _ => panic!("Expected infix condition"),
                }
                assert_eq!(arms[0].body.len(), 1);
                assert!(default.is_some());
            }
            other => panic!("Expected option expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_unless_statement() {
        let program = parse_ok("unless valid { 42 }");
        match &program.statements[0] {
            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                // condition should be negated (prefix !)
                match condition {
                    Expression::Prefix { operator, .. } => assert_eq!(operator, "!"),
                    _ => panic!("Expected negated condition"),
                }
                assert_eq!(consequence.len(), 1);
                assert!(alternative.is_none());
            }
            other => panic!("Expected if statement (desugared unless), got {:?}", other),
        }
    }

    #[test]
    fn test_parse_postfix_when_guard() {
        let program = parse_ok("x = 5 when True");
        match &program.statements[0] {
            Statement::If {
                consequence,
                alternative,
                ..
            } => {
                assert_eq!(consequence.len(), 1);
                assert!(matches!(&consequence[0], Statement::Assign { .. }));
                assert!(alternative.is_none());
            }
            other => panic!("Expected if statement (desugared when guard), got {:?}", other),
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

    // ==================== TYPED DECLARATION TESTS ====================

    #[test]
    fn test_parse_typed_let_strict() {
        let program = parse_ok("x <int> = 10");
        match &program.statements[0] {
            Statement::TypedLet { name, type_ann, walrus, .. } => {
                assert_eq!(name.value, "x");
                assert_eq!(*type_ann, TypeAnnotation::Int);
                assert!(!walrus);
            }
            other => panic!("Expected TypedLet, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_typed_let_walrus() {
        let program = parse_ok("x <str> := \"hi\"");
        match &program.statements[0] {
            Statement::TypedLet { name, type_ann, walrus, .. } => {
                assert_eq!(name.value, "x");
                assert_eq!(*type_ann, TypeAnnotation::Str);
                assert!(walrus);
            }
            other => panic!("Expected TypedLet, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_typed_declare() {
        let program = parse_ok("x as <float>");
        match &program.statements[0] {
            Statement::TypedDeclare { name, type_ann } => {
                assert_eq!(name.value, "x");
                assert_eq!(*type_ann, TypeAnnotation::Float);
            }
            other => panic!("Expected TypedDeclare, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_assign() {
        let program = parse_ok("x = 20");
        match &program.statements[0] {
            Statement::Assign { name, .. } => {
                assert_eq!(name.value, "x");
            }
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_less_than_not_typed() {
        // x < 10 should still parse as infix comparison, not typed declaration
        let program = parse_ok("x < 10");
        match &program.statements[0] {
            Statement::Expr(Expression::Infix { operator, .. }) => {
                assert_eq!(operator, "<");
            }
            other => panic!("Expected infix expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_struct_type_annotation() {
        // Unknown type names are now parsed as struct types (validated at eval time)
        let program = parse_ok("x <Person> = 1");
        match &program.statements[0] {
            Statement::TypedLet { name, type_ann, .. } => {
                assert_eq!(name.value, "x");
                assert_eq!(*type_ann, TypeAnnotation::Struct("Person".to_string()));
            }
            other => panic!("Expected TypedLet, got {:?}", other),
        }
    }

    // ==================== FUNCTION PARSING TESTS ====================

    #[test]
    fn test_parse_anonymous_function() {
        let program = parse_ok("fun(a, b) { a + b }");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expr(Expression::FunctionLiteral { parameters, body, .. }) => {
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].ident.value, "a");
                assert_eq!(parameters[1].ident.value, "b");
                assert_eq!(body.len(), 1);
            }
            other => panic!("Expected FunctionLiteral expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_named_function() {
        let program = parse_ok("fun add(a, b) { a + b }");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Let { name, value } => {
                assert_eq!(name.value, "add");
                match value {
                    Expression::FunctionLiteral { parameters, body, .. } => {
                        assert_eq!(parameters.len(), 2);
                        assert_eq!(parameters[0].ident.value, "a");
                        assert_eq!(parameters[1].ident.value, "b");
                        assert_eq!(body.len(), 1);
                    }
                    other => panic!("Expected FunctionLiteral value, got {:?}", other),
                }
            }
            other => panic!("Expected Let statement, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_zero_param_function() {
        let program = parse_ok("fun() { 42 }");
        match &program.statements[0] {
            Statement::Expr(Expression::FunctionLiteral { parameters, .. }) => {
                assert_eq!(parameters.len(), 0);
            }
            other => panic!("Expected FunctionLiteral, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_give_statement() {
        let program = parse_ok("give 42");
        match &program.statements[0] {
            Statement::Give { value, .. } => {
                match value {
                    Expression::Int { value, .. } => assert_eq!(*value, 42),
                    other => panic!("Expected Int, got {:?}", other),
                }
            }
            other => panic!("Expected Give statement, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_let_with_function_value() {
        let program = parse_ok("f := fun(x) { x }");
        match &program.statements[0] {
            Statement::Let { name, value } => {
                assert_eq!(name.value, "f");
                match value {
                    Expression::FunctionLiteral { parameters, .. } => {
                        assert_eq!(parameters.len(), 1);
                        assert_eq!(parameters[0].ident.value, "x");
                    }
                    other => panic!("Expected FunctionLiteral, got {:?}", other),
                }
            }
            other => panic!("Expected Let statement, got {:?}", other),
        }
    }

    // ==================== ERROR HANDLING TESTS ====================

    #[test]
    fn test_parser_errors() {
        let tests = vec![
            ("x :=", "no prefix parse fn"),          // Missing value after :=
            ("each x in { }", "expected next token"), // {} parses as empty map, then block '{' is missing
        ];

        for (input, expected_error) in tests {
            let (_, errors) = parse(input);
            assert!(!errors.is_empty(), "Expected errors for '{}'", input);
            assert!(
                errors
                    .iter()
                    .any(|e| e.to_lowercase().contains(expected_error)),
                "Expected error containing '{}' for '{}', got {:?}",
                expected_error,
                input,
                errors
            );
        }
    }

    // ==================== STRUCT PARSING TESTS ====================

    #[test]
    fn test_parse_struct_definition() {
        let program = parse_ok("struct Person {\n    name <str>\n    age <int>\n}");
        match &program.statements[0] {
            Statement::StructDef { name, parent, fields, .. } => {
                assert_eq!(name.value, "Person");
                assert!(parent.is_none());
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name.value, "name");
                assert_eq!(fields[0].type_ann, TypeAnnotation::Str);
                assert_eq!(fields[1].name.value, "age");
                assert_eq!(fields[1].type_ann, TypeAnnotation::Int);
            }
            other => panic!("Expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_struct_with_parent() {
        let program = parse_ok("struct American(Person) {\n    nationality <str>\n}");
        match &program.statements[0] {
            Statement::StructDef { name, parent, fields, .. } => {
                assert_eq!(name.value, "American");
                assert!(parent.is_some());
                assert_eq!(parent.as_ref().unwrap().value, "Person");
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name.value, "nationality");
            }
            other => panic!("Expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_contains_def() {
        let program = parse_ok("Person contains {\n    fun greet() { 42 }\n}");
        match &program.statements[0] {
            Statement::ContainsDef { struct_name, methods, .. } => {
                assert_eq!(struct_name.value, "Person");
                assert_eq!(methods.len(), 1);
                assert_eq!(methods[0].0.value, "greet");
            }
            other => panic!("Expected ContainsDef, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_dot_access() {
        let program = parse_ok("p.name");
        match &program.statements[0] {
            Statement::Expr(Expression::DotAccess { left, field, .. }) => {
                match left.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.value, "p"),
                    _ => panic!("Expected Ident as left"),
                }
                assert_eq!(field.value, "name");
            }
            other => panic!("Expected DotAccess expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_dot_assign() {
        let program = parse_ok("p.name = \"Jane\"");
        match &program.statements[0] {
            Statement::DotAssign { object, field, value, .. } => {
                match object {
                    Expression::Ident(ident) => assert_eq!(ident.value, "p"),
                    _ => panic!("Expected Ident as object"),
                }
                assert_eq!(field.value, "name");
                match value {
                    Expression::Str { value, .. } => assert_eq!(value, "Jane"),
                    _ => panic!("Expected string value"),
                }
            }
            other => panic!("Expected DotAssign, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_struct_literal() {
        let program = parse_ok("Person { name: \"Alice\", age: 30 }");
        match &program.statements[0] {
            Statement::Expr(Expression::StructLiteral { struct_name, field_values, .. }) => {
                assert_eq!(struct_name, "Person");
                assert_eq!(field_values.len(), 2);
                assert_eq!(field_values[0].0, "name");
                assert_eq!(field_values[1].0, "age");
            }
            other => panic!("Expected StructLiteral expression, got {:?}", other),
        }
    }

    // ==================== TUPLE AND MAP PARSING TESTS ====================

    #[test]
    fn test_parse_tuple_literal() {
        let program = parse_ok("(1, 2, 3)");
        match &program.statements[0] {
            Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
                assert_eq!(elements.len(), 3);
            }
            other => panic!("Expected TupleLiteral, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_empty_tuple() {
        let program = parse_ok("()");
        match &program.statements[0] {
            Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
                assert_eq!(elements.len(), 0);
            }
            other => panic!("Expected empty TupleLiteral, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_single_tuple() {
        let program = parse_ok("(1,)");
        match &program.statements[0] {
            Statement::Expr(Expression::TupleLiteral { elements, .. }) => {
                assert_eq!(elements.len(), 1);
            }
            other => panic!("Expected single-element TupleLiteral, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_grouped_not_tuple() {
        let program = parse_ok("(1 + 2)");
        match &program.statements[0] {
            Statement::Expr(Expression::Grouped(_)) => {}
            other => panic!("Expected Grouped expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_map_literal() {
        let program = parse_ok("{\"a\": 1, \"b\": 2}");
        match &program.statements[0] {
            Statement::Expr(Expression::MapLiteral { entries, .. }) => {
                assert_eq!(entries.len(), 2);
            }
            other => panic!("Expected MapLiteral, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_empty_map() {
        let program = parse_ok("{}");
        match &program.statements[0] {
            Statement::Expr(Expression::MapLiteral { entries, .. }) => {
                assert_eq!(entries.len(), 0);
            }
            other => panic!("Expected empty MapLiteral, got {:?}", other),
        }
    }

    // ==================== HIDE / SELF / TYPED PARAM PARSER TESTS ====================

    #[test]
    fn test_parse_hide_struct_field() {
        let program = parse_ok("struct Person {\n    hide name <str>\n    age <int>\n}");
        match &program.statements[0] {
            Statement::StructDef { fields, .. } => {
                assert_eq!(fields.len(), 2);
                assert!(fields[0].hidden);
                assert_eq!(fields[0].name.value, "name");
                assert!(!fields[1].hidden);
                assert_eq!(fields[1].name.value, "age");
            }
            other => panic!("Expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_self_dot_access() {
        let program = parse_ok("self.field");
        match &program.statements[0] {
            Statement::Expr(Expression::DotAccess { left, field, .. }) => {
                match left.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.value, "self"),
                    _ => panic!("Expected Ident('self') as left"),
                }
                assert_eq!(field.value, "field");
            }
            other => panic!("Expected DotAccess expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_typed_function_parameters() {
        let program = parse_ok("fun f(x <int>, y) { x }");
        match &program.statements[0] {
            Statement::Let { value, .. } => {
                match value {
                    Expression::FunctionLiteral { parameters, .. } => {
                        assert_eq!(parameters.len(), 2);
                        assert_eq!(parameters[0].ident.value, "x");
                        assert_eq!(parameters[0].type_ann, Some(TypeAnnotation::Int));
                        assert_eq!(parameters[1].ident.value, "y");
                        assert_eq!(parameters[1].type_ann, None);
                    }
                    other => panic!("Expected FunctionLiteral, got {:?}", other),
                }
            }
            other => panic!("Expected Let, got {:?}", other),
        }
    }
}
