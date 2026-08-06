use crate::ast::*;
use crate::lexer::Comment;
use crate::token::Span;
use crate::token::TokenType;

pub struct Formatter {
    indent: usize,
    output: String,
    /// Comments from the source, in line order, and how far we have replayed
    /// them. The AST has no comment nodes, so they are re-inserted by comparing
    /// each comment's source line against the line each statement came from.
    comments: Vec<Comment>,
    next_comment: usize,
    /// `#[indent]` files delimit blocks with a trailing `:` and indentation
    /// rather than braces; emitting braces into one produces a file that no
    /// longer parses.
    indent_style: bool,
    /// Where the next sibling of the statement/item that opened the current
    /// block starts. A comment on or after that line, or indented no further
    /// than it, was written for that sibling rather than inside the block.
    next_anchor: Option<Span>,
    /// Source column of the last statement emitted in the current block, and of
    /// the statement that opened it. A comment belongs to the block when it is
    /// indented at least as far as the block's statements *and* strictly past
    /// the opener — the first test keeps it out of a sibling's block, the second
    /// keeps an unindented comment at the opener's own margin outside entirely.
    body_column: usize,
    open_column: usize,
    /// Set when the next statement is a `hide fun` declaration, so the keyword
    /// is re-emitted ahead of it.
    hide_next_fn: bool,
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            output: String::new(),
            comments: Vec::new(),
            next_comment: 0,
            indent_style: false,
            next_anchor: None,
            body_column: usize::MAX,
            open_column: 0,
            hide_next_fn: false,
        }
    }

    pub fn format(program: &Program) -> String {
        Formatter::new().run(program)
    }

    /// Format `program`, restoring `comments` and matching the source's block
    /// style. `indent_style` must be true when the source carries `#[indent]`.
    pub fn format_source(program: &Program, comments: &[Comment], indent_style: bool) -> String {
        let mut f = Formatter::new();
        f.comments = comments.to_vec();
        f.indent_style = indent_style;
        f.run(program)
    }

    fn run(mut self, program: &Program) -> String {
        self.format_program(program);
        // Ensure file ends with exactly one newline
        let result = self.output.trim_end().to_string();
        if result.is_empty() {
            String::new()
        } else {
            result + "\n"
        }
    }

    /// Opens a statement block. In brace style this is `<lead>{`; in indent
    /// style it is `<lead>:` with the body's indentation doing the delimiting —
    /// the exact inverse of the lexer's colon-at-end-of-line to `{` rule.
    fn open_block(&mut self, lead: &str) {
        // Until a statement is emitted in it, the new block admits no comments.
        self.body_column = usize::MAX;
        if self.indent_style {
            self.push(lead.trim_end());
            self.push(":");
        } else {
            self.push(lead);
            self.push("{");
        }
        self.newline();
        self.indent += 1;
    }

    /// Closes a block opened by `open_block`, leaving the cursor at the end of
    /// the block's last line so callers can append or newline uniformly.
    fn close_block(&mut self) {
        self.flush_comments_inside_block();
        self.indent -= 1;
        if self.indent_style {
            // Dedent closes the block, so drop the body's trailing newline.
            while self.output.ends_with('\n') {
                self.output.pop();
            }
        } else {
            self.push_indent();
            self.push("}");
        }
    }

    /// `open_block` + statements + `close_block`. An empty body has no
    /// indentable content, so indent style needs an explicit `none` placeholder
    /// where brace style can just write `{}`.
    fn block(&mut self, lead: &str, body: &[Statement]) {
        self.open_block(lead);
        if self.indent_style && body.is_empty() {
            self.push_indent();
            self.push("none");
            self.newline();
        }
        self.format_block(body);
        self.close_block();
    }

    fn current_column(&self) -> usize {
        match self.output.rfind('\n') {
            Some(i) => self.output.len() - i - 1,
            None => self.output.len(),
        }
    }

    fn array_inline_width(&self, elements: &[Expression]) -> usize {
        let mut f = Formatter::new();
        f.push("[");
        for (i, elem) in elements.iter().enumerate() {
            if i > 0 {
                f.push(", ");
            }
            f.format_expression(elem);
        }
        f.push("]");
        f.output.lines().map(|l| l.len()).max().unwrap_or(0)
    }

    fn push(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn push_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn format_program(&mut self, program: &Program) {
        let stmts = &program.statements;
        for (i, stmt) in stmts.iter().enumerate() {
            // `hide` lives on the Program, not the statement, so it has to be
            // replayed here — dropping it would silently publish a private
            // function on the next `oxigen fmt`.
            if let Statement::Let { name, value: Expression::FunctionLiteral { .. } } = stmt
                && program.hidden.iter().any(|h| h == &name.value)
            {
                self.hide_next_fn = true;
            }
            self.emit_statement_line(stmt, stmts.get(i + 1));

            if i + 1 < stmts.len() && should_add_top_level_blank_line(stmt, &stmts[i + 1]) {
                self.newline();
            }
        }
        // Trailing comments, or the whole file when it has no statements.
        self.flush_comments_before(usize::MAX);
    }

    fn format_block(&mut self, statements: &[Statement]) {
        for (i, stmt) in statements.iter().enumerate() {
            self.emit_statement_line(stmt, statements.get(i + 1));
        }
    }

    /// Emits one statement on its own line, replaying any comments the author
    /// wrote above it and re-attaching a comment that trailed it.
    ///
    /// `next` is the following statement in the same list. Any block this
    /// statement opens must end before `next` begins, which is what lets
    /// `close_block` tell an end-of-block comment from one written after it.
    fn emit_statement_line(&mut self, stmt: &Statement, next: Option<&Statement>) {
        let span = stmt_span(stmt);
        if let Some(span) = span {
            self.flush_comments_before(span.line());
        }
        self.push_indent();
        if std::mem::take(&mut self.hide_next_fn) {
            self.push("hide ");
        }

        let (saved_anchor, saved_open) = (self.next_anchor, self.open_column);
        self.next_anchor = next.and_then(stmt_span).or(saved_anchor);
        if let Some(span) = span {
            self.open_column = span.column();
        }

        self.format_statement(stmt);

        self.next_anchor = saved_anchor;
        self.open_column = saved_open;
        // Set after formatting so an opened block closes on its own column first.
        if let Some(span) = span {
            self.body_column = span.column();
            self.push_trailing_comment(span.line());
        }
        self.newline();
    }

    /// Replays own-line comments that appeared before source line `line`.
    fn flush_comments_before(&mut self, line: usize) {
        while let Some(comment) = self.comments.get(self.next_comment) {
            if comment.line >= line {
                break;
            }
            // Emit on its own line rather than duplicating the passed statement's position.
            let text = comment.text.clone();
            self.next_comment += 1;
            self.push_indent();
            self.push(&text);
            self.newline();
        }
    }

    /// Replays comments that trailed a block's last statement, before the block
    /// is closed.
    ///
    /// The AST stores no span for a closing `}`, so "is this comment inside the
    /// block?" is answered from two sides: it must come before `block_bound`
    /// (the next sibling of the statement that opened the block), and it must
    /// be indented past the block's delimiters (see `body_column`). The column
    /// tests are what keep a comment written after a block's close — including
    /// after the last block in a file, where no line bound exists — from being
    /// pulled inside it.
    fn flush_comments_inside_block(&mut self) {
        while let Some(comment) = self.comments.get(self.next_comment) {
            let claimed_by_next = self
                .next_anchor
                .is_some_and(|a| comment.line >= a.line() || comment.column <= a.column());
            if claimed_by_next
                || comment.column < self.body_column
                || comment.column <= self.open_column
            {
                break;
            }
            let text = comment.text.clone();
            self.next_comment += 1;
            self.push_indent();
            self.push(&text);
            self.newline();
        }
    }

    /// Comment handling for the items of a block that aren't statements —
    /// methods, struct fields, enum variants, match arms. They still sit on
    /// source lines authors write comments above, so without this they would be
    /// swept into the previous item's body instead.
    ///
    /// Call around each item: `before` replays what was written above it,
    /// `after` re-attaches a trailing comment and marks the item as the block's
    /// latest, so the block closes against the right column.
    fn before_item(&mut self, span: Option<Span>, next: Option<Span>) -> (Option<Span>, usize) {
        if let Some(span) = span {
            self.flush_comments_before(span.line());
        }
        let saved = (self.next_anchor, self.open_column);
        self.next_anchor = next.or(self.next_anchor);
        if let Some(span) = span {
            self.open_column = span.column();
        }
        saved
    }

    fn after_item(&mut self, span: Option<Span>, saved: (Option<Span>, usize)) {
        (self.next_anchor, self.open_column) = saved;
        if let Some(span) = span {
            self.push_trailing_comment(span.line());
            self.body_column = span.column();
        }
    }

    /// Appends a comment that shared a line with the statement just emitted.
    fn push_trailing_comment(&mut self, line: usize) {
        let Some(comment) = self.comments.get(self.next_comment) else {
            return;
        };
        if comment.line != line || comment.own_line {
            return;
        }
        let text = comment.text.clone();
        self.next_comment += 1;
        self.push("  ");
        self.push(&text);
    }

    fn format_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Let { name, value } => {
                // Named `fun`s are stored as Let { name, FunctionLiteral }.
                if let Expression::FunctionLiteral {
                    token,
                    parameters,
                    body,
                } = value
                    && token.token_type == TokenType::Function {
                        self.push("fun ");
                        self.push(&name.value);
                        self.push("(");
                        self.format_params(parameters);
                        self.push(")");
                        self.format_function_body(body);
                        return;
                    }
                self.push(&name.value);
                self.push(" := ");
                self.format_expression(value);
            }
            Statement::TypedLet {
                name,
                type_ann,
                value,
                walrus,
            } => {
                self.push(&name.value);
                self.push(" <");
                self.push(&format_type_annotation(type_ann));
                self.push(">");
                if *walrus {
                    self.push(" := ");
                } else {
                    self.push(" = ");
                }
                self.format_expression(value);
            }
            Statement::TypedDeclare { name, type_ann } => {
                self.push(&name.value);
                self.push(" <");
                self.push(&format_type_annotation(type_ann));
                self.push(">");
            }
            Statement::Assign { name, value } => {
                self.push(&name.value);
                self.push(" = ");
                self.format_expression(value);
            }
            Statement::Expr(expr) => {
                self.format_expression(expr);
            }
            Statement::Give { value, .. } => {
                self.push("give ");
                self.format_expression(value);
            }
            Statement::Skip { .. } => {
                self.push("skip");
            }
            Statement::Stop { .. } => {
                self.push("stop");
            }
            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                if let Some(alt) = alternative {
                    if consequence.len() == 1 && alt.len() == 1 {
                        self.format_statement(&consequence[0]);
                        if let Some(inner) = strip_negation(condition) {
                            self.push(" unless ");
                            self.format_expression(inner);
                        } else {
                            self.push(" when ");
                            self.format_expression(condition);
                        }
                        self.push(" then ");
                        self.format_statement(&alt[0]);
                        return;
                    }
                } else if let Some(inner) = strip_negation(condition) {
                    if consequence.len() == 1 {
                        self.format_statement(&consequence[0]);
                        self.push(" unless ");
                        self.format_expression(inner);
                    } else {
                        self.push("unless ");
                        self.format_expression(inner);
                        self.block(" ", consequence);
                    }
                    return;
                } else if consequence.len() == 1 {
                    self.format_statement(&consequence[0]);
                    self.push(" when ");
                    self.format_expression(condition);
                    return;
                }

                self.format_if_as_option(condition, consequence, alternative.as_deref());
            }
            Statement::Each {
                variable,
                index_variable,
                iterable,
                body,
                ..
            } => {
                self.push("each ");
                if let Some(index) = index_variable {
                    self.push(&index.value);
                    self.push(", ");
                }
                self.push(&variable.value);
                self.push(" in ");
                self.format_expression(iterable);
                self.block(" ", body);
            }
            Statement::Repeat {
                condition, body, ..
            } => {
                if let Some(inner) = strip_negation(condition) {
                    self.push("repeat unless ");
                    self.format_expression(inner);
                } else {
                    self.push("repeat when ");
                    self.format_expression(condition);
                }
                self.block(" ", body);
            }
            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                self.push("pattern ");
                self.push(&name.value);
                self.push("(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.push(&p.value);
                }
                self.push(") when ");
                self.format_expression(condition);
            }
            Statement::Choose { subject, arms, .. } => {
                self.push("choose ");
                self.format_expression(subject);
                self.open_block(" ");
                for (i, arm) in arms.iter().enumerate() {
                    let span = choose_arm_span(arm);
                    let saved = self.before_item(span, arms.get(i + 1).and_then(choose_arm_span));
                    self.push_indent();
                    self.push(&arm.pattern_name);
                    if let Some(params) = &arm.inline_params {
                        self.push("(");
                        for (i, p) in params.iter().enumerate() {
                            if i > 0 {
                                self.push(", ");
                            }
                            self.push(&p.value);
                        }
                        self.push(")");
                    }
                    if let Some(cond) = &arm.inline_condition {
                        self.push(" when ");
                        self.format_expression(cond);
                    }
                    self.push(" -> ");
                    if arm.body.len() == 1
                        && let Statement::Expr(e) = &arm.body[0] {
                            self.format_expression(e);
                            self.after_item(span, saved);
                            self.newline();
                            continue;
                        }
                    self.block("", &arm.body);
                    self.after_item(span, saved);
                    self.newline();
                }
                self.close_block();
            }
            Statement::StructDef {
                name,
                parent,
                fields,
                ..
            } => {
                self.push("struct ");
                self.push(&name.value);
                if let Some(p) = parent {
                    self.push("(");
                    self.push(&p.value);
                    self.push(")");
                }
                self.open_block(" ");
                for (i, field) in fields.iter().enumerate() {
                    let span = Some(field.name.token.span);
                    let saved =
                        self.before_item(span, fields.get(i + 1).map(|f| f.name.token.span));
                    self.push_indent();
                    if field.hidden {
                        self.push("hide ");
                    }
                    self.push(&field.name.value);
                    self.push(" <");
                    self.push(&format_type_annotation(&field.type_ann));
                    self.push(">");
                    self.after_item(span, saved);
                    self.newline();
                }
                self.close_block();
            }
            Statement::EnumDef { name, variants, .. } => {
                self.push("enum ");
                self.push(&name.value);
                self.open_block(" ");
                for (i, variant) in variants.iter().enumerate() {
                    let span = Some(variant.name.token.span);
                    let saved =
                        self.before_item(span, variants.get(i + 1).map(|v| v.name.token.span));
                    self.push_indent();
                    self.push(&variant.name.value);
                    match &variant.kind {
                        crate::ast::VariantKind::Unit(None) => {}
                        crate::ast::VariantKind::Unit(Some(expr)) => {
                            self.push(": ");
                            self.format_expression(expr);
                        }
                        crate::ast::VariantKind::Tuple(params) => {
                            self.push("(");
                            for (i, (pname, ptype)) in params.iter().enumerate() {
                                if i > 0 {
                                    self.push(", ");
                                }
                                self.push(&pname.value);
                                self.push(" <");
                                self.push(&format_type_annotation(ptype));
                                self.push(">");
                            }
                            self.push(")");
                        }
                        crate::ast::VariantKind::Struct(fields) => {
                            self.push(" { ");
                            for (i, field) in fields.iter().enumerate() {
                                if i > 0 {
                                    self.push(", ");
                                }
                                self.push(&field.name.value);
                                self.push(" <");
                                self.push(&format_type_annotation(&field.type_ann));
                                self.push(">");
                            }
                            self.push(" }");
                        }
                    }
                    self.after_item(span, saved);
                    self.newline();
                }
                self.close_block();
            }
            Statement::IncludesDef {
                struct_name,
                methods,
                ..
            } => {
                self.push(&struct_name.value);
                self.open_block(" includes ");
                for (i, (method_name, method_expr)) in methods.iter().enumerate() {
                    if i > 0 {
                        self.newline();
                    }
                    let span = Some(method_name.token.span);
                    let saved =
                        self.before_item(span, methods.get(i + 1).map(|m| m.0.token.span));
                    self.push_indent();
                    self.push("fun ");
                    self.push(&method_name.value);
                    // The method_expr should be a FunctionLiteral
                    if let Expression::FunctionLiteral {
                        parameters, body, ..
                    } = method_expr
                    {
                        self.push("(");
                        self.format_params(parameters);
                        self.push(")");
                        self.format_function_body(body);
                    }
                    self.after_item(span, saved);
                    self.newline();
                }
                self.close_block();
            }
            Statement::DotAssign {
                object,
                field,
                value,
                ..
            } => {
                self.format_expression(object);
                self.push(".");
                self.push(&field.value);
                self.push(" = ");
                self.format_expression(value);
            }
            Statement::IndexAssign {
                object,
                index,
                value,
                ..
            } => {
                self.format_expression(object);
                self.push("[");
                self.format_expression(index);
                self.push("] = ");
                self.format_expression(value);
            }
            Statement::Introduce {
                path, selective, ..
            } => {
                // Dots are path, not decoration: dropping them resolves `.json` to the stdlib.
                let rendered = if path.is_relative {
                    format!(
                        "{}{}",
                        ".".repeat(path.parent_levels + 1),
                        path.segments.join(".")
                    )
                } else {
                    path.segments.join(".")
                };
                if let Some(names) = selective {
                    self.push("introduce {");
                    for (i, name) in names.iter().enumerate() {
                        if i > 0 {
                            self.push(", ");
                        }
                        self.push(&name.value);
                    }
                    self.push("} from ");
                    self.push(&rendered);
                } else {
                    self.push("introduce ");
                    self.push(&rendered);
                }
            }
            Statement::Unpack {
                names,
                value,
                values,
                reassign,
            } => {
                for (i, name) in names.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.push(&name.value);
                }
                self.push(if *reassign { " = " } else { " := " });
                if let Some(exprs) = values {
                    for (i, expr) in exprs.iter().enumerate() {
                        if i > 0 {
                            self.push(", ");
                        }
                        self.format_expression(expr);
                    }
                } else {
                    self.format_expression(value);
                }
            }
            Statement::Main { body, .. } => {
                self.block("main ", body);
            }
            Statement::Test { name, body, .. } => {
                self.push("<test>(");
                self.format_expression(name);
                self.block(") ", body);
            }
        }
    }

    fn format_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Ident(ident) => {
                self.push(&ident.value);
            }
            Expression::Int { value, .. } => {
                self.push(&value.to_string());
            }
            Expression::Float { value, .. } => {
                let s = value.to_string();
                self.push(&s);
                // Ensure float always has a decimal point
                if !s.contains('.') {
                    self.push(".0");
                }
            }
            Expression::Str { value, token } => {
                if token.token_type == TokenType::MultilineString {
                    self.push("\"\"\"");
                    self.push(&escape_string_triple(value));
                    self.push("\"\"\"");
                } else {
                    self.push("\"");
                    self.push(&escape_string(value));
                    self.push("\"");
                }
            }
            Expression::Char { value, .. } => {
                self.push("'");
                self.push(&escape_char(*value));
                self.push("'");
            }
            Expression::Boolean { value, .. } => {
                self.push(if *value { "True" } else { "False" });
            }
            Expression::NoneExpr { .. } => {
                self.push("None");
            }
            Expression::Array { elements, .. } => {
                if elements.is_empty() {
                    self.push("[]");
                    return;
                }

                const MAX_WIDTH: usize = 80;
                let col = self.current_column();
                let inline = self.array_inline_width(elements);

                if col + inline <= MAX_WIDTH {
                    self.push("[");
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            self.push(", ");
                        }
                        self.format_expression(elem);
                    }
                    self.push("]");
                } else {
                    self.push("[");
                    self.newline();
                    self.indent += 1;
                    for elem in elements.iter() {
                        self.push_indent();
                        self.format_expression(elem);
                        self.push(",");
                        self.newline();
                    }
                    self.indent -= 1;
                    self.push_indent();
                    self.push("]");
                }
            }
            Expression::Prefix {
                operator, right, ..
            } => {
                self.push(operator);
                if operator == "not" {
                    self.push(" ");
                }
                self.format_expression(right);
            }
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => {
                self.format_expression(left);
                self.push(" ");
                self.push(operator);
                self.push(" ");
                self.format_expression(right);
            }
            Expression::Postfix { left, operator, .. } => {
                self.format_expression(left);
                self.push(operator);
            }
            Expression::Call {
                function,
                args,
                named_args,
                ..
            } => {
                self.format_expression(function);
                self.push("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.format_expression(arg);
                }
                for (i, (name, val)) in named_args.iter().enumerate() {
                    if i > 0 || !args.is_empty() {
                        self.push(", ");
                    }
                    self.push(name);
                    self.push("=");
                    self.format_expression(val);
                }
                self.push(")");
            }
            Expression::Index { left, index, .. } => {
                self.format_expression(left);
                self.push("[");
                self.format_expression(index);
                self.push("]");
            }
            Expression::Grouped(inner) => {
                self.push("(");
                self.format_expression(inner);
                self.push(")");
            }
            Expression::FunctionLiteral {
                parameters, body, ..
            } => {
                self.push("fun(");
                self.format_params(parameters);
                self.push(")");
                self.format_function_body(body);
            }
            Expression::StructLiteral {
                struct_name,
                field_values,
                ..
            } => {
                self.push(struct_name);
                self.push("(");
                for (i, (name, val)) in field_values.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.push(name);
                    self.push("=");
                    self.format_expression(val);
                }
                self.push(")");
            }
            Expression::DotAccess { left, field, .. } => {
                self.format_expression(left);
                self.push(".");
                self.push(&field.value);
            }
            Expression::Slice {
                left, start, end, ..
            } => {
                self.format_expression(left);
                self.push("[");
                if let Some(s) = start {
                    self.format_expression(s);
                }
                self.push(":");
                if let Some(e) = end {
                    self.format_expression(e);
                }
                self.push("]");
            }
            Expression::EnumVariantConstruct {
                enum_name,
                variant_name,
                kind,
                ..
            } => {
                self.push(enum_name);
                self.push(".");
                self.push(variant_name);
                match kind {
                    crate::ast::EnumConstructKind::Tuple(args) => {
                        self.push("(");
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                self.push(", ");
                            }
                            self.format_expression(arg);
                        }
                        self.push(")");
                    }
                    crate::ast::EnumConstructKind::Struct(fields) => {
                        self.push(" { ");
                        for (i, (fname, fval)) in fields.iter().enumerate() {
                            if i > 0 {
                                self.push(", ");
                            }
                            self.push(fname);
                            self.push(": ");
                            self.format_expression(fval);
                        }
                        self.push(" }");
                    }
                }
            }
            Expression::TupleLiteral { elements, .. } => {
                self.push("(");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.format_expression(elem);
                }
                if elements.len() == 1 {
                    self.push(",");
                }
                self.push(")");
            }
            Expression::MapLiteral { entries, .. } => {
                if entries.is_empty() {
                    // `{}` is the only empty-map form; `{:}` is always rejected.
                    self.push("{}");
                    return;
                }
                self.push("{");
                for (i, (key, val)) in entries.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.format_expression(key);
                    self.push(": ");
                    self.format_expression(val);
                }
                self.push("}");
            }
            Expression::Option {
                arms,
                default,
                error_default,
                ..
            } => {
                self.push("option");
                self.open_block(" ");
                let tail_anchor = error_default
                    .as_deref()
                    .or(default.as_deref())
                    .and_then(|b| b.first())
                    .and_then(stmt_span);
                for (i, arm) in arms.iter().enumerate() {
                    let span = expr_span(&arm.condition);
                    let next = arms
                        .get(i + 1)
                        .and_then(|a| expr_span(&a.condition))
                        .or(tail_anchor);
                    let saved = self.before_item(span, next);
                    self.push_indent();
                    self.format_expression(&arm.condition);
                    self.block(" -> ", &arm.body);
                    self.after_item(span, saved);
                    self.newline();
                }
                if let Some(err_default) = error_default {
                    self.push_indent();
                    self.block("error -> ", err_default);
                    self.newline();
                }
                if let Some(def) = default {
                    self.push_indent();
                    self.block("", def);
                    self.newline();
                }
                self.close_block();
            }
            Expression::Guard {
                value,
                binding,
                error_tag,
                fallback,
                ..
            } => {
                self.format_expression(value);
                self.push(" guard ");
                if let Some(tag) = error_tag {
                    self.push(tag);
                    self.push(":");
                }
                self.push(&binding.value);
                self.push(" -> ");
                self.format_expression(fallback);
            }
            Expression::Log {
                tag,
                sub_tag,
                message,
                ..
            } => {
                self.push("<log");
                if let Some(t) = tag {
                    self.push("<");
                    self.push(t);
                    if let Some(st) = sub_tag {
                        self.push("<");
                        self.push(st);
                        self.push(">");
                    }
                    self.push(">");
                }
                self.push(">");
                if let Some(msg) = message {
                    self.push("(");
                    self.format_expression(msg);
                    self.push(")");
                }
            }
            Expression::ErrorConstruct { tag, value, .. } => {
                self.push("<Error");
                if let Some(t) = tag {
                    self.push("<");
                    self.push(t);
                    self.push(">");
                }
                self.push(">");
                self.push("(");
                self.format_expression(value);
                self.push(")");
            }
            Expression::ValueConstruct { value, .. } => {
                self.push("<Value>(");
                self.format_expression(value);
                self.push(")");
            }
            // `<type<T>>(expr)` is the only form the parser knows. This used to
            // emit `expr as <T>`, a syntax that has never existed, so formatting
            // any file using an angle-form conversion left it unparseable.
            Expression::TypeWrap { target, value, .. } => {
                self.push("<type<");
                self.push(&format_type_annotation(target));
                self.push(">>(");
                self.format_expression(value);
                self.push(")");
            }
            Expression::Fail { value, .. } => {
                self.push("<fail>(");
                self.format_expression(value);
                self.push(")");
            }
            Expression::Diverge { body, .. } => {
                self.block("diverge ", body);
            }
            Expression::DivergeEach {
                variable,
                iterable,
                body,
                ..
            } => {
                self.push("diverge each ");
                self.push(&variable.value);
                self.push(" in ");
                self.format_expression(iterable);
                self.block(" ", body);
            }
            Expression::Converge { task, timeout, .. } => {
                self.push("converge ");
                self.format_expression(task);
                if let Some(ms) = timeout {
                    self.push(" within ");
                    self.format_expression(ms);
                }
            }
            Expression::Unless {
                consequence,
                condition,
                alternative,
                ..
            } => {
                self.format_expression(consequence);
                self.push(" unless ");
                self.format_expression(condition);
                self.push(" then ");
                self.format_expression(alternative);
            }
            Expression::StringInterp { parts, token } => {
                let multiline = token.token_type == TokenType::MultilineInterpStart;
                let fence = if multiline { "\"\"\"" } else { "\"" };
                self.push(fence);
                for part in parts {
                    match part {
                        StringInterpPart::Literal(s) => {
                            if multiline {
                                self.push(&escape_string_triple(s));
                            } else {
                                self.push(&escape_string(s));
                            }
                        }
                        StringInterpPart::Expr(e) => {
                            self.push("{");
                            self.format_expression(e);
                            self.push("}");
                        }
                    }
                }
                self.push(fence);
            }
        }
    }

    fn format_params(&mut self, params: &[TypedParam]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.push(", ");
            }
            self.push(&param.ident.value);
            if param.optional {
                self.push("?");
            }
            if let Some(type_ann) = &param.type_ann {
                self.push(" <");
                self.push(&format_type_annotation(type_ann));
                self.push(">");
            }
            if let Some(default) = &param.default {
                self.push(" = ");
                self.format_expression(default);
            }
        }
    }

    fn format_function_body(&mut self, body: &[Statement]) {
        // Inline only for brace style; indent style has no inline block form.
        if !self.indent_style
            && body.len() == 1
            && let Statement::Expr(expr) = &body[0]
                && !is_multiline_expr(expr) {
                    self.push(" { ");
                    self.format_expression(expr);
                    self.push(" }");
                    return;
                }
        self.block(" ", body);
    }

    fn format_if_as_option(
        &mut self,
        condition: &Expression,
        consequence: &[Statement],
        alternative: Option<&[Statement]>,
    ) {
        self.push("option");
        self.open_block(" ");
        self.push_indent();
        self.format_expression(condition);
        self.push(" -> ");
        self.format_option_body(consequence);
        self.newline();

        if let Some(alt) = alternative {
            self.push_indent();
            self.format_option_body(alt);
            self.newline();
        }

        self.close_block();
    }

    fn format_option_body(&mut self, body: &[Statement]) {
        self.block("", body);
    }
}

/// Source line a statement starts on, used to decide which comments belong
/// above it. `None` for statements that carry no token at all (`skip`/`stop`);
/// those simply don't trigger a comment flush, so pending comments land on the
/// next statement that does have a position.
/// A choose arm's pattern name is a bare `String` with no token, so the arm is
/// located by its guard condition or, failing that, its first body statement.
fn choose_arm_span(arm: &ChooseArm) -> Option<Span> {
    arm.inline_params
        .as_ref()
        .and_then(|p| p.first())
        .map(|p| p.token.span)
        .or_else(|| arm.inline_condition.as_ref().and_then(expr_span))
        .or_else(|| arm.body.first().and_then(stmt_span))
}

fn stmt_span(stmt: &Statement) -> Option<Span> {
    match stmt {
        Statement::Skip { token } | Statement::Stop { token } => Some(token.span),
        Statement::Let { name, .. }
        | Statement::TypedLet { name, .. }
        | Statement::TypedDeclare { name, .. }
        | Statement::Assign { name, .. } => Some(name.token.span),
        Statement::Unpack { names, value, .. } => names
            .first()
            .map(|n| n.token.span)
            .or_else(|| expr_span(value)),
        Statement::Expr(expr) => expr_span(expr),
        Statement::Each { token, .. }
        | Statement::Repeat { token, .. }
        | Statement::Pattern { token, .. }
        | Statement::Choose { token, .. }
        | Statement::If { token, .. }
        | Statement::Give { token, .. }
        | Statement::StructDef { token, .. }
        | Statement::EnumDef { token, .. }
        | Statement::IncludesDef { token, .. }
        | Statement::Introduce { token, .. }
        | Statement::Main { token, .. }
        | Statement::Test { token, .. } => Some(token.span),
        // The statement starts at the target, not the `=` the token points to.
        Statement::DotAssign { token, object, .. } => {
            expr_span(object).or(Some(token.span))
        }
        Statement::IndexAssign { token, object, .. } => {
            expr_span(object).or(Some(token.span))
        }
    }
}

/// Source line an expression starts on. Infix/postfix/call nodes hold the
/// operator's token, which sits after the expression begins, so those recurse
/// into their leftmost child.
fn expr_span(expr: &Expression) -> Option<Span> {
    match expr {
        Expression::Ident(ident) => Some(ident.token.span),
        Expression::Grouped(inner) => expr_span(inner),
        Expression::Infix { left, token, .. }
        | Expression::Postfix { left, token, .. }
        | Expression::Index { left, token, .. }
        | Expression::DotAccess { left, token, .. }
        | Expression::Slice { left, token, .. } => expr_span(left).or(Some(token.span)),
        Expression::Call { function, token, .. } => {
            expr_span(function).or(Some(token.span))
        }
        Expression::Guard { value, token, .. } | Expression::TypeWrap { value, token, .. } => {
            expr_span(value).or(Some(token.span))
        }
        Expression::Unless {
            consequence, token, ..
        } => expr_span(consequence).or(Some(token.span)),
        Expression::Int { token, .. }
        | Expression::Str { token, .. }
        | Expression::Float { token, .. }
        | Expression::Char { token, .. }
        | Expression::Boolean { token, .. }
        | Expression::NoneExpr { token }
        | Expression::Array { token, .. }
        | Expression::Prefix { token, .. }
        | Expression::FunctionLiteral { token, .. }
        | Expression::StructLiteral { token, .. }
        | Expression::EnumVariantConstruct { token, .. }
        | Expression::TupleLiteral { token, .. }
        | Expression::MapLiteral { token, .. }
        | Expression::Option { token, .. }
        | Expression::Log { token, .. }
        | Expression::ErrorConstruct { token, .. }
        | Expression::ValueConstruct { token, .. }
        | Expression::Fail { token, .. }
        | Expression::StringInterp { token, .. }
        | Expression::Diverge { token, .. }
        | Expression::DivergeEach { token, .. }
        | Expression::Converge { token, .. } => Some(token.span),
    }
}

fn format_type_annotation(ann: &TypeAnnotation) -> String {
    match ann {
        TypeAnnotation::Int => "int".to_string(),
        TypeAnnotation::Str => "str".to_string(),
        TypeAnnotation::Float => "float".to_string(),
        TypeAnnotation::Char => "char".to_string(),
        TypeAnnotation::Bool => "bool".to_string(),
        TypeAnnotation::Array => "array".to_string(),
        TypeAnnotation::Byte => "byte".to_string(),
        TypeAnnotation::Uint => "uint".to_string(),
        TypeAnnotation::Tuple => "tuple".to_string(),
        TypeAnnotation::Map => "map".to_string(),
        TypeAnnotation::Set => "set".to_string(),
        TypeAnnotation::Generic => "generic".to_string(),
        TypeAnnotation::NoneType => "None".to_string(),
        TypeAnnotation::ErrorType(tag) => match tag {
            Some(t) => format!("Error<{}>", t),
            None => "Error".to_string(),
        },
        TypeAnnotation::ValueType => "Value".to_string(),
        // Every caller wraps this in `<`...`>`, so the join closes and reopens
        // the brackets: `<int> || <float>`, which is how the stdlib writes it.
        TypeAnnotation::Union(types) => types
            .iter()
            .map(format_type_annotation)
            .collect::<Vec<_>>()
            .join("> || <"),
        TypeAnnotation::Struct(name) => name.clone(),
        TypeAnnotation::EnumGeneric => "Enum".to_string(),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TopLevelSection {
    Import,
    Variable,
    Definition,
    Other,
}

fn top_level_section(stmt: &Statement) -> TopLevelSection {
    match stmt {
        Statement::Introduce { .. } => TopLevelSection::Import,
        Statement::Let { value, .. } => match value {
            Expression::FunctionLiteral { token, .. }
                if token.token_type == TokenType::Function =>
            {
                TopLevelSection::Definition
            }
            _ => TopLevelSection::Variable,
        },
        Statement::TypedLet { value, .. } => match value {
            Expression::FunctionLiteral { token, .. }
                if token.token_type == TokenType::Function =>
            {
                TopLevelSection::Definition
            }
            _ => TopLevelSection::Variable,
        },
        Statement::TypedDeclare { .. } | Statement::Assign { .. } => TopLevelSection::Variable,
        Statement::Pattern { .. }
        | Statement::StructDef { .. }
        | Statement::EnumDef { .. }
        | Statement::IncludesDef { .. }
        | Statement::Main { .. }
        | Statement::Test { .. } => TopLevelSection::Definition,
        _ => TopLevelSection::Other,
    }
}

fn should_add_top_level_blank_line(current: &Statement, next: &Statement) -> bool {
    let current_section = top_level_section(current);
    let next_section = top_level_section(next);

    if current_section == TopLevelSection::Definition && next_section == TopLevelSection::Definition
    {
        return true;
    }

    current_section != TopLevelSection::Other
        && next_section != TopLevelSection::Other
        && current_section != next_section
}

fn is_multiline_expr(expr: &Expression) -> bool {
    match expr {
        Expression::Option { .. } | Expression::FunctionLiteral { .. } => true,
        Expression::MapLiteral { entries, .. } => !entries.is_empty(),
        _ => false,
    }
}

fn strip_negation(expr: &Expression) -> Option<&Expression> {
    if let Expression::Prefix {
        operator, right, ..
    } = expr
        && operator == "not" {
            return Some(right.as_ref());
        }
    None
}

fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            // A bare `{` would re-open an interpolation on the way back in.
            '{' => result.push_str("\\{"),
            _ => result.push(c),
        }
    }
    result
}

/// Escape the body of a triple-quoted string. Raw newlines are kept verbatim
/// (that's the point of a multi-line literal); backslashes, tabs and carriage
/// returns are escaped as usual. A `"` is escaped only when it would otherwise
/// collide with the closing `"""` fence — when it's the third in a run, or the
/// final character of this chunk (which would butt up against the fence). This
/// keeps stray quotes (e.g. `She said "hi"`) unescaped in the common case.
fn escape_string_triple(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let mut result = String::new();
    let mut quote_run = 0; // consecutive unescaped `"` already emitted
    for (i, &c) in chars.iter().enumerate() {
        match c {
            '\\' => {
                result.push_str("\\\\");
                quote_run = 0;
            }
            '\n' => {
                result.push('\n');
                quote_run = 0;
            }
            '\t' => {
                result.push_str("\\t");
                quote_run = 0;
            }
            '\r' => {
                result.push_str("\\r");
                quote_run = 0;
            }
            '"' if quote_run == 2 || i + 1 == chars.len() => {
                result.push_str("\\\"");
                quote_run = 0;
            }
            '"' => {
                result.push('"');
                quote_run += 1;
            }
            // A bare `{` would re-open an interpolation on the way back in.
            '{' => {
                result.push_str("\\{");
                quote_run = 0;
            }
            _ => {
                result.push(c);
                quote_run = 0;
            }
        }
    }
    result
}

fn escape_char(c: char) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        _ => c.to_string(),
    }
}
