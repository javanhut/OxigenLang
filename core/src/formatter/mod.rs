use crate::ast::*;
use crate::token::TokenType;

pub struct Formatter {
    indent: usize,
    output: String,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            output: String::new(),
        }
    }

    pub fn format(program: &Program) -> String {
        let mut f = Formatter::new();
        f.format_program(program);
        // Ensure file ends with exactly one newline
        let result = f.output.trim_end().to_string();
        if result.is_empty() {
            String::new()
        } else {
            result + "\n"
        }
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
            self.push_indent();
            self.format_statement(stmt);
            self.newline();

            if i + 1 < stmts.len() && should_add_top_level_blank_line(stmt, &stmts[i + 1]) {
                self.newline();
            }
        }
    }

    fn format_block(&mut self, statements: &[Statement]) {
        for stmt in statements {
            self.push_indent();
            self.format_statement(stmt);
            self.newline();
        }
    }

    fn format_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Let { name, value } => {
                // Named functions: `fun name(params) { body }`
                // The parser stores them as Let { name, FunctionLiteral { token: Function } }
                if let Expression::FunctionLiteral {
                    token,
                    parameters,
                    body,
                } = value
                {
                    if token.token_type == TokenType::Function {
                        self.push("fun ");
                        self.push(&name.value);
                        self.push("(");
                        self.format_params(parameters);
                        self.push(")");
                        self.format_function_body(body);
                        return;
                    }
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
            Statement::Skip => {
                self.push("skip");
            }
            Statement::Stop => {
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
                        self.push(" {");
                        self.newline();
                        self.indent += 1;
                        self.format_block(consequence);
                        self.indent -= 1;
                        self.push_indent();
                        self.push("}");
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
                iterable,
                body,
                ..
            } => {
                self.push("each ");
                self.push(&variable.value);
                self.push(" in ");
                self.format_expression(iterable);
                self.push(" {");
                self.newline();
                self.indent += 1;
                self.format_block(body);
                self.indent -= 1;
                self.push_indent();
                self.push("}");
            }
            Statement::Repeat {
                condition, body, ..
            } => {
                self.push("repeat ");
                self.format_expression(condition);
                self.push(" {");
                self.newline();
                self.indent += 1;
                self.format_block(body);
                self.indent -= 1;
                self.push_indent();
                self.push("}");
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
                self.push(" {");
                self.newline();
                self.indent += 1;
                for arm in arms {
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
                    if arm.body.len() == 1 {
                        if let Statement::Expr(e) = &arm.body[0] {
                            self.format_expression(e);
                            self.newline();
                            continue;
                        }
                    }
                    self.push("{");
                    self.newline();
                    self.indent += 1;
                    self.format_block(&arm.body);
                    self.indent -= 1;
                    self.push_indent();
                    self.push("}");
                    self.newline();
                }
                self.indent -= 1;
                self.push_indent();
                self.push("}");
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
                self.push(" {");
                self.newline();
                self.indent += 1;
                for field in fields {
                    self.push_indent();
                    if field.hidden {
                        self.push("hide ");
                    }
                    self.push(&field.name.value);
                    self.push(" <");
                    self.push(&format_type_annotation(&field.type_ann));
                    self.push(">");
                    self.newline();
                }
                self.indent -= 1;
                self.push_indent();
                self.push("}");
            }
            Statement::EnumDef { name, variants, .. } => {
                self.push("enum ");
                self.push(&name.value);
                self.push(" {");
                self.newline();
                self.indent += 1;
                for variant in variants {
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
                    self.newline();
                }
                self.indent -= 1;
                self.push_indent();
                self.push("}");
            }
            Statement::ContainsDef {
                struct_name,
                methods,
                ..
            } => {
                self.push(&struct_name.value);
                self.push(" contains {");
                self.newline();
                self.indent += 1;
                for (i, (method_name, method_expr)) in methods.iter().enumerate() {
                    if i > 0 {
                        self.newline();
                    }
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
                    self.newline();
                }
                self.indent -= 1;
                self.push_indent();
                self.push("}");
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
                if let Some(names) = selective {
                    self.push("introduce {");
                    for (i, name) in names.iter().enumerate() {
                        if i > 0 {
                            self.push(", ");
                        }
                        self.push(&name.value);
                    }
                    self.push("} from ");
                    self.push(&path.segments.join("."));
                } else {
                    self.push("introduce ");
                    self.push(&path.segments.join("."));
                }
            }
            Statement::Unpack { names, value, values, reassign } => {
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
                self.push("main {");
                self.newline();
                self.indent += 1;
                self.format_block(body);
                self.indent -= 1;
                self.push_indent();
                self.push("}");
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
            Expression::Str { value, .. } => {
                self.push("\"");
                self.push(&escape_string(value));
                self.push("\"");
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
                    self.push("{:}");
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
                self.push("option {");
                self.newline();
                self.indent += 1;
                for arm in arms {
                    self.push_indent();
                    self.format_expression(&arm.condition);
                    self.push(" -> {");
                    self.newline();
                    self.indent += 1;
                    self.format_block(&arm.body);
                    self.indent -= 1;
                    self.push_indent();
                    self.push("}");
                    self.newline();
                }
                if let Some(err_default) = error_default {
                    self.push_indent();
                    self.push("error -> {");
                    self.newline();
                    self.indent += 1;
                    self.format_block(err_default);
                    self.indent -= 1;
                    self.push_indent();
                    self.push("}");
                    self.newline();
                }
                if let Some(def) = default {
                    self.push_indent();
                    self.push("{");
                    self.newline();
                    self.indent += 1;
                    self.format_block(def);
                    self.indent -= 1;
                    self.push_indent();
                    self.push("}");
                    self.newline();
                }
                self.indent -= 1;
                self.push_indent();
                self.push("}");
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
            Expression::TypeWrap { target, value, .. } => {
                self.format_expression(value);
                self.push(" as <");
                self.push(&format_type_annotation(target));
                self.push(">");
            }
            Expression::Fail { value, .. } => {
                self.push("<fail>(");
                self.format_expression(value);
                self.push(")");
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
            Expression::StringInterp { parts, .. } => {
                self.push("\"");
                for part in parts {
                    match part {
                        StringInterpPart::Literal(s) => {
                            self.push(&escape_string(s));
                        }
                        StringInterpPart::Expr(e) => {
                            self.push("{");
                            self.format_expression(e);
                            self.push("}");
                        }
                    }
                }
                self.push("\"");
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
        // Single expression body: inline on the same line
        if body.len() == 1 {
            if let Statement::Expr(expr) = &body[0] {
                if !is_multiline_expr(expr) {
                    self.push(" { ");
                    self.format_expression(expr);
                    self.push(" }");
                    return;
                }
            }
        }
        self.push(" {");
        self.newline();
        self.indent += 1;
        self.format_block(body);
        self.indent -= 1;
        self.push_indent();
        self.push("}");
    }

    fn format_if_as_option(
        &mut self,
        condition: &Expression,
        consequence: &[Statement],
        alternative: Option<&[Statement]>,
    ) {
        self.push("option {");
        self.newline();
        self.indent += 1;
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

        self.indent -= 1;
        self.push_indent();
        self.push("}");
    }

    fn format_option_body(&mut self, body: &[Statement]) {
        self.push("{");
        self.newline();
        self.indent += 1;
        self.format_block(body);
        self.indent -= 1;
        self.push_indent();
        self.push("}");
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
        TypeAnnotation::Union(types) => types
            .iter()
            .map(|t| format!("<{}>", format_type_annotation(t)))
            .collect::<Vec<_>>()
            .join(" || "),
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
        | Statement::ContainsDef { .. }
        | Statement::Main { .. } => TopLevelSection::Definition,
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
    {
        if operator == "not" {
            return Some(right.as_ref());
        }
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
            _ => result.push(c),
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
