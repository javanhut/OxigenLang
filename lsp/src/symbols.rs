use tower_lsp::lsp_types::*;

use oxigen_core::ast::{Expression, Program, Statement};
use oxigen_core::token::Span;

pub fn get_document_symbols(program: &Program) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    collect_symbols(&program.statements, &mut symbols);
    symbols
}

fn collect_symbols(statements: &[Statement], symbols: &mut Vec<SymbolInformation>) {
    for stmt in statements {
        match stmt {
            Statement::Let { name, value } => {
                let kind = if matches!(value, Expression::FunctionLiteral { .. }) {
                    SymbolKind::FUNCTION
                } else {
                    SymbolKind::VARIABLE
                };
                symbols.push(make_symbol(
                    &name.value,
                    kind,
                    name.token.span,
                ));
            }
            Statement::TypedLet { name, value, .. } => {
                let kind = if matches!(value, Expression::FunctionLiteral { .. }) {
                    SymbolKind::FUNCTION
                } else {
                    SymbolKind::VARIABLE
                };
                symbols.push(make_symbol(
                    &name.value,
                    kind,
                    name.token.span,
                ));
            }
            Statement::TypedDeclare { name, .. } => {
                symbols.push(make_symbol(
                    &name.value,
                    SymbolKind::VARIABLE,
                    name.token.span,
                ));
            }
            Statement::StructDef { name, fields, .. } => {
                symbols.push(make_symbol(
                    &name.value,
                    SymbolKind::STRUCT,
                    name.token.span,
                ));
                // Add fields as sub-symbols
                for field in fields {
                    symbols.push(make_symbol(
                        &field.name.value,
                        SymbolKind::FIELD,
                        field.name.token.span,
                    ));
                }
            }
            Statement::ContainsDef { struct_name, methods, .. } => {
                for (method_name, _) in methods {
                    symbols.push(make_symbol(
                        &format!("{}.{}", struct_name.value, method_name.value),
                        SymbolKind::METHOD,
                        method_name.token.span,
                    ));
                }
            }
            Statement::Pattern { name, .. } => {
                symbols.push(make_symbol(
                    &name.value,
                    SymbolKind::FUNCTION,
                    name.token.span,
                ));
            }
            Statement::Each { body, .. } => {
                collect_symbols(body, symbols);
            }
            Statement::Repeat { body, .. } => {
                collect_symbols(body, symbols);
            }
            Statement::If { consequence, alternative, .. } => {
                collect_symbols(consequence, symbols);
                if let Some(alt) = alternative {
                    collect_symbols(alt, symbols);
                }
            }
            Statement::Main { body, .. } => {
                collect_symbols(body, symbols);
            }
            Statement::Introduce { path, .. } => {
                let module_name = path.segments.join(".");
                symbols.push(make_symbol(
                    &format!("introduce {}", module_name),
                    SymbolKind::MODULE,
                    // Use a default span since introduce tokens may vary
                    Span::new(1, 0),
                ));
            }
            _ => {}
        }
    }
}

#[allow(deprecated)]
fn make_symbol(name: &str, kind: SymbolKind, span: Span) -> SymbolInformation {
    let line = span.line.saturating_sub(1) as u32;
    let col = span.column.saturating_sub(1) as u32;

    let pos = Position {
        line,
        character: col,
    };

    SymbolInformation {
        name: name.to_string(),
        kind,
        tags: None,
        deprecated: None,
        location: Location {
            uri: Url::parse("file:///").unwrap(),
            range: Range {
                start: pos,
                end: Position {
                    line,
                    character: col + name.len() as u32,
                },
            },
        },
        container_name: None,
    }
}
