use oxigen_core::parser::{Diagnostic as OxiDiagnostic, Severity};
use tower_lsp::lsp_types::*;

pub fn to_lsp_diagnostics(diagnostics: &[OxiDiagnostic]) -> Vec<Diagnostic> {
    diagnostics.iter().map(to_lsp_diagnostic).collect()
}

fn to_lsp_diagnostic(diag: &OxiDiagnostic) -> Diagnostic {
    // Parser uses 1-based line/column, LSP uses 0-based
    let line = diag.span.line.saturating_sub(1) as u32;
    let col = diag.span.column.saturating_sub(1) as u32;

    let range = Range {
        start: Position {
            line,
            character: col,
        },
        end: Position {
            line,
            character: col + 1,
        },
    };

    let message = match &diag.suggestion {
        Some(hint) => format!("{}\nhint: {}", diag.message, hint),
        None => diag.message.clone(),
    };

    Diagnostic {
        range,
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        source: Some("oxigen".to_string()),
        message,
        ..Default::default()
    }
}
