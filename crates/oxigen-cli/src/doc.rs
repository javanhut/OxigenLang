//! `oxigen doc`: the reference pages in docs/, built into the binary so they
//! match the version installed.
//!
//!     oxigen doc                  every module, and the builtins
//!     oxigen doc io               the io page
//!     oxigen doc io.append_file   one function's section
//!     oxigen doc exec_argv        every function with that name, anywhere

macro_rules! pages {
    ($($name:literal),* $(,)?) => {
        &[$(($name, include_str!(concat!("../../../docs/stdlib/", $name, ".md")))),*]
    };
}

const MODULES: &[(&str, &str)] = pages!(
    "ansi", "api", "array", "channel", "datetime", "encoding", "env", "ffi", "hash", "io", "json",
    "math", "net", "os", "parse_args", "path", "random", "regex", "result", "strings", "test",
    "time", "toml",
);

const BUILTINS: &str = include_str!("../../../docs/builtins.md");

/// The first paragraph after a page's title, as one line.
fn summary(page: &str) -> String {
    page.lines()
        .skip_while(|l| l.starts_with('#') || l.trim().is_empty())
        .take_while(|l| !l.trim().is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

fn heading_level(line: &str) -> Option<usize> {
    let n = line.chars().take_while(|&c| c == '#').count();
    (n > 0 && line[n..].starts_with(' ')).then_some(n)
}

/// Whether a heading documents `name`: `` ### `name(args)` `` or `` `name` ``.
fn documents(line: &str, name: &str) -> bool {
    line.split('`').skip(1).step_by(2).any(|code| {
        code == name || code.starts_with(&format!("{name}(")) || code.ends_with(&format!(".{name}"))
    })
}

/// Each section of `page` whose heading documents `name`, with its body.
fn sections<'a>(page: &'a str, name: &str) -> Vec<String> {
    let lines: Vec<&str> = page.lines().collect();
    let mut found = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        let Some(level) = heading_level(line) else { continue };
        if level < 2 || !documents(line, name) {
            continue;
        }
        let end = lines[i + 1..]
            .iter()
            .position(|l| heading_level(l).is_some_and(|n| n <= level))
            .map_or(lines.len(), |p| i + 1 + p);
        found.push(lines[i..end].join("\n").trim_end().to_string());
    }
    found
}

fn index() -> String {
    let mut out = String::from("Standard library (`oxigen doc <module>`):\n\n");
    let width = MODULES.iter().map(|(n, _)| n.len()).max().unwrap_or(0);
    for (name, page) in MODULES {
        let first = summary(page);
        let short = first.split(". ").next().unwrap_or(&first).trim_end_matches('.');
        out.push_str(&format!("  {name:width$}  {short}\n"));
    }
    out.push_str("\nBuilt-in functions, available without an import: `oxigen doc builtins`\n");
    out.push_str("One function: `oxigen doc io.append_file`, or just `oxigen doc append_file`\n");
    out
}

/// The text for `oxigen doc <topic>`, or an error naming what was not found.
pub fn lookup(topic: Option<&str>) -> Result<String, String> {
    let Some(topic) = topic else {
        return Ok(index());
    };
    if topic == "builtins" {
        return Ok(BUILTINS.trim_end().to_string());
    }
    if let Some((_, page)) = MODULES.iter().find(|(n, _)| *n == topic) {
        return Ok(page.trim_end().to_string());
    }
    if let Some((module, name)) = topic.split_once('.') {
        let Some((_, page)) = MODULES.iter().find(|(n, _)| *n == module) else {
            return Err(format!("no module named `{module}`; run `oxigen doc` for the list"));
        };
        let found = sections(page, name);
        if found.is_empty() {
            return Err(format!("`{module}` has no `{name}`; run `oxigen doc {module}` for its functions"));
        }
        return Ok(found.join("\n\n"));
    }
    // A bare name: a builtin, or a function in any module.
    let mut found: Vec<String> = sections(BUILTINS, topic)
        .into_iter()
        .map(|s| format!("[builtin]\n{s}"))
        .collect();
    for (module, page) in MODULES {
        found.extend(sections(page, topic).into_iter().map(|s| format!("[{module}]\n{s}")));
    }
    if found.is_empty() {
        return Err(format!("nothing named `{topic}`; run `oxigen doc` for the modules"));
    }
    Ok(found.join("\n\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_functions_by_module_and_by_name() {
        assert!(lookup(Some("io.append_file")).unwrap().contains("append_file(path, content)"));
        assert!(lookup(Some("exec_argv")).unwrap().starts_with("[os]"));
        assert!(lookup(Some("push")).unwrap().starts_with("[builtin]"));
        assert!(lookup(Some("io")).unwrap().starts_with("# io"));
        assert!(lookup(Some("io.nope")).is_err());
        assert!(lookup(None).unwrap().contains("channel"));
    }
}
