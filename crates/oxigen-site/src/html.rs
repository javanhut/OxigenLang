//! Small HTML pieces shared by every page.

pub fn escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
    out
}

/// An Oxigen code card: highlighted at build time, with Copy, and Run plus
/// "Open in playground" when the example runs on its own. site.js wires the
/// buttons; the code itself needs no script to read.
pub fn oxi_block(code: &str, runnable: bool) -> String {
    oxi_card(code, runnable, None)
}

/// [`oxi_block`] with the output of a run already shown under the code, as
/// the build produced it. Run replaces it with a live run.
pub fn oxi_card(code: &str, runnable: bool, output: Option<&str>) -> String {
    let output = match output {
        Some(text) => format!(
            "<div class=\"code-output\"><div class=\"code-output-bar\"><span>Output</span><span class=\"time\"></span></div><pre aria-live=\"polite\">{}</pre></div>",
            escape(text.trim_end())
        ),
        None => String::new(),
    };
    let run = if runnable {
        "<button class=\"btn-run\" type=\"button\" aria-label=\"Run this example\"><svg viewBox=\"0 0 16 16\" aria-hidden=\"true\"><path d=\"M4 2.5v11l9-5.5z\"/></svg>Run</button><button class=\"btn-open\" type=\"button\" aria-label=\"Open in the playground\">Playground</button>"
    } else {
        ""
    };
    format!(
        "<div class=\"code code-oxi{cls}\" data-lang=\"oxi\"><div class=\"code-bar\"><span class=\"code-lang\">Oxigen</span><span class=\"code-actions\">{run}<button class=\"btn-copy\" type=\"button\">Copy</button></span></div><pre><code class=\"language-oxi\">{}</code></pre>{output}</div>\n",
        oxigen_core::highlight::to_html(code),
        cls = if runnable { " runnable" } else { "" },
    )
}

/// A cheap content hash for cache-busting asset URLs (FNV-1a, hex).
pub fn fingerprint(bytes: &[u8]) -> String {
    let mut hash: u64 = 0xcbf29ce484222325;
    for b in bytes {
        hash ^= *b as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{:08x}", hash as u32)
}

/// Put `base` in front of the root-relative URLs in `html`'s attributes, for
/// a site served below the root of its host: a GitHub project page lives at
/// `user.github.io/Repo/`, so `/docs/` must become `/Repo/docs/`. Protocol-
/// relative `//host` URLs and text are left alone; an empty `base` changes
/// nothing.
pub fn rebase(html: &str, base: &str) -> String {
    if base.is_empty() {
        return html.to_string();
    }
    let mut out = String::with_capacity(html.len() + html.len() / 16);
    let mut rest = html;
    while let Some(at) = rest.find("=\"/") {
        let (before, after) = rest.split_at(at + 2);
        out.push_str(before);
        let attribute = ["href", "src", "content", "action"]
            .iter()
            .any(|a| before.ends_with(&format!("{a}=\"")));
        if attribute && !after.starts_with("//") {
            out.push_str(base);
        }
        rest = after;
    }
    out.push_str(rest);
    out
}

#[cfg(test)]
mod tests {
    use super::rebase;

    #[test]
    fn rebases_root_relative_attributes_only() {
        let html = r##"<a href="/docs/">d</a><img src="/x.png"><meta name="m" content="/a.js"><a href="//cdn.example">c</a><a href="https://x/y">e</a><a href="#top">t</a><p data-x="/keep">/text</p>"##;
        assert_eq!(
            rebase(html, "/Repo"),
            r##"<a href="/Repo/docs/">d</a><img src="/Repo/x.png"><meta name="m" content="/Repo/a.js"><a href="//cdn.example">c</a><a href="https://x/y">e</a><a href="#top">t</a><p data-x="/keep">/text</p>"##
        );
        assert_eq!(rebase(html, ""), html);
    }
}
