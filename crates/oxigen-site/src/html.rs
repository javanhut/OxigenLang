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
    let run = if runnable {
        "<button class=\"btn-run\" type=\"button\" aria-label=\"Run this example\"><svg viewBox=\"0 0 16 16\" aria-hidden=\"true\"><path d=\"M4 2.5v11l9-5.5z\"/></svg>Run</button><button class=\"btn-open\" type=\"button\" aria-label=\"Open in the playground\">Playground</button>"
    } else {
        ""
    };
    format!(
        "<div class=\"code code-oxi{cls}\" data-lang=\"oxi\"><div class=\"code-bar\"><span class=\"code-lang\">Oxigen</span><span class=\"code-actions\">{run}<button class=\"btn-copy\" type=\"button\">Copy</button></span></div><pre><code class=\"language-oxi\">{}</code></pre></div>\n",
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
