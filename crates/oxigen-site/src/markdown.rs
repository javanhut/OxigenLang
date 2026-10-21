//! Renders docs/*.md into page bodies.
//!
//! Besides plain Markdown it rewrites links between docs to site URLs (and
//! links into the rest of the repository to GitHub), gives headings the ids
//! GitHub gives them so the docs' own `#anchor` links keep working,
//! highlights `oxi` blocks with the real lexer, and runs each `oxi` block in
//! the playground's sandbox: one that runs and prints gets a Run button.

use crate::config::Config;
use crate::html::escape;
use oxigen_playground::Outcome;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, HeadingLevel, Options, Parser, Tag, TagEnd};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Loop iterations a docs example may take to count as runnable. Far below
/// the playground's budget: an example is meant to finish at once.
const SNIPPET_BUDGET: u64 = 5_000_000;

pub struct Doc {
    /// Relative to docs/, such as `stdlib/math.md`.
    pub file: String,
    /// Such as `/docs/stdlib/math/`.
    pub url: String,
    /// The sidebar's name for the page.
    pub title: String,
    /// The page's first heading.
    pub heading: String,
    pub description: String,
    pub section: String,
    pub source: String,
}

pub struct Heading {
    pub level: u8,
    pub id: String,
    pub text: String,
}

pub struct SearchEntry {
    pub title: String,
    pub page: String,
    pub url: String,
    pub text: String,
}

pub struct Rendered {
    pub html: String,
    pub headings: Vec<Heading>,
    pub search: Vec<SearchEntry>,
}

#[derive(Default)]
pub struct Stats {
    pub examples: usize,
    pub runnable: usize,
}

pub struct Renderer<'a> {
    config: &'a Config,
    /// docs-relative file to site URL.
    urls: HashMap<String, String>,
    runnable: RefCell<HashMap<String, bool>>,
    pub stats: RefCell<Stats>,
}

impl<'a> Renderer<'a> {
    pub fn new(config: &'a Config, docs: &[Doc]) -> Self {
        Renderer {
            config,
            urls: docs
                .iter()
                .map(|d| (d.file.clone(), d.url.clone()))
                .collect(),
            runnable: RefCell::new(HashMap::new()),
            stats: RefCell::new(Stats::default()),
        }
    }

    pub fn render(&self, doc: &Doc) -> Rendered {
        let mut out: Vec<Event> = Vec::new();
        let mut headings = Vec::new();
        let mut search = Vec::new();
        let mut ids: HashMap<String, usize> = HashMap::new();

        let mut heading: Option<(HeadingLevel, Vec<Event>)> = None;
        let mut code: Option<(String, String)> = None;
        // The search entry being filled: the text under the latest heading.
        let mut entry = SearchEntry {
            title: doc.heading.clone(),
            page: doc.title.clone(),
            url: doc.url.clone(),
            text: String::new(),
        };

        for event in Parser::new_ext(&doc.source, options()) {
            if let Some((_, inner)) = &mut heading {
                if let Event::End(TagEnd::Heading(level)) = event {
                    let inner = std::mem::take(inner);
                    heading = None;
                    let text = plain_text(&inner);
                    let id = unique(&mut ids, github_slug(&text));
                    let mut body = String::new();
                    pulldown_cmark::html::push_html(&mut body, inner.into_iter());
                    let n = level as u8;
                    if n == 1 {
                        out.push(Event::Html(format!("<h1 id=\"{id}\">{body}</h1>\n").into()));
                    } else {
                        out.push(Event::Html(
                            format!("<h{n} id=\"{id}\"><a class=\"anchor\" href=\"#{id}\" aria-label=\"Link to this section\">#</a>{body}</h{n}>\n").into(),
                        ));
                    }
                    if n <= 3 {
                        if n > 1 {
                            push_entry(
                                &mut search,
                                std::mem::replace(
                                    &mut entry,
                                    SearchEntry {
                                        title: text.clone(),
                                        page: doc.title.clone(),
                                        url: format!("{}#{id}", doc.url),
                                        text: String::new(),
                                    },
                                ),
                            );
                        }
                        headings.push(Heading { level: n, id, text });
                    }
                } else {
                    inner.push(self.link(&doc.file, event));
                }
                continue;
            }
            if let Some((_, text)) = &mut code {
                match event {
                    Event::Text(t) => text.push_str(&t),
                    Event::End(TagEnd::CodeBlock) => {
                        let (lang, text) = code.take().unwrap();
                        out.push(Event::Html(self.code_block(&lang, &text).into()));
                    }
                    _ => {}
                }
                continue;
            }
            match event {
                Event::Start(Tag::Heading { level, .. }) => heading = Some((level, Vec::new())),
                Event::Start(Tag::CodeBlock(kind)) => {
                    let lang = match kind {
                        CodeBlockKind::Fenced(lang) => {
                            lang.split_whitespace().next().unwrap_or("").to_string()
                        }
                        CodeBlockKind::Indented => String::new(),
                    };
                    code = Some((lang, String::new()));
                }
                Event::Start(Tag::Table(_)) => {
                    out.push(Event::Html("<div class=\"table-wrap\">".into()));
                    out.push(event);
                }
                Event::End(TagEnd::Table) => {
                    out.push(event);
                    out.push(Event::Html("</div>".into()));
                }
                Event::Text(ref t) | Event::Code(ref t) => {
                    if entry.text.len() < 400 {
                        entry.text.push_str(t);
                        entry.text.push(' ');
                    }
                    out.push(event);
                }
                other => out.push(self.link(&doc.file, other)),
            }
        }
        push_entry(&mut search, entry);

        let mut html = String::new();
        pulldown_cmark::html::push_html(&mut html, out.into_iter());
        Rendered {
            html,
            headings,
            search,
        }
    }

    /// Point a link at the site: another doc becomes its page, a file
    /// elsewhere in the repository its GitHub page.
    fn link<'e>(&self, from: &str, event: Event<'e>) -> Event<'e> {
        let Event::Start(Tag::Link {
            link_type,
            dest_url,
            title,
            id,
        }) = event
        else {
            return event;
        };
        let dest_url = CowStr::from(self.resolve(from, &dest_url));
        Event::Start(Tag::Link {
            link_type,
            dest_url,
            title,
            id,
        })
    }

    pub fn abs(&self, path: &str) -> String {
        self.config.abs(path)
    }

    pub fn resolve(&self, from: &str, dest: &str) -> String {
        if dest.starts_with('#')
            || dest.contains("://")
            || dest.starts_with("mailto:")
            || dest.starts_with('/')
        {
            return dest.to_string();
        }
        let (path, anchor) = match dest.split_once('#') {
            Some((p, a)) => (p, format!("#{a}")),
            None => (dest, String::new()),
        };
        // Resolve against the linking doc's directory, inside the repository.
        let mut parts: Vec<&str> = std::iter::once("docs")
            .chain(
                from.rsplit_once('/')
                    .map(|(dir, _)| dir)
                    .into_iter()
                    .flat_map(|d| d.split('/')),
            )
            .collect();
        for segment in path.split('/') {
            match segment {
                "" | "." => {}
                ".." => {
                    if parts.pop().is_none() {
                        return dest.to_string();
                    }
                }
                s => parts.push(s),
            }
        }
        let repo_path = parts.join("/");
        if let Some(doc) = repo_path.strip_prefix("docs/")
            && let Some(url) = self.urls.get(doc)
        {
            return format!("{url}{anchor}");
        }
        format!("{}{anchor}", self.config.blob(&repo_path))
    }

    fn code_block(&self, lang: &str, text: &str) -> String {
        let text = text.strip_suffix('\n').unwrap_or(text);
        match lang {
            "oxi" | "oxigen" => {
                self.stats.borrow_mut().examples += 1;
                let runnable = self.is_runnable(text);
                if runnable {
                    self.stats.borrow_mut().runnable += 1;
                }
                crate::html::oxi_block(text, runnable)
            }
            "" | "text" | "output" => {
                format!(
                    "<div class=\"code code-plain\"><pre><code>{}</code></pre></div>\n",
                    escape(text)
                )
            }
            lang => format!(
                "<div class=\"code\" data-lang=\"{lang}\"><div class=\"code-bar\"><span class=\"code-lang\">{lang}</span><button class=\"btn-copy\" type=\"button\">Copy</button></div><pre><code class=\"language-{lang}\">{}</code></pre></div>\n",
                escape(text),
                lang = escape(lang),
            ),
        }
    }

    /// Whether `code` runs to completion in the sandbox and prints something.
    fn is_runnable(&self, code: &str) -> bool {
        if let Some(&known) = self.runnable.borrow().get(code) {
            return known;
        }
        let printed = Rc::new(RefCell::new(String::new()));
        let into = Rc::clone(&printed);
        oxigen_core::output::set_sink(Some(Box::new(move |t| into.borrow_mut().push_str(t))));
        // An interpreter panic only means the example is not offered; say nothing.
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let outcome = std::panic::catch_unwind(|| oxigen_playground::run(code, SNIPPET_BUDGET));
        std::panic::set_hook(hook);
        oxigen_core::output::set_sink(None);
        let runnable =
            matches!(outcome, Ok(Outcome::Finished)) && !printed.borrow().trim().is_empty();
        self.runnable
            .borrow_mut()
            .insert(code.to_string(), runnable);
        runnable
    }
}

fn options() -> Options {
    Options::ENABLE_TABLES
        | Options::ENABLE_STRIKETHROUGH
        | Options::ENABLE_FOOTNOTES
        | Options::ENABLE_TASKLISTS
}

fn push_entry(search: &mut Vec<SearchEntry>, mut entry: SearchEntry) {
    entry.text = entry.text.split_whitespace().collect::<Vec<_>>().join(" ");
    search.push(entry);
}

fn plain_text(events: &[Event]) -> String {
    let mut text = String::new();
    for event in events {
        if let Event::Text(t) | Event::Code(t) = event {
            text.push_str(t);
        }
    }
    text
}

/// The id GitHub gives a heading: lowercase, punctuation dropped, spaces
/// to hyphens.
pub fn github_slug(text: &str) -> String {
    text.trim()
        .to_lowercase()
        .chars()
        .filter_map(|c| match c {
            ' ' => Some('-'),
            c if c.is_alphanumeric() || c == '-' || c == '_' => Some(c),
            _ => None,
        })
        .collect()
}

fn unique(ids: &mut HashMap<String, usize>, slug: String) -> String {
    let count = ids.entry(slug.clone()).or_insert(0);
    let id = if *count == 0 {
        slug.clone()
    } else {
        format!("{slug}-{count}")
    };
    *count += 1;
    id
}

/// The first paragraph's text, cut to a meta description's length.
pub fn first_paragraph(source: &str) -> String {
    let mut text = String::new();
    let mut in_paragraph = false;
    for event in Parser::new_ext(source, options()) {
        match event {
            Event::Start(Tag::Paragraph) => in_paragraph = true,
            Event::End(TagEnd::Paragraph) if in_paragraph => break,
            Event::Text(t) | Event::Code(t) if in_paragraph => text.push_str(&t),
            Event::SoftBreak | Event::HardBreak if in_paragraph => text.push(' '),
            _ => {}
        }
    }
    truncate(&text, 158)
}

pub fn truncate(text: &str, max: usize) -> String {
    let text = text.split_whitespace().collect::<Vec<_>>().join(" ");
    if text.chars().count() <= max {
        return text;
    }
    let mut cut: String = text.chars().take(max).collect();
    if let Some(space) = cut.rfind(' ') {
        cut.truncate(space);
    }
    format!("{}…", cut.trim_end_matches([',', ';', ':', '.']))
}

/// The text of the first `# ` heading.
pub fn first_heading(source: &str) -> Option<String> {
    let mut inside = false;
    let mut text = String::new();
    for event in Parser::new_ext(source, options()) {
        match event {
            Event::Start(Tag::Heading {
                level: HeadingLevel::H1,
                ..
            }) => inside = true,
            Event::End(TagEnd::Heading(HeadingLevel::H1)) => return Some(text),
            Event::Text(t) | Event::Code(t) if inside => text.push_str(&t),
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slugs_match_github() {
        assert_eq!(github_slug("sin(x), cos(x), tan(x)"), "sinx-cosx-tanx");
        assert_eq!(
            github_slug("stream_get, stream_post, stream_put, stream_patch"),
            "stream_get-stream_post-stream_put-stream_patch"
        );
        assert_eq!(
            github_slug("read_chunk(stream, max, timeout_ms)"),
            "read_chunkstream-max-timeout_ms"
        );
        assert_eq!(github_slug("Safety invariants"), "safety-invariants");
    }

    #[test]
    fn descriptions_are_cut_on_a_word() {
        assert_eq!(truncate("one two three", 9), "one two…");
        assert_eq!(truncate("short", 9), "short");
    }
}
