//! The hand-written pages in website/pages, and the documentation page
//! layout around each rendered doc.
//!
//! A page is HTML with a TOML header between `+++` lines (`title`,
//! `description`, `path`, `nav`, `class`) and `{{...}}` placeholders the
//! build fills in: `{{example:NAME}}` for a code card from website/examples,
//! `{{bench_chart}}`, `{{docs_index}}` and the rest listed in `fill`.

use crate::bench::Report;
use crate::config::Config;
use crate::html::{escape, oxi_block};
use crate::layout::{Nav, Page};
use crate::markdown::{Doc, Rendered};
use crate::release::Release;
use oxigen_playground::Outcome;
use serde_json::json;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

pub struct Example {
    /// The file name without its order prefix: `01-hello.oxi` is `hello`.
    pub slug: String,
    /// The first line's comment.
    pub title: String,
    /// The second line's comment.
    pub summary: String,
    pub source: String,
    pub output: String,
}

pub struct Context<'a> {
    pub config: &'a Config,
    pub docs: &'a [Doc],
    pub report: &'a Report,
    pub examples: &'a [Example],
    /// website/snippets: shown, not run (they need threads or the test runner).
    pub snippets: &'a [(String, String)],
    /// The latest GitHub release; none when built with --offline.
    pub release: Option<&'a Release>,
}

/// website/examples, in file-name order. Each must run in the playground's
/// sandbox: a showcase that fails is a build error, not a broken page.
pub fn load_examples(dir: &Path) -> Result<Vec<Example>, String> {
    let mut paths: Vec<_> = std::fs::read_dir(dir)
        .map_err(|e| format!("{}: {e}", dir.display()))?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|e| e == "oxi"))
        .collect();
    paths.sort();
    let mut examples = Vec::new();
    for path in paths {
        let source =
            std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        let comments: Vec<&str> = source
            .lines()
            .filter(|l| !l.starts_with("#["))
            .take(2)
            .map(|l| l.trim_start_matches("//").trim())
            .collect();
        // The title and summary comments are shown beside the code, so the
        // code shown (and run) starts after them.
        let mut lines: Vec<&str> = source.lines().collect();
        let first_comment = lines.iter().position(|l| l.starts_with("//")).unwrap_or(0);
        let mut taken = 0;
        while taken < 2
            && lines
                .get(first_comment)
                .is_some_and(|l| l.starts_with("//"))
        {
            lines.remove(first_comment);
            taken += 1;
        }
        let source = lines.join("\n").trim_start_matches('\n').to_string() + "\n";
        let stem = path.file_stem().unwrap().to_string_lossy();
        let slug = stem.split_once('-').map_or(&*stem, |(_, s)| s).to_string();

        let printed = Rc::new(RefCell::new(String::new()));
        let into = Rc::clone(&printed);
        oxigen_core::output::set_sink(Some(Box::new(move |t| into.borrow_mut().push_str(t))));
        let outcome = oxigen_playground::run(&source, oxigen_playground::LOOP_BUDGET);
        oxigen_core::output::set_sink(None);
        if let Outcome::Failed(error) = outcome {
            return Err(format!(
                "{}: the example fails in the playground:\n{error}",
                path.display()
            ));
        }
        let output = printed.borrow().clone();
        examples.push(Example {
            slug,
            title: comments.first().copied().unwrap_or("Example").to_string(),
            summary: comments.get(1).copied().unwrap_or("").to_string(),
            source,
            output,
        });
    }
    Ok(examples)
}

/// website/snippets, by file stem. Each must at least compile.
pub fn load_snippets(dir: &Path) -> Result<Vec<(String, String)>, String> {
    let mut snippets = Vec::new();
    for entry in std::fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))? {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.extension().is_none_or(|e| e != "oxi") {
            continue;
        }
        let source =
            std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        let diagnostics = oxigen_playground::check(&source);
        if diagnostics.contains("\"severity\":\"error\"") {
            return Err(format!(
                "{}: does not compile: {diagnostics}",
                path.display()
            ));
        }
        snippets.push((
            path.file_stem().unwrap().to_string_lossy().into_owned(),
            source,
        ));
    }
    Ok(snippets)
}

pub fn load(path: &Path, cx: &Context) -> Result<Page, String> {
    let text = std::fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    let rest = text
        .strip_prefix("+++\n")
        .ok_or_else(|| format!("{}: no +++ header", path.display()))?;
    let (header, body) = rest
        .split_once("\n+++\n")
        .ok_or_else(|| format!("{}: unterminated +++ header", path.display()))?;
    let header: toml::Table = header
        .parse()
        .map_err(|e| format!("{}: {e}", path.display()))?;
    let get = |key: &str| {
        header
            .get(key)
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string()
    };
    let page_path = get("path");
    let nav = match get("nav").as_str() {
        "home" => Nav::Home,
        "docs" => Nav::Docs,
        "playground" => Nav::Playground,
        "download" => Nav::Download,
        "benchmarks" => Nav::Benchmarks,
        "about" => Nav::About,
        _ => Nav::None,
    };
    let class = match get("class").as_str() {
        "page-home" => "page-home",
        "page-playground" => "page-playground",
        "page-about" => "page-about",
        "page-bench" => "page-bench",
        "page-download" => "page-download",
        "page-docs-index" => "page-docs-index",
        _ => "page-plain",
    };
    let json_ld = page_graph(cx.config, &page_path, &get("title"), &get("description"));
    Ok(Page {
        body: fill(body, cx, path)?,
        title: get("title"),
        description: get("description"),
        path: page_path,
        article: false,
        nav,
        json_ld,
        head: String::new(),
        class,
    })
}

fn fill(body: &str, cx: &Context, path: &Path) -> Result<String, String> {
    let mut out = String::with_capacity(body.len() * 2);
    let mut rest = body;
    while let Some(start) = rest.find("{{") {
        out.push_str(&rest[..start]);
        let end = rest[start..]
            .find("}}")
            .ok_or_else(|| format!("{}: unclosed {{{{", path.display()))?
            + start;
        let key = rest[start + 2..end].trim();
        out.push_str(
            &placeholder(key, cx)
                .ok_or_else(|| format!("{}: unknown placeholder {{{{{key}}}}}", path.display()))?,
        );
        rest = &rest[end + 2..];
    }
    out.push_str(rest);
    Ok(out)
}

fn placeholder(key: &str, cx: &Context) -> Option<String> {
    let example = |slug: &str| cx.examples.iter().find(|e| e.slug == slug);
    let r = cx.report;
    Some(match key {
        "version" => cx
            .release
            .map_or(env!("CARGO_PKG_VERSION"), |r| &r.version)
            .to_string(),
        "install_command" => format!("curl -fsSL {} | sh", cx.config.abs("/install.sh")),
        "release_summary" => match cx.release {
            Some(r) => format!(
                "Version {}, released {}.",
                escape(&r.version),
                long_date(&r.date)
            ),
            None => format!(
                "The latest version is on the <a href=\"{}/releases/latest\">releases page</a>.",
                escape(&cx.config.repo)
            ),
        },
        "release_url" => escape(
            &cx.release
                .map_or(format!("{}/releases/latest", cx.config.repo), |r| {
                    r.url.clone()
                }),
        ),
        "downloads_table" => downloads_table(cx),
        "downloads_json" => {
            let files: Vec<_> = cx
                .release
                .into_iter()
                .flat_map(|r| &r.files)
                .map(|f| {
                    let (os, cpu) = f.label();
                    json!({ "platform": f.platform, "os": os, "cpu": cpu, "name": f.name, "size": size(f.size), "url": f.url })
                })
                .collect();
            serde_json::to_string(&files).unwrap().replace("</", "<\\/")
        }
        "author" => escape(&cx.config.author.name),
        "author_url" => escape(&cx.config.author.url),
        "repo" => escape(&cx.config.repo),
        "stdlib_count" => oxigen_playground::STDLIB.len().to_string(),
        "creator_portrait" => portrait(cx.config),
        "stdlib_list" => stdlib_list(cx),
        "docs_index" => docs_index(cx),
        "example_tabs" => example_tabs(cx),
        "playground_examples" => {
            let list: Vec<_> = cx.examples.iter().map(|e| json!({ "slug": e.slug, "title": e.title, "summary": e.summary, "source": e.source })).collect();
            serde_json::to_string(&list).unwrap().replace("</", "<\\/")
        }
        "playground_initial" => oxigen_core::highlight::to_html(&cx.examples.first()?.source),
        "playground_initial_raw" => escape(&cx.examples.first()?.source),
        "playground_options" => cx
            .examples
            .iter()
            .map(|e| {
                format!(
                    "<option value=\"{}\">{}</option>",
                    escape(&e.slug),
                    escape(&e.title)
                )
            })
            .collect(),
        "bench_chart" => r.chart(|p| cx.config.blob(p)),
        "bench_table" => r.table(),
        "bench_median" => format!("{:.0}", r.median_speedup(false)),
        "bench_max" => format!("{:.0}", r.max_speedup()),
        "bench_wins" => r.wins_over_python().to_string(),
        "bench_headline" => {
            if r.wins_over_python() == r.rows.len() {
                "Faster than CPython on every benchmark in the suite.".to_string()
            } else {
                format!(
                    "Faster than CPython on {} of {} benchmarks.",
                    r.wins_over_python(),
                    r.rows.len()
                )
            }
        }
        "bench_count" => r.rows.len().to_string(),
        "bench_date" => escape(&r.generated),
        "bench_platform" => escape(&r.platform),
        "bench_python" => escape(&r.python),
        "bench_node" => escape(&r.node),
        "bench_bun" => escape(&r.bun),
        "bench_oxigen" => escape(&r.oxigen),
        key => {
            if let Some(slug) = key.strip_prefix("example:") {
                oxi_block(&example(slug)?.source, true)
            } else if let Some(name) = key.strip_prefix("snippet:") {
                let (_, source) = cx.snippets.iter().find(|(n, _)| n == name)?;
                oxi_block(source, false)
            } else if let Some(slug) = key.strip_prefix("output:") {
                escape(example(slug)?.output.trim_end())
            } else {
                escape(&cx.config.blob(key.strip_prefix("blob:")?))
            }
        }
    })
}

/// Structured data for a hand-written page: the about page is the creator's
/// profile, with the questions people ask about the language.
fn page_graph(
    config: &Config,
    path: &str,
    title: &str,
    description: &str,
) -> Vec<serde_json::Value> {
    let url = config.abs(path);
    let mut graph = vec![json!({
        "@type": match path { "/about/" => "AboutPage", "/playground/" => "WebApplication", _ => "WebPage" },
        "@id": format!("{url}#page"),
        "url": url,
        "name": title,
        "description": description,
        "isPartOf": { "@id": config.abs("/#website") },
        "about": { "@id": config.abs("/#language") },
        "author": { "@id": crate::seo::person_id(config) },
    })];
    if path == "/playground/" {
        let page = graph[0].as_object_mut().unwrap();
        page.insert("applicationCategory".into(), json!("DeveloperApplication"));
        page.insert(
            "operatingSystem".into(),
            json!("Any (runs in the browser via WebAssembly)"),
        );
        page.insert(
            "offers".into(),
            json!({ "@type": "Offer", "price": "0", "priceCurrency": "USD" }),
        );
    }
    if path == "/about/" {
        let author = &config.author.name;
        let faq = [
            ("Who created OxigenLang?", format!("OxigenLang was created by {author}, who designed the language and develops its compiler, bytecode virtual machine, Cranelift JIT, standard library and tooling.")),
            ("What is OxigenLang?", config.description.clone()),
            ("What is OxigenLang written in?", "OxigenLang is implemented in Rust. Source code compiles to bytecode for a virtual machine, and a baseline JIT built on Cranelift compiles hot functions to native machine code. The language server is written in Go.".to_string()),
            ("Is OxigenLang free and open source?", format!("Yes. OxigenLang is open source under the MIT or Apache-2.0 license, at {}.", config.repo)),
            ("Can I try OxigenLang without installing it?", format!("Yes. The playground at {} runs OxigenLang in the browser, compiled to WebAssembly.", config.abs("/playground/"))),
            ("What file extension does OxigenLang use?", "OxigenLang source files use the .oxi extension and run with the oxigen command, for example `oxigen hello.oxi`.".to_string()),
        ];
        graph.push(json!({
            "@type": "FAQPage",
            "@id": format!("{url}#faq"),
            "mainEntity": faq.iter().map(|(q, a)| json!({ "@type": "Question", "name": q, "acceptedAnswer": { "@type": "Answer", "text": a } })).collect::<Vec<_>>(),
        }));
    }
    if path != "/" {
        graph.push(crate::seo::breadcrumbs(
            config,
            &[(title.split(" · ").next().unwrap_or(title), path)],
        ));
    }
    graph
}

/// `2026-08-20` as `20 August 2026`.
fn long_date(date: &str) -> String {
    const MONTHS: [&str; 12] = [
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December",
    ];
    let parts: Vec<&str> = date.split('-').collect();
    match parts.as_slice() {
        [y, m, d] => match m.parse::<usize>() {
            Ok(m @ 1..=12) => format!("{} {} {y}", d.trim_start_matches('0'), MONTHS[m - 1]),
            _ => date.to_string(),
        },
        _ => date.to_string(),
    }
}

fn size(bytes: u64) -> String {
    format!("{:.1} MB", bytes as f64 / 1_000_000.0)
}

/// Every file of the latest release, with its size and SHA-256.
fn downloads_table(cx: &Context) -> String {
    let Some(release) = cx.release.filter(|r| !r.files.is_empty()) else {
        return format!(
            "<p>The files are listed on the <a href=\"{}/releases/latest\" rel=\"noopener\">latest release</a>.</p>",
            escape(&cx.config.repo)
        );
    };
    let mut rows = String::new();
    for f in &release.files {
        let (os, cpu) = f.label();
        let sum = f.sha256.as_deref().map_or("<span class=\"muted\">not published</span>".to_string(), |s| {
            format!("<code class=\"sha\" title=\"{s}\">{}…</code><button class=\"btn-copy mini\" type=\"button\" data-copy=\"{s}\">Copy</button>", &s[..s.len().min(12)])
        });
        rows.push_str(&format!(
            "<tr data-platform=\"{p}\"><th scope=\"row\">{os}</th><td>{cpu}</td><td><a href=\"{url}\" download>{name}</a></td><td class=\"num\">{size}</td><td>{sum}</td></tr>",
            p = escape(&f.platform),
            url = escape(&f.url),
            name = escape(&f.name),
            size = size(f.size),
        ));
    }
    format!(
        "<div class=\"table-wrap\"><table class=\"downloads\"><thead><tr><th scope=\"col\">System</th><th scope=\"col\">CPU</th><th scope=\"col\">File</th><th scope=\"col\" class=\"num\">Size</th><th scope=\"col\">SHA-256</th></tr></thead><tbody>{rows}</tbody></table></div>"
    )
}

/// The author's photo, or a monogram until site.toml names one.
fn portrait(config: &Config) -> String {
    let name = escape(&config.author.name);
    match &config.author.photo {
        Some(photo) => format!(
            "<img src=\"{}\" alt=\"{name}\" width=\"480\" height=\"600\" loading=\"lazy\">",
            escape(photo)
        ),
        None => {
            let initials: String = config
                .author
                .name
                .split_whitespace()
                .filter_map(|w| w.chars().next())
                .collect();
            format!(
                "<div class=\"monogram\" role=\"img\" aria-label=\"{name}\"><span>{}</span></div>",
                escape(&initials)
            )
        }
    }
}

fn docs_index(cx: &Context) -> String {
    let mut html = String::new();
    for section in &cx.config.sections {
        let stdlib = section.title == "Standard library";
        html.push_str(&format!(
            "<section class=\"index-section\"><h2>{}</h2><div class=\"doc-list\">",
            escape(&section.title)
        ));
        for doc in cx.docs.iter().filter(|d| d.section == section.title) {
            let name = if stdlib && doc.file != "stdlib/README.md" {
                format!("<code>{}</code>", escape(&doc.title))
            } else {
                format!("<strong>{}</strong>", escape(&doc.title))
            };
            html.push_str(&format!(
                "<a href=\"{}\">{name}<span>{}</span></a>",
                doc.url,
                escape(&doc.description)
            ));
        }
        html.push_str("</div></section>");
    }
    html
}

fn stdlib_list(cx: &Context) -> String {
    cx.docs
        .iter()
        .filter(|d| d.file.starts_with("stdlib/") && d.file != "stdlib/README.md")
        .map(|d| {
            format!(
                "<a href=\"{}\"><code>{}</code></a>",
                d.url,
                escape(&d.title)
            )
        })
        .collect()
}

/// The home page's examples: a numbered index beside the selected one.
fn example_tabs(cx: &Context) -> String {
    let mut tabs = String::new();
    let mut panels = String::new();
    for (i, e) in cx.examples.iter().enumerate() {
        let selected = i == 0;
        tabs.push_str(&format!(
            "<button role=\"tab\" type=\"button\" id=\"tab-{slug}\" aria-controls=\"panel-{slug}\" aria-selected=\"{selected}\" tabindex=\"{tabindex}\"><span>{n:02}</span><span>{title}</span></button>",
            slug = escape(&e.slug),
            n = i + 1,
            title = escape(&e.title),
            tabindex = if selected { 0 } else { -1 },
        ));
        panels.push_str(&format!(
            "<div role=\"tabpanel\" id=\"panel-{slug}\" aria-labelledby=\"tab-{slug}\"{hidden}><p class=\"example-summary\">{summary}</p>{card}</div>",
            slug = escape(&e.slug),
            hidden = if selected { "" } else { " hidden" },
            summary = escape(&e.summary),
            card = oxi_block(&e.source, true),
        ));
    }
    format!(
        "<div class=\"examples\" data-tabs><div class=\"example-index\" role=\"tablist\" aria-label=\"Examples\" aria-orientation=\"vertical\">{tabs}</div><div>{panels}</div></div>"
    )
}

/// The documentation page: sidebar, the rendered doc, its table of contents,
/// and links to the pages either side.
pub fn doc_page(config: &Config, docs: &[Doc], index: usize, rendered: &Rendered) -> String {
    let doc = &docs[index];
    let mut sidebar = String::new();
    for section in &config.sections {
        let open = section.title == doc.section;
        sidebar.push_str(&format!(
            "<details class=\"nav-group\"{}><summary>{}</summary><ul>",
            if open { " open" } else { "" },
            escape(&section.title)
        ));
        for d in docs.iter().filter(|d| d.section == section.title) {
            let current = if d.url == doc.url {
                " aria-current=\"page\""
            } else {
                ""
            };
            sidebar.push_str(&format!(
                "<li><a href=\"{}\"{current}>{}</a></li>",
                d.url,
                escape(&d.title)
            ));
        }
        sidebar.push_str("</ul></details>");
    }

    let toc: String = rendered
        .headings
        .iter()
        .filter(|h| h.level == 2 || h.level == 3)
        .map(|h| {
            format!(
                "<li class=\"toc-h{}\"><a href=\"#{}\">{}</a></li>",
                h.level,
                h.id,
                escape(&h.text)
            )
        })
        .collect();
    let toc = if toc.is_empty() {
        String::new()
    } else {
        format!(
            "<aside class=\"docs-toc\" aria-label=\"On this page\"><p class=\"toc-title\">On this page</p><ol>{toc}</ol></aside>"
        )
    };

    let pager_link = |d: Option<&Doc>, rel: &str, label: &str| match d {
        Some(d) => format!(
            "<a class=\"pager-{rel}\" href=\"{}\" rel=\"{rel}\"><span>{label}</span>{}</a>",
            d.url,
            escape(&d.title)
        ),
        None => "<span></span>".to_string(),
    };
    let prev = index.checked_sub(1).map(|i| &docs[i]);
    let next = docs.get(index + 1);
    let section_first = docs
        .iter()
        .find(|d| d.section == doc.section)
        .map(|d| d.url.as_str())
        .unwrap_or("/docs/");

    format!(
        "<div class=\"docs-layout\">\
<button class=\"docs-nav-toggle\" type=\"button\" aria-expanded=\"false\" data-docs-nav-toggle>{section} <span aria-hidden=\"true\">▾</span></button>\
<nav class=\"docs-sidebar\" aria-label=\"Documentation\">{sidebar}</nav>\
<article class=\"docs-content\">\
<nav class=\"breadcrumbs\" aria-label=\"Breadcrumb\"><a href=\"/docs/\">Docs</a><span>/</span><a href=\"{section_first}\">{section}</a></nav>\
<div class=\"prose\">{body}</div>\
<div class=\"doc-meta\"><a href=\"{edit}\" rel=\"noopener\">Edit this page on GitHub</a><a href=\"{md}index.md\">View as Markdown</a></div>\
<nav class=\"pager\" aria-label=\"Pages\">{prev}{next}</nav>\
</article>{toc}</div>",
        section = escape(&doc.section),
        body = rendered.html,
        edit = escape(
            &config
                .blob(&format!("docs/{}", doc.file))
                .replace("/blob/", "/edit/")
        ),
        md = doc.url,
        prev = pager_link(prev, "prev", "Previous"),
        next = pager_link(next, "next", "Next"),
    )
}
