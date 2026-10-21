//! Builds oxigenlang.com into a directory of static files.
//!
//!     oxigen-site [--out DIR] [--wasm FILE] [--offline]
//!
//! Run from the repository root (`imlazy site` does). The documentation comes
//! from docs/, the hand-written pages, examples, styles and scripts from
//! website/, the benchmark figures from benchmark_reports/, and the
//! playground's interpreter from the WebAssembly build (`imlazy wasm`), and the
//! download page's files from the latest GitHub release (`--offline` skips
//! that, and the page then links to the releases list instead).

mod bench;
mod config;
mod html;
mod layout;
mod markdown;
mod pages;
mod release;
mod seo;

use config::Config;
use layout::{Assets, Nav, Page};
use markdown::{Doc, Renderer};
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_WASM: &str = "target/wasm32-unknown-unknown/wasm/oxigen_playground.wasm";

fn main() {
    let mut out = PathBuf::from("target/site");
    let mut wasm = PathBuf::from(DEFAULT_WASM);
    let mut offline = false;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--out" => out = args.next().map(PathBuf::from).unwrap_or_else(|| usage()),
            "--wasm" => wasm = args.next().map(PathBuf::from).unwrap_or_else(|| usage()),
            "--offline" => offline = true,
            _ => usage(),
        }
    }
    // Docs examples run in-process; give the interpreter the stack the CLI gives it.
    let build = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(move || build(&out, &wasm, offline))
        .expect("spawn build thread");
    match build.join() {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            eprintln!("oxigen-site: {e}");
            std::process::exit(1);
        }
        Err(_) => std::process::exit(1),
    }
}

fn usage() -> ! {
    eprintln!("usage: oxigen-site [--out DIR] [--wasm FILE] [--offline]");
    std::process::exit(2);
}

fn read(path: impl AsRef<Path>) -> Result<String, String> {
    let path = path.as_ref();
    fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))
}

fn write(out: &Path, path: &str, contents: impl AsRef<[u8]>) -> Result<(), String> {
    let file = out.join(path.trim_start_matches('/'));
    if let Some(dir) = file.parent() {
        fs::create_dir_all(dir).map_err(|e| format!("{}: {e}", dir.display()))?;
    }
    fs::write(&file, contents).map_err(|e| format!("{}: {e}", file.display()))
}

/// `/docs/x/` is written as `/docs/x/index.html`.
fn write_page(out: &Path, path: &str, html: &str) -> Result<(), String> {
    let file = if path.ends_with('/') {
        format!("{path}index.html")
    } else {
        path.to_string()
    };
    write(out, &file, html)
}

fn build(out: &Path, wasm: &Path, offline: bool) -> Result<(), String> {
    let config = Config::load(Path::new("website/site.toml"))?;
    let latest = if offline {
        None
    } else {
        let repo = config.repo.trim_start_matches("https://github.com/");
        let latest = release::latest(repo)
            .map_err(|e| format!("reading the latest release: {e} (--offline skips it)"))?;
        Some(latest)
    };
    let year = year();
    if out.exists() {
        fs::remove_dir_all(out).map_err(|e| format!("{}: {e}", out.display()))?;
    }

    // ── Static files and assets ──
    copy_dir(Path::new("website/static"), out)?;
    let wasm_bytes = fs::read(wasm)
        .map_err(|e| format!("{}: {e} (build it with `imlazy wasm`)", wasm.display()))?;
    let wasm_url = format!("/assets/oxigen.wasm?v={}", html::fingerprint(&wasm_bytes));
    write(out, "/assets/oxigen.wasm", &wasm_bytes)?;
    let mut fingerprints = Vec::new();
    for name in [
        "site.css",
        "site.js",
        "playground.js",
        "runner.js",
        "worker.js",
    ] {
        let bytes = fs::read(Path::new("website/assets").join(name))
            .map_err(|e| format!("website/assets/{name}: {e}"))?;
        fingerprints.push((name, html::fingerprint(&bytes)));
        write(out, &format!("/assets/{name}"), bytes)?;
    }
    let print = |name: &str| {
        fingerprints
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, f)| f.clone())
            .unwrap()
    };
    let assets = Assets {
        css: print("site.css"),
        js: print("site.js"),
        version: latest
            .as_ref()
            .map_or(env!("CARGO_PKG_VERSION").to_string(), |r| r.version.clone()),
    };
    // site.js loads the runner on demand; it needs the runner's and the module's addresses.
    let runtime_head = format!(
        "\n<meta name=\"oxigen-wasm\" content=\"{wasm_url}\">\n<meta name=\"oxigen-runner\" content=\"/assets/runner.js?v={}\">\n<meta name=\"oxigen-worker\" content=\"/assets/worker.js?v={}\">",
        print("runner.js"),
        print("worker.js"),
    );

    // ── Documentation ──
    let mut docs = Vec::new();
    for section in &config.sections {
        for page in &section.pages {
            let source = read(Path::new("docs").join(&page.file))?;
            let heading = markdown::first_heading(&source)
                .ok_or_else(|| format!("docs/{}: no `# ` heading", page.file))?;
            docs.push(Doc {
                file: page.file.clone(),
                url: format!("/docs/{}/", page.slug),
                title: page.title.clone().unwrap_or_else(|| heading.clone()),
                description: page
                    .description
                    .clone()
                    .unwrap_or_else(|| markdown::first_paragraph(&source)),
                heading,
                section: section.title.clone(),
                source,
            });
        }
    }
    let renderer = Renderer::new(&config, &docs);
    let mut sitemap: Vec<(String, f32)> = vec![
        ("/".into(), 1.0),
        ("/download/".into(), 0.9),
        ("/playground/".into(), 0.9),
        ("/docs/".into(), 0.9),
        ("/about/".into(), 0.8),
        ("/benchmarks/".into(), 0.7),
    ];
    let mut search = Vec::new();
    let mut full = format!(
        "# OxigenLang documentation\n\nOxigenLang is a programming language created by {}. Website: {}. Source: {}.\n\n",
        config.author.name, config.url, config.repo
    );
    for (i, doc) in docs.iter().enumerate() {
        let rendered = renderer.render(doc);
        let section_url = docs
            .iter()
            .find(|d| d.section == doc.section)
            .map(|d| d.url.clone())
            .unwrap_or_default();
        let body = pages::doc_page(&config, &docs, i, &rendered);
        let page = Page {
            path: doc.url.clone(),
            title: if doc.file.starts_with("stdlib/") && doc.file != "stdlib/README.md" {
                format!("{} module · OxigenLang standard library", doc.title)
            } else {
                format!("{} · OxigenLang documentation", doc.title)
            },
            description: doc.description.clone(),
            article: true,
            nav: if doc.file.starts_with("stdlib/") {
                Nav::Stdlib
            } else {
                Nav::Docs
            },
            body,
            json_ld: seo::doc_graph(&config, doc, &section_url),
            head: format!(
                "{runtime_head}\n<link rel=\"alternate\" type=\"text/markdown\" href=\"{}index.md\">",
                doc.url
            ),
            class: "page-docs",
        };
        write_page(
            out,
            &doc.url,
            &layout::render(&config, &assets, &page, year),
        )?;
        let markdown = absolute_links(&renderer, doc);
        write(out, &format!("{}index.md", doc.url), &markdown)?;
        full.push_str(&format!(
            "\n\n---\n\nSource: {}\n\n{}\n",
            config.abs(&doc.url),
            markdown.trim()
        ));
        sitemap.push((
            doc.url.clone(),
            if doc.file.starts_with("stdlib/") {
                0.6
            } else {
                0.8
            },
        ));
        search.extend(rendered.search);
    }

    // ── Hand-written pages ──
    let report = bench::parse(&read("benchmark_reports/latest-native.md")?)?;
    let examples = pages::load_examples(Path::new("website/examples"))?;
    let snippets = pages::load_snippets(Path::new("website/snippets"))?;
    let context = pages::Context {
        config: &config,
        docs: &docs,
        report: &report,
        examples: &examples,
        snippets: &snippets,
        release: latest.as_ref(),
    };
    for entry in fs::read_dir("website/pages").map_err(|e| format!("website/pages: {e}"))? {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.extension().is_none_or(|e| e != "html") {
            continue;
        }
        let mut page = pages::load(&path, &context)?;
        page.head.push_str(&runtime_head);
        if page.path == "/playground/" {
            // A module, so its top-level names stay out of site.js's global scope.
            page.head.push_str(&format!(
                "\n<script type=\"module\" src=\"/assets/playground.js?v={}\"></script>",
                print("playground.js")
            ));
        }
        write_page(
            out,
            &page.path,
            &layout::render(&config, &assets, &page, year),
        )?;
    }

    // ── For crawlers ──
    write(out, "/sitemap.xml", seo::sitemap(&config, &sitemap))?;
    write(out, "/robots.txt", seo::robots(&config))?;
    let mut by_section: Vec<(String, Vec<&Doc>)> = Vec::new();
    for doc in &docs {
        match by_section.iter_mut().find(|(t, _)| *t == doc.section) {
            Some((_, list)) => list.push(doc),
            None => by_section.push((doc.section.clone(), vec![doc])),
        }
    }
    write(out, "/llms.txt", seo::llms_txt(&config, &by_section))?;
    write(out, "/llms-full.txt", full)?;
    write(out, "/site.webmanifest", seo::manifest())?;
    let search: Vec<serde_json::Value> = search
        .iter()
        .map(|e| serde_json::json!({ "t": e.title, "p": e.page, "u": e.url, "x": markdown::truncate(&e.text, 180) }))
        .collect();
    write(out, "/search.json", serde_json::to_string(&search).unwrap())?;
    // GitHub Pages: serve the custom domain, and do not run Jekyll over the files.
    write(
        out,
        "/CNAME",
        format!("{}\n", config.url.trim_start_matches("https://")),
    )?;
    write(out, "/.nojekyll", "")?;

    let stats = renderer.stats.borrow();
    println!(
        "Built {} pages into {} ({} of {} docs examples runnable in the browser).",
        docs.len() + 6,
        out.display(),
        stats.runnable,
        stats.examples
    );
    Ok(())
}

/// The Markdown of `doc` with its links made absolute, for /docs/x/index.md
/// and llms-full.txt, read outside the site.
fn absolute_links(renderer: &Renderer, doc: &Doc) -> String {
    let mut out = String::with_capacity(doc.source.len());
    let mut rest = doc.source.as_str();
    while let Some(start) = rest.find("](") {
        let (before, after) = rest.split_at(start + 2);
        out.push_str(before);
        let end = after.find(')').unwrap_or(after.len());
        let dest = &after[..end];
        // Only link targets: `](` also turns up in code, as in `fns[0](x)`.
        let is_link = !dest.contains(' ')
            && (dest.contains(".md") || dest.starts_with("./") || dest.starts_with("../"));
        if is_link {
            let resolved = renderer.resolve(&doc.file, dest);
            out.push_str(&if resolved.starts_with('/') {
                renderer.abs(&resolved)
            } else {
                resolved
            });
        } else {
            out.push_str(dest);
        }
        rest = &after[end..];
    }
    out.push_str(rest);
    out
}

fn copy_dir(from: &Path, to: &Path) -> Result<(), String> {
    for entry in fs::read_dir(from).map_err(|e| format!("{}: {e}", from.display()))? {
        let entry = entry.map_err(|e| e.to_string())?;
        let target = to.join(entry.file_name());
        if entry.path().is_dir() {
            copy_dir(&entry.path(), &target)?;
        } else {
            fs::create_dir_all(to).map_err(|e| e.to_string())?;
            fs::copy(entry.path(), &target)
                .map_err(|e| format!("{}: {e}", entry.path().display()))?;
        }
    }
    Ok(())
}

fn year() -> i32 {
    let days = oxigen_core::clock::since_epoch()
        .unwrap_or_default()
        .as_secs()
        / 86_400;
    // Days to civil year (Howard Hinnant's algorithm).
    let z = days as i64 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    (yoe + era * 400 + i64::from(month <= 2)) as i32
}
