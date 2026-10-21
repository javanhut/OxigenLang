//! The shell every page shares: the head (titles, canonical URL, social cards,
//! structured data), the header, the search dialog and the footer.

use crate::config::Config;
use crate::html::escape;
use serde_json::Value;

pub struct Assets {
    pub css: String,
    pub js: String,
    /// The released version, which the structured data names.
    pub version: String,
    /// The path the site is served under, such as `/OxigenLang`; empty at a
    /// domain's root. The scripts read it from `<html data-base>`, which
    /// `html::rebase` leaves alone.
    pub base: String,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Nav {
    Home,
    Docs,
    Stdlib,
    Playground,
    Download,
    Benchmarks,
    About,
    None,
}

pub struct Page {
    /// The site path, such as `/docs/functions/`.
    pub path: String,
    /// The whole `<title>`.
    pub title: String,
    pub description: String,
    pub article: bool,
    pub nav: Nav,
    pub body: String,
    /// Extra JSON-LD nodes for this page; the site-wide graph is always added.
    pub json_ld: Vec<Value>,
    /// Extra tags for the head.
    pub head: String,
    /// Class on `<body>`, for page-specific layouts.
    pub class: &'static str,
}

pub fn render(config: &Config, assets: &Assets, page: &Page, year: i32) -> String {
    let url = config.abs(&page.path);
    let title = escape(&page.title);
    let description = escape(&page.description);
    let author = escape(&config.author.name);
    let mut graph = crate::seo::site_graph(config, &assets.version);
    graph.extend(page.json_ld.iter().cloned());
    let json_ld = serde_json::json!({ "@context": "https://schema.org", "@graph": graph })
        .to_string()
        .replace("</", "<\\/");
    let og_type = if page.article { "article" } else { "website" };
    // The 404 page is served for every missing URL; it must not be indexed as one.
    let robots = if page.path == "/404.html" {
        "noindex, follow"
    } else {
        "index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1"
    };
    let og_image = config.abs("/og.png");
    let nav = nav(page.nav);
    let repo = &config.repo;
    let body = &page.body;
    let head = &page.head;
    let class = page.class;
    let css = &assets.css;
    let base = escape(&assets.base);
    let js = &assets.js;
    let article_meta = if page.article {
        format!(
            "\n<meta property=\"article:author\" content=\"{}\">",
            escape(&config.author.url)
        )
    } else {
        String::new()
    };
    format!(
        r##"<!doctype html>
<html lang="en" data-base="{base}">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<meta name="description" content="{description}">
<meta name="author" content="{author}">
<meta name="creator" content="{author}">
<meta name="robots" content="{robots}">
<link rel="canonical" href="{url}">
<meta property="og:type" content="{og_type}">
<meta property="og:site_name" content="OxigenLang">
<meta property="og:title" content="{title}">
<meta property="og:description" content="{description}">
<meta property="og:url" content="{url}">
<meta property="og:image" content="{og_image}">
<meta property="og:image:width" content="1200">
<meta property="og:image:height" content="630">
<meta property="og:image:alt" content="OxigenLang, a programming language created by Javan Hutchinson">
<meta property="og:locale" content="en_US">{article_meta}
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="{title}">
<meta name="twitter:description" content="{description}">
<meta name="twitter:image" content="{og_image}">
<meta name="theme-color" content="#111820">
<link rel="preload" href="/fonts/geist-latin.woff2" as="font" type="font/woff2" crossorigin>
<link rel="preload" href="/fonts/geist-mono-latin.woff2" as="font" type="font/woff2" crossorigin>
<link rel="icon" href="/favicon.svg" type="image/svg+xml">
<link rel="icon" href="/favicon-32.png" sizes="32x32" type="image/png">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
<link rel="manifest" href="/site.webmanifest">
<link rel="alternate" type="text/plain" title="OxigenLang for language models" href="/llms.txt">
<link rel="sitemap" type="application/xml" href="/sitemap.xml">
<link rel="stylesheet" href="/assets/site.css?v={css}">{head}
<script>try{{var t=localStorage.getItem("theme");if(t)document.documentElement.dataset.theme=t}}catch(e){{}}</script>
<script type="application/ld+json">{json_ld}</script>
</head>
<body class="{class}">
<a class="skip" href="#main">Skip to content</a>
<header class="site-header">
  <div class="header-inner">
    <a class="brand" href="/" aria-label="OxigenLang home">{logo}<span class="brand-name">Oxigen<span>Lang</span></span></a>
    <nav class="site-nav" aria-label="Main">{nav}</nav>
    <div class="header-tools">
      <button class="search-trigger" type="button" aria-label="Search the documentation" data-search-open>
        <svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5"/></svg><span>Search</span><kbd>/</kbd>
      </button>
      <button class="icon-btn theme-toggle" type="button" aria-label="Toggle colour theme" data-theme-toggle>
        <svg class="i-sun" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/></svg>
        <svg class="i-moon" viewBox="0 0 24 24" aria-hidden="true"><path d="M20 14.5A8 8 0 0 1 9.5 4 8 8 0 1 0 20 14.5z"/></svg>
      </button>
      <a class="icon-btn" href="{repo}" aria-label="OxigenLang on GitHub" rel="noopener">{github}</a>
      <button class="icon-btn menu-toggle" type="button" aria-label="Open menu" aria-expanded="false" data-menu-toggle>
        <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 7h16M4 12h16M4 17h16"/></svg>
      </button>
    </div>
  </div>
</header>
<main id="main">
{body}
</main>
<footer class="site-footer">
  <div class="footer-inner">
    <div class="footer-brand">
      <a class="brand" href="/">{logo}<span class="brand-name">Oxigen<span>Lang</span></span></a>
      <p>A general-purpose programming language by <a href="/about/" rel="author">{author}</a>.</p>
    </div>
    <div class="footer-cols">
      <div><h2>Learn</h2><a href="/docs/getting-started/">Getting started</a><a href="/docs/syntax/">Syntax overview</a><a href="/playground/">Playground</a></div>
      <div><h2>Download</h2><a href="/download/">Downloads</a><a href="{repo}/releases" rel="noopener">Releases</a><a href="/benchmarks/">Benchmarks</a></div>
      <div><h2>Documentation</h2><a href="/docs/">Language guide</a><a href="/docs/stdlib/">Standard library</a><a href="/docs/embedding/">Embedding</a></div>
      <div><h2>Get involved</h2><a href="{repo}" rel="noopener">Source code</a><a href="{repo}/issues" rel="noopener">Issues</a><a href="/about/">About</a><a href="/llms.txt">llms.txt</a></div>
    </div>
  </div>
  <p class="footer-legal">© {year} {author}. OxigenLang is open source under the MIT or Apache-2.0 license.</p>
</footer>
<div class="search-dialog" hidden data-search>
  <div class="search-panel" role="dialog" aria-modal="true" aria-label="Search the documentation">
    <div class="search-field"><svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5"/></svg><input type="search" placeholder="Search the docs" aria-label="Search" autocomplete="off" spellcheck="false" data-search-input><kbd>Esc</kbd></div>
    <ol class="search-results" data-search-results></ol>
  </div>
</div>
<script src="/assets/site.js?v={js}" defer></script>
</body>
</html>
"##,
        logo = LOGO,
        github = GITHUB,
    )
}

fn nav(active: Nav) -> String {
    let items = [
        (Nav::Docs, "/docs/", "Docs"),
        (Nav::Stdlib, "/docs/stdlib/", "Library"),
        (Nav::Playground, "/playground/", "Playground"),
        (Nav::Download, "/download/", "Download"),
        (Nav::Benchmarks, "/benchmarks/", "Benchmarks"),
        (Nav::About, "/about/", "About"),
    ];
    items
        .iter()
        .map(|(nav, href, label)| {
            let current = if *nav == active {
                " aria-current=\"page\""
            } else {
                ""
            };
            format!("<a href=\"{href}\"{current}>{label}</a>")
        })
        .collect()
}

pub const LOGO: &str = r##"<svg class="logo" viewBox="0 0 32 32" aria-hidden="true"><circle cx="14" cy="18" r="10" fill="none" stroke="currentColor" stroke-width="4"/><circle cx="26.5" cy="5.5" r="3.5" fill="#4fa3b8"/></svg>"##;

const GITHUB: &str = r##"<svg viewBox="0 0 24 24" aria-hidden="true" class="fill"><path d="M12 .5a11.5 11.5 0 0 0-3.64 22.41c.58.1.79-.25.79-.56v-2c-3.2.7-3.88-1.37-3.88-1.37-.53-1.33-1.28-1.69-1.28-1.69-1.05-.71.08-.7.08-.7 1.15.08 1.76 1.19 1.76 1.19 1.03 1.76 2.7 1.25 3.36.96.1-.75.4-1.25.73-1.54-2.56-.29-5.25-1.28-5.25-5.69 0-1.26.45-2.29 1.19-3.1-.12-.29-.52-1.46.11-3.05 0 0 .97-.31 3.17 1.18a11 11 0 0 1 5.77 0c2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.76.11 3.05.74.81 1.19 1.84 1.19 3.1 0 4.42-2.7 5.39-5.27 5.68.41.36.78 1.06.78 2.14v3.17c0 .31.21.67.8.56A11.5 11.5 0 0 0 12 .5z"/></svg>"##;
