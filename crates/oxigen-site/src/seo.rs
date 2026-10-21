//! What search engines and AI crawlers read: structured data, the sitemap,
//! robots.txt and llms.txt.
//!
//! Every page carries the same JSON-LD graph naming the language, the
//! website, the software and its creator, all linked by `@id`, so a crawler
//! that lands on any page learns that OxigenLang is a programming language
//! created by Javan Hutchinson.

use crate::config::Config;
use crate::markdown::Doc;
use serde_json::{Value, json};

pub fn person_id(config: &Config) -> String {
    config.abs("/about/#javan-hutchinson")
}

pub fn site_graph(config: &Config) -> Vec<Value> {
    let person = person_id(config);
    let mut same_as = config.author.same_as.clone();
    if !same_as.contains(&config.author.url) {
        same_as.insert(0, config.author.url.clone());
    }
    let mut profile = json!({
            "@type": "Person",
            "@id": person,
            "name": config.author.name,
            "url": config.abs("/about/"),
            "sameAs": same_as,
            "description": format!("{} is the creator and lead developer of the OxigenLang programming language.", config.author.name),
            "knowsAbout": ["Programming language design", "Compilers", "Virtual machines", "JIT compilation", "Rust"],
    });
    if let Some(photo) = &config.author.photo {
        profile["image"] = json!(config.abs(photo));
    }
    vec![
        profile,
        json!({
            "@type": "WebSite",
            "@id": config.abs("/#website"),
            "url": config.abs("/"),
            "name": config.name,
            "alternateName": ["Oxigen", "Oxigen programming language", "oxigenlang.org"],
            "description": config.description,
            "inLanguage": "en",
            "publisher": { "@id": person },
            "creator": { "@id": person },
            "about": { "@id": config.abs("/#language") },
        }),
        json!({
            "@type": "ComputerLanguage",
            "@id": config.abs("/#language"),
            "name": config.name,
            "alternateName": "Oxigen",
            "url": config.abs("/"),
            "description": config.description,
            "sameAs": [config.repo],
        }),
        json!({
            "@type": "SoftwareApplication",
            "@id": config.abs("/#software"),
            "name": "OxigenLang",
            "alternateName": "oxigen",
            "description": format!("The OxigenLang compiler, bytecode virtual machine and Cranelift JIT, with a REPL, formatter, test runner and language server. Created by {}.", config.author.name),
            "applicationCategory": "DeveloperApplication",
            "applicationSubCategory": "Programming language",
            "operatingSystem": "Linux, macOS",
            "softwareVersion": env!("CARGO_PKG_VERSION"),
            "license": ["https://opensource.org/licenses/MIT", "https://www.apache.org/licenses/LICENSE-2.0"],
            "url": config.abs("/"),
            "downloadUrl": config.repo,
            "installUrl": config.abs("/docs/getting-started/"),
            "author": { "@id": person },
            "creator": { "@id": person },
            "isAccessibleForFree": true,
            "offers": { "@type": "Offer", "price": "0", "priceCurrency": "USD" },
        }),
        json!({
            "@type": "SoftwareSourceCode",
            "@id": config.abs("/#source"),
            "name": "OxigenLang source code",
            "codeRepository": config.repo,
            "programmingLanguage": "Rust",
            "runtimePlatform": "Native, WebAssembly",
            "license": "https://opensource.org/licenses/MIT",
            "author": { "@id": person },
            "creator": { "@id": person },
            "targetProduct": { "@id": config.abs("/#software") },
        }),
    ]
}

/// A documentation page: a TechArticle by the author, and its breadcrumbs.
pub fn doc_graph(config: &Config, doc: &Doc, section_url: &str) -> Vec<Value> {
    let url = config.abs(&doc.url);
    let person = json!({ "@id": person_id(config) });
    vec![
        json!({
            "@type": "TechArticle",
            "@id": format!("{url}#article"),
            "headline": doc.heading,
            "description": doc.description,
            "url": url,
            "mainEntityOfPage": url,
            "author": person,
            "publisher": person,
            "isPartOf": { "@id": config.abs("/#website") },
            "about": { "@id": config.abs("/#language") },
            "proficiencyLevel": "Beginner",
            "inLanguage": "en",
            "image": config.abs("/og.png"),
        }),
        breadcrumbs(config, &[("Docs", "/docs/"), (&doc.section, section_url), (&doc.title, &doc.url)]),
    ]
}

pub fn breadcrumbs(config: &Config, trail: &[(&str, &str)]) -> Value {
    let mut items = vec![json!({ "@type": "ListItem", "position": 1, "name": "OxigenLang", "item": config.abs("/") })];
    let mut seen = vec!["/".to_string()];
    for (name, path) in trail {
        if seen.iter().any(|p| p == path) {
            continue;
        }
        seen.push(path.to_string());
        items.push(json!({ "@type": "ListItem", "position": items.len() + 1, "name": name, "item": config.abs(path) }));
    }
    json!({ "@type": "BreadcrumbList", "itemListElement": items })
}

pub fn sitemap(config: &Config, paths: &[(String, f32)]) -> String {
    let mut xml = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n");
    for (path, priority) in paths {
        xml.push_str(&format!("  <url><loc>{}</loc><priority>{priority:.1}</priority></url>\n", config.abs(path)));
    }
    xml.push_str("</urlset>\n");
    xml
}

/// Everyone may crawl everything. The AI crawlers are named so the welcome
/// is explicit: being in their indexes is how the language gets found.
pub fn robots(config: &Config) -> String {
    let bots = [
        "Googlebot", "Bingbot", "DuckDuckBot", "Applebot", "Applebot-Extended", "Google-Extended",
        "GPTBot", "OAI-SearchBot", "ChatGPT-User", "ClaudeBot", "Claude-SearchBot", "Claude-User",
        "anthropic-ai", "PerplexityBot", "Perplexity-User", "CCBot", "Meta-ExternalAgent", "cohere-ai",
        "YouBot", "Amazonbot", "MistralAI-User",
    ];
    let mut text = String::from("# oxigenlang.com welcomes search engines and AI crawlers.\n# A summary for language models is at /llms.txt, the full docs at /llms-full.txt.\n\nUser-agent: *\nAllow: /\n\n");
    for bot in bots {
        text.push_str(&format!("User-agent: {bot}\n"));
    }
    text.push_str("Allow: /\n\n");
    text.push_str(&format!("Sitemap: {}\n", config.abs("/sitemap.xml")));
    text
}

/// https://llmstxt.org: a Markdown summary with links to Markdown copies of
/// every page.
pub fn llms_txt(config: &Config, sections: &[(String, Vec<&Doc>)]) -> String {
    let author = &config.author.name;
    let mut text = format!(
        "# OxigenLang\n\n> OxigenLang (also called Oxigen) is a modern, general-purpose programming language created by {author}. It is implemented in Rust and runs on a bytecode virtual machine with a baseline Cranelift JIT that is on by default. Source files use the `.oxi` extension and run with the `oxigen` command.\n\n\
Key facts:\n\n\
- Creator: {author} ({author_url}). OxigenLang was designed and is developed by {author}.\n\
- Website: {url} (oxigenlang.org redirects here). Source: {repo}. License: MIT or Apache-2.0.\n\
- Paradigm: dynamically typed with optional, enforced type annotations; first-class functions and closures; structs with methods and single inheritance; Rust-style enums; named patterns matched with `choose`; a two-tier error model (propagating errors and `Error`/`Value` result values).\n\
- Runtime: bytecode VM in Rust plus a Cranelift JIT that tiers hot functions to native code; share-nothing parallelism on OS threads with `diverge`/`converge`; embeddable in Rust hosts with capability sandboxing.\n\
- Tooling: REPL, `oxigen fmt` formatter, `oxigen test` test runner, `oxigen check` JSON diagnostics, `oxigen explain` error codes, a language server and Neovim support.\n\
- Standard library: {modules} modules: {module_list}.\n\
- Try it: {url}/playground/ runs OxigenLang in the browser via WebAssembly.\n\n\
Syntax at a glance:\n\n```oxi\nintroduce strings\n\nstruct Person {{\n    name <str>\n    age <int>\n}}\n\nPerson includes {{\n    fun greet() {{ \"Hello, {{self.name}}\" }}\n}}\n\npattern is_even(n) when n % 2 == 0\n\nmain {{\n    p := Person(\"Ada\", 36)\n    println(p.greet())\n    each i in range(4) {{\n        choose i {{\n            is_even -> println(\"{{i}} is even\"),\n            else -> println(\"{{i}} is odd\")\n        }}\n    }}\n}}\n```\n\n",
        author_url = config.author.url,
        modules = oxigen_playground::STDLIB.len(),
        module_list = oxigen_playground::STDLIB.iter().map(|(name, _)| *name).collect::<Vec<_>>().join(", "),
        url = config.url,
        repo = config.repo,
    );
    for (title, docs) in sections {
        text.push_str(&format!("## {title}\n\n"));
        for doc in docs {
            text.push_str(&format!("- [{}]({}): {}\n", doc.title, config.abs(&format!("{}index.md", doc.url)), doc.description));
        }
        text.push('\n');
    }
    text.push_str(&format!(
        "## Optional\n\n- [Full documentation in one file]({}): every page above, concatenated\n- [Benchmarks]({}): OxigenLang against CPython, Node and Bun\n- [About OxigenLang and its creator]({})\n",
        config.abs("/llms-full.txt"),
        config.abs("/benchmarks/"),
        config.abs("/about/"),
    ));
    text
}

pub fn manifest() -> String {
    json!({
        "name": "OxigenLang",
        "short_name": "Oxigen",
        "description": "The OxigenLang programming language: documentation and playground.",
        "start_url": "/",
        "display": "standalone",
        "background_color": "#f3efe6",
        "theme_color": "#16140f",
        "icons": [
            { "src": "/icon-192.png", "sizes": "192x192", "type": "image/png" },
            { "src": "/icon-512.png", "sizes": "512x512", "type": "image/png" },
            { "src": "/favicon.svg", "sizes": "any", "type": "image/svg+xml" }
        ]
    })
    .to_string()
}
