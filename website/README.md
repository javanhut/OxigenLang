# oxigenlang.org

The OxigenLang website: a static site built from this repository by
`crates/oxigen-site`, with a playground that runs the real interpreter and the
real language server in the browser through WebAssembly.

```bash
imlazy site          # build into target/site (installs what is missing first)
imlazy site-serve    # build, then serve at http://localhost:8000
imlazy site-test     # the playground's tests, in Node (22 or later)
```

The site is rebuilt from `main` on every push, so the playground, the docs
and the stdlib always match `main`, released or not. Only the download page
follows releases.

## The playground

- **Interpreter.** `crates/oxigen-playground`, built for WASI twice:
  `wasm32-wasip1` (the docs pages' Run buttons, and the editor's highlighting,
  checking and formatting on the page itself) and `wasm32-wasip1-threads`
  (playground runs). It runs a file the way `oxigen main.oxi` does, or its
  tests the way `oxigen test` does, through the same std calls as natively.
- **System.** `website/assets/wasi.js` supplies what WASI asks for: a file
  system in memory (`/work` holds the open files, and what a run writes comes
  back as tabs), standard input, the clock, sleeping, arguments, environment
  and, in the threads build, a Web Worker per thread. The network, other
  programs and C libraries stay refused, with an error naming them.
- **Isolation.** Pausing a program until the user types, and threads, need
  memory shared between the page and its workers, which browsers only give a
  cross-origin isolated page. GitHub Pages cannot send the headers for that,
  so `website/static/playground/sw.js`, a service worker scoped to
  `/playground/`, adds them, and the page reloads once on a first visit.
  Without a service worker (a private window in some browsers), runs are
  single-threaded and read input from the "Input and arguments" box.
- **Editor.** CodeMirror 6 with standard or vim keys (`:w` formats, `:run`,
  `:test`), highlighting and live errors from the interpreter's own lexer and
  compiler, and completion and hovers from `lsp-go` built for WASI, running in
  `lsp-worker.js`. The editor's code is vendored, pinned and checksummed, in
  `website/assets/vendor/` by `scripts/vendor-editor.sh` (`imlazy
  vendor-editor`) and loaded through an import map, with no bundler.

`imlazy site-test` covers the WASI layer, runs in both builds, typed input,
files, threads, tests, the editor's tools and the language server. The editor
itself still needs a look in a browser: typing, vim keys, completion, tabs,
running with input, and a narrow viewport.

## What comes from where

| Source | Becomes |
|---|---|
| `docs/*.md`, `docs/stdlib/*.md` | `/docs/...`: every documentation page. The order and titles are in `site.toml`. |
| `website/pages/*.html` | The home, playground, about, benchmarks, docs index and 404 pages. HTML with a `+++` TOML header and `{{...}}` placeholders (see `crates/oxigen-site/src/pages.rs`). |
| `website/examples/*.oxi` | The home page's example tabs and the playground's example menu. Each must run in the docs pages' sandbox (no files, input or threads), or the build fails. The first comment line is the title, the second the summary. |
| `website/examples/playground/*.oxi` | Playground-only examples that read input, write files or start threads. Each must compile. |
| `website/snippets/*.oxi` | Code shown without a Run button, because it needs threads or the test runner. Each must compile. |
| `benchmark_reports/latest-native.md` | `/benchmarks/` and the home page figures. Re-run `scripts/bench.sh`, rebuild, and the site follows. |
| `website/assets/` | `site.css`, `site.js` (every page), `runner.js`, `worker.js` and `wasi.js` (running code), `playground.js`, `lsp.js` and `lsp-worker.js` (the language server), `vendor/` (CodeMirror). |
| `website/static/` | Copied as is: icons, the social card `og.png`. |
| `website/brand/` | SVG sources for the logo, icon and social card. Re-render with `rsvg-convert` (see below). |

The build also writes `sitemap.xml`, `robots.txt`, `llms.txt`, `llms-full.txt`,
a Markdown copy of every doc at `/docs/<page>/index.md`, `search.json` for the
search dialog, and `CNAME`.

### Runnable docs examples

Every `oxi` code block in the docs runs through the same sandbox as the
playground while the site builds. One that finishes and prints something
gets **Run** and **Playground** buttons; one that needs files, the network
or threads, or is a fragment, is shown without them. The build reports how
many qualified.

## Search engines and AI crawlers

- Every page has a canonical URL on `https://www.oxigenlang.org`, a description,
  Open Graph and Twitter cards, and JSON-LD naming the language
  (`ComputerLanguage`), the software (`SoftwareApplication`,
  `SoftwareSourceCode`) and its creator (`Person`, Javan Hutchinson), linked
  by `@id`. Docs pages are `TechArticle`s with breadcrumbs; the about page
  carries an `FAQPage`.
- `robots.txt` allows everything and names the AI crawlers explicitly.
- `llms.txt` (see llmstxt.org) summarises the language and links a Markdown
  copy of every page; `llms-full.txt` is all the docs in one file.
- The pages are plain HTML: everything is readable without JavaScript.

To list more profiles for the creator (LinkedIn, X, a personal site), add
them to `same_as` in `site.toml`. To show a photo on the home and about pages
(and give search engines one for the `Person`), put it in `website/static/`
and set `photo` in `site.toml`; until then those pages show a monogram.

## Deploying

`.github/workflows/site.yml` builds the site on every push and pull request
and deploys `main` to GitHub Pages. Once, on GitHub:

1. **Settings → Pages → Source: GitHub Actions.**
2. **Settings → Pages → Custom domain: `www.oxigenlang.org`**, then tick
   **Enforce HTTPS** once the certificate is issued.

At the registrar for **oxigenlang.org**, point the apex domain and `www` at
GitHub Pages as GitHub's
[Managing a custom domain for your GitHub Pages site](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)
describes. Its record values are the current ones; GitHub has changed them
before. GitHub then redirects the apex to `www`.

For **oxigenlang.com**, set up a **permanent (301) URL forward** at its
registrar to `https://www.oxigenlang.org`, keeping the path. A second copy of
the site would compete with the first in search results; the redirect makes
`.com` count towards `.org`. The address the site calls canonical is `url` in
`site.toml`: change it there and in the custom domain together.

Then add both domains to Google Search Console and Bing Webmaster Tools,
verify ownership with a DNS TXT record, and submit
`https://www.oxigenlang.org/sitemap.xml`.

## Re-rendering the images

```bash
cd website
rsvg-convert -w 1200 -h 630 brand/og.svg -o static/og.png
rsvg-convert -w 180 -h 180 brand/icon.svg -o static/apple-touch-icon.png
rsvg-convert -w 192 -h 192 brand/icon.svg -o static/icon-192.png
rsvg-convert -w 512 -h 512 brand/icon.svg -o static/icon-512.png
rsvg-convert -w 32 -h 32 brand/icon.svg -o static/favicon-32.png
cp brand/logo.svg static/favicon.svg
```
