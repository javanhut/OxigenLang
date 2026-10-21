# oxigenlang.com

The OxigenLang website: a static site built from this repository by
`crates/oxigen-site`, with a playground that runs the real interpreter in the
browser through WebAssembly (`crates/oxigen-playground`).

```bash
imlazy site          # build into target/site (installs the wasm target first if missing)
imlazy site-serve    # build, then serve at http://localhost:8000
```

## What comes from where

| Source | Becomes |
|---|---|
| `docs/*.md`, `docs/stdlib/*.md` | `/docs/...`: every documentation page. The order and titles are in `site.toml`. |
| `website/pages/*.html` | The home, playground, about, benchmarks, docs index and 404 pages. HTML with a `+++` TOML header and `{{...}}` placeholders (see `crates/oxigen-site/src/pages.rs`). |
| `website/examples/*.oxi` | The home page's example tabs and the playground's example menu. Each must run in the browser sandbox, or the build fails. The first comment line is the title, the second the summary. |
| `website/snippets/*.oxi` | Code shown without a Run button, because it needs threads or the test runner. Each must compile. |
| `benchmark_reports/latest-native.md` | `/benchmarks/` and the home page figures. Re-run `scripts/bench.sh`, rebuild, and the site follows. |
| `website/assets/` | `site.css`, `site.js` (every page), `runner.js` and `worker.js` (running code), `playground.js`. |
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

- Every page has a canonical URL on `https://oxigenlang.com`, a description,
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
2. **Settings → Pages → Custom domain: `oxigenlang.com`**, then tick
   **Enforce HTTPS** once the certificate is issued.

At the registrar for **oxigenlang.com**:

| Type | Name | Value |
|---|---|---|
| A | `@` | 185.199.108.153 |
| A | `@` | 185.199.109.153 |
| A | `@` | 185.199.110.153 |
| A | `@` | 185.199.111.153 |
| AAAA | `@` | 2606:50c0:8000::153 |
| AAAA | `@` | 2606:50c0:8001::153 |
| AAAA | `@` | 2606:50c0:8002::153 |
| AAAA | `@` | 2606:50c0:8003::153 |
| CNAME | `www` | `javanhut.github.io` |

For **oxigenlang.org**, set up a **permanent (301) URL forward** at its
registrar to `https://oxigenlang.com`, keeping the path. A second copy of the
site would compete with the first in search results; the redirect makes
`.org` count towards `.com`.

Then add both domains to Google Search Console and Bing Webmaster Tools,
verify ownership with a DNS TXT record, and submit
`https://oxigenlang.com/sitemap.xml`.

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
