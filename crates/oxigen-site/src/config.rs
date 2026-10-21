//! website/site.toml: the site's address, its author, and the docs sidebar.

use std::path::Path;
use toml::{Table, Value};

pub struct Config {
    /// The canonical origin, without a trailing slash.
    pub url: String,
    pub name: String,
    pub description: String,
    pub repo: String,
    pub branch: String,
    pub author: Author,
    pub sections: Vec<Section>,
}

pub struct Author {
    pub name: String,
    pub url: String,
    /// A site path to a photo of the author, or none.
    pub photo: Option<String>,
    pub same_as: Vec<String>,
}

pub struct Section {
    pub title: String,
    pub pages: Vec<PageRef>,
}

pub struct PageRef {
    /// Relative to docs/.
    pub file: String,
    pub slug: String,
    pub title: Option<String>,
    pub description: Option<String>,
}

impl Config {
    pub fn load(path: &Path) -> Result<Config, String> {
        let text = std::fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
        let table: Table = text
            .parse()
            .map_err(|e| format!("{}: {e}", path.display()))?;
        let author = table
            .get("author")
            .and_then(Value::as_table)
            .ok_or("site.toml: missing [author]")?;
        let mut sections = Vec::new();
        for section in table
            .get("section")
            .and_then(Value::as_array)
            .ok_or("site.toml: no [[section]]")?
        {
            let section = section
                .as_table()
                .ok_or("site.toml: [[section]] is not a table")?;
            let mut pages = Vec::new();
            for page in section
                .get("pages")
                .and_then(Value::as_array)
                .ok_or("site.toml: section without pages")?
            {
                let page = page.as_table().ok_or("site.toml: a page is not a table")?;
                pages.push(PageRef {
                    file: string(page, "file")?,
                    slug: string(page, "slug")?,
                    title: page
                        .get("title")
                        .and_then(Value::as_str)
                        .map(str::to_string),
                    description: page
                        .get("description")
                        .and_then(Value::as_str)
                        .map(str::to_string),
                });
            }
            sections.push(Section {
                title: string(section, "title")?,
                pages,
            });
        }
        Ok(Config {
            url: string(&table, "url")?.trim_end_matches('/').to_string(),
            name: string(&table, "name")?,
            description: string(&table, "description")?,
            repo: string(&table, "repo")?.trim_end_matches('/').to_string(),
            branch: string(&table, "branch")?,
            author: Author {
                name: string(author, "name")?,
                url: string(author, "url")?,
                photo: author
                    .get("photo")
                    .and_then(Value::as_str)
                    .filter(|p| !p.is_empty())
                    .map(str::to_string),
                same_as: author
                    .get("same_as")
                    .and_then(Value::as_array)
                    .map(|a| {
                        a.iter()
                            .filter_map(Value::as_str)
                            .map(str::to_string)
                            .collect()
                    })
                    .unwrap_or_default(),
            },
            sections,
        })
    }

    /// The absolute URL of site path `path` (which starts with `/`).
    pub fn abs(&self, path: &str) -> String {
        format!("{}{path}", self.url)
    }

    /// The repository URL of `path`, relative to the repository root.
    pub fn blob(&self, path: &str) -> String {
        let kind = if path.ends_with('/') || !path.rsplit('/').next().unwrap_or("").contains('.') {
            "tree"
        } else {
            "blob"
        };
        format!(
            "{}/{kind}/{}/{}",
            self.repo,
            self.branch,
            path.trim_end_matches('/')
        )
    }
}

fn string(table: &Table, key: &str) -> Result<String, String> {
    table
        .get(key)
        .and_then(Value::as_str)
        .map(str::to_string)
        .ok_or_else(|| format!("site.toml: missing `{key}`"))
}
