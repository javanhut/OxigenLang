//! The latest GitHub release, read when the site is built, for the download
//! page. release.yml rebuilds the site after publishing one, so the page
//! lists the new files without anyone editing it.

use serde_json::Value;

pub struct Release {
    /// Such as `0.1.4`.
    pub version: String,
    /// `YYYY-MM-DD`.
    pub date: String,
    pub url: String,
    pub files: Vec<File>,
}

pub struct File {
    pub name: String,
    /// Such as `linux_x86`, as the installer names platforms.
    pub platform: String,
    pub size: u64,
    pub url: String,
    pub sha256: Option<String>,
}

impl File {
    /// The operating system and CPU, as people say them.
    pub fn label(&self) -> (&'static str, &'static str) {
        match self.platform.as_str() {
            "linux_x86" => ("Linux", "x86-64"),
            "linux_arm64" => ("Linux", "ARM64"),
            "macos_x86" => ("macOS", "Intel"),
            "macos_arm64" => ("macOS", "Apple silicon"),
            _ => ("Other", ""),
        }
    }
}

/// The latest release of `repo` (`owner/name`). `GITHUB_TOKEN`, when set,
/// lifts the API's anonymous rate limit.
pub fn latest(repo: &str) -> Result<Release, String> {
    let url = format!("https://api.github.com/repos/{repo}/releases/latest");
    let mut request = ureq::get(&url)
        .header("User-Agent", "oxigen-site")
        .header("Accept", "application/vnd.github+json");
    let token = std::env::var("GITHUB_TOKEN").unwrap_or_default();
    if !token.is_empty() {
        request = request.header("Authorization", &format!("Bearer {token}"));
    }
    let body = request
        .call()
        .map_err(|e| format!("{url}: {e}"))?
        .into_body()
        .read_to_string()
        .map_err(|e| format!("{url}: {e}"))?;
    parse(&body)
}

fn parse(body: &str) -> Result<Release, String> {
    let json: Value = serde_json::from_str(body).map_err(|e| format!("release JSON: {e}"))?;
    let text = |v: &Value, key: &str| v.get(key).and_then(Value::as_str).unwrap_or("").to_string();
    let tag = text(&json, "tag_name");
    if tag.is_empty() {
        return Err("the latest release has no tag".into());
    }
    let mut files = Vec::new();
    for asset in json
        .get("assets")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        let name = text(asset, "name");
        // oxigen_lang_v0_1_4_linux_x86.tar.gz -> linux_x86
        let Some(stem) = name.strip_suffix(".tar.gz") else {
            continue;
        };
        let mut parts = stem.rsplitn(3, '_');
        let (arch, os) = (parts.next().unwrap_or(""), parts.next().unwrap_or(""));
        let platform = format!("{os}_{arch}");
        files.push(File {
            platform,
            size: asset.get("size").and_then(Value::as_u64).unwrap_or(0),
            url: text(asset, "browser_download_url"),
            sha256: asset
                .get("digest")
                .and_then(Value::as_str)
                .and_then(|d| d.strip_prefix("sha256:"))
                .map(str::to_string),
            name,
        });
    }
    // Linux before macOS, x86-64 before ARM: the order of the table.
    let rank = |p: &str| {
        ["linux_x86", "linux_arm64", "macos_arm64", "macos_x86"]
            .iter()
            .position(|x| *x == p)
            .unwrap_or(9)
    };
    files.sort_by_key(|f| rank(&f.platform));
    Ok(Release {
        version: tag.trim_start_matches('v').to_string(),
        date: text(&json, "published_at")
            .split('T')
            .next()
            .unwrap_or("")
            .to_string(),
        url: text(&json, "html_url"),
        files,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_platforms_and_digests() {
        let release = parse(
            r#"{"tag_name":"v0.1.4","published_at":"2026-08-20T00:32:07Z","html_url":"https://x/v0.1.4",
               "assets":[{"name":"oxigen_lang_v0_1_4_macos_arm64.tar.gz","size":2731146,"digest":"sha256:ab12",
                          "browser_download_url":"https://x/m.tar.gz"},
                         {"name":"oxigen_lang_v0_1_4_linux_x86.tar.gz","size":3682463,"browser_download_url":"https://x/l.tar.gz"},
                         {"name":"SHA256SUMS","size":10}]}"#,
        )
        .unwrap();
        assert_eq!(release.version, "0.1.4");
        assert_eq!(release.date, "2026-08-20");
        let platforms: Vec<_> = release.files.iter().map(|f| f.platform.as_str()).collect();
        assert_eq!(platforms, ["linux_x86", "macos_arm64"]);
        assert_eq!(release.files[1].sha256.as_deref(), Some("ab12"));
        assert_eq!(release.files[0].sha256, None);
    }
}
