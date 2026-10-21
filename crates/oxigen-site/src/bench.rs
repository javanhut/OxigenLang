//! The benchmarks page, from benchmark_reports/latest-native.md, which
//! `scripts/bench.sh` writes. Re-running the benchmarks and rebuilding the
//! site updates the page.

use crate::html::escape;

pub struct Report {
    pub generated: String,
    pub platform: String,
    pub oxigen: String,
    pub python: String,
    pub node: String,
    pub bun: String,
    pub warmups: String,
    pub runs: String,
    pub rows: Vec<Row>,
}

pub struct Row {
    pub name: String,
    pub no_jit: f64,
    pub default: f64,
    pub jit: f64,
    pub python: f64,
    pub bun: Option<f64>,
    pub node: Option<f64>,
}

pub fn parse(text: &str) -> Result<Report, String> {
    let meta = |key: &str| -> String {
        text.lines()
            .find_map(|l| l.trim_start_matches("- ").strip_prefix(key))
            .map(|v| v.replace('`', "").trim().to_string())
            .unwrap_or_default()
    };
    let kernel = meta("Kernel:");
    let platform = match kernel.split_whitespace().last() {
        Some("arm64") if kernel.starts_with("Darwin") => "Apple Silicon Mac (macOS, arm64)".to_string(),
        _ => kernel.clone(),
    };
    let table = text.split("## Min times (ms)").nth(1).ok_or("benchmark report: no `## Min times (ms)` table")?;
    // Only this section: the next table's rows also start with `| bench_`.
    let table = table.split("\n## ").next().unwrap_or(table);
    let mut rows = Vec::new();
    for line in table.lines().filter(|l| l.starts_with("| bench_")) {
        let cells: Vec<&str> = line.trim_matches('|').split('|').map(str::trim).collect();
        let num = |i: usize| cells.get(i).and_then(|c| c.parse::<f64>().ok());
        let need = |i: usize| num(i).ok_or_else(|| format!("benchmark report: bad cell in `{line}`"));
        rows.push(Row {
            name: cells[0].to_string(),
            no_jit: need(1)?,
            default: need(2)?,
            jit: need(3)?,
            python: need(4)?,
            bun: num(5),
            node: num(6),
        });
    }
    if rows.is_empty() {
        return Err("benchmark report: the min-times table is empty".into());
    }
    let first_line = |key: &str| meta(key).split(" (").next().unwrap_or("").to_string();
    Ok(Report {
        generated: meta("Generated:").split('T').next().unwrap_or("").to_string(),
        platform,
        oxigen: meta("Oxigen:").replace("Oxigen Version: ", ""),
        python: first_line("Python:").replace("Python ", ""),
        node: first_line("Node:").trim_start_matches('v').to_string(),
        bun: meta("Bun:"),
        warmups: meta("Warmups:"),
        runs: meta("Runs:"),
        rows,
    })
}

/// What each benchmark measures, and its source file under example/.
fn describe(name: &str) -> (&'static str, &'static str) {
    match name {
        "bench_arith" => ("Arithmetic", "integer and float arithmetic in a hot loop"),
        "bench_closure" => ("Closures", "calling closures that capture variables"),
        "bench_collatz" => ("Collatz", "Collatz sequence lengths, branches and integer maths"),
        "bench_fib" => ("Recursive Fibonacci", "deep recursive function calls"),
        "bench_loop" => ("Counting loop", "a tight counting loop"),
        "bench_nested_loop" => ("Nested loops", "two nested counting loops"),
        "bench_nested_loop_big" => ("Nested loops, large", "nested loops over a larger range"),
        "bench_primes_parallel" => ("Primes, parallel", "prime counting split across cores with diverge/converge"),
        "bench_primes_serial" => ("Primes, serial", "prime counting on one core"),
        "bench_struct_method" => ("Struct methods", "method calls on struct instances"),
        _ => ("Benchmark", ""),
    }
}

fn ratio(base: f64, value: f64) -> f64 {
    if value <= 0.0 { 0.0 } else { base / value }
}

fn times(x: f64) -> String {
    if x >= 10.0 { format!("{x:.0}×") } else { format!("{x:.1}×") }
}

fn tip_ms(x: Option<f64>) -> String {
    x.map_or("not run".into(), |v| format!("{v:.0} ms"))
}

fn ms(x: Option<f64>) -> String {
    x.map_or("–".into(), |v| format!("{v:.0}"))
}

impl Report {
    pub fn median_speedup(&self, jit: bool) -> f64 {
        let mut s: Vec<f64> = self.rows.iter().map(|r| ratio(r.python, if jit { r.jit } else { r.default })).collect();
        s.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let n = s.len();
        if n % 2 == 1 { s[n / 2] } else { (s[n / 2 - 1] + s[n / 2]) / 2.0 }
    }

    pub fn wins_over_python(&self) -> usize {
        self.rows.iter().filter(|r| r.default < r.python).count()
    }

    pub fn max_speedup(&self) -> f64 {
        self.rows.iter().map(|r| ratio(r.python, r.default)).fold(0.0, f64::max)
    }

    /// The bar chart: Oxigen's speed-up over CPython per benchmark, for the
    /// default build and for `--jit`. Rows are sorted by the default build.
    pub fn chart(&self, repo_blob: impl Fn(&str) -> String) -> String {
        let mut rows: Vec<&Row> = self.rows.iter().collect();
        rows.sort_by(|a, b| ratio(b.python, b.default).partial_cmp(&ratio(a.python, a.default)).unwrap());
        let max = rows.iter().map(|r| ratio(r.python, r.default).max(ratio(r.python, r.jit))).fold(1.0, f64::max);
        // Round the scale up to a clean tick.
        let step = if max > 40.0 { 20.0 } else if max > 20.0 { 10.0 } else { 5.0 };
        let top = (max / step).ceil() * step;
        let ticks: String = (0..=((top / step) as usize))
            .map(|i| {
                let v = i as f64 * step;
                format!("<span style=\"left:{:.3}%\">{}</span>", v / top * 100.0, if i == 0 { "0".into() } else { format!("{v:.0}×") })
            })
            .collect();
        let mut bars = String::new();
        for r in rows {
            let (label, what) = describe(&r.name);
            let d = ratio(r.python, r.default);
            let j = ratio(r.python, r.jit);
            bars.push_str(&format!(
                "<li class=\"bar-row\" tabindex=\"0\" data-default=\"{d:.3}\" data-jit=\"{j:.3}\" data-tip=\"{tip}\">\
<a class=\"bar-label\" href=\"{src}\" rel=\"noopener\" title=\"{what}\">{label}</a>\
<span class=\"bar-track\"><span class=\"bar\" style=\"--w:{dw:.3}%;--wj:{jw:.3}%\"></span><span class=\"bar-value\" data-default=\"{dt}\" data-jit=\"{jt}\">{dt}</span></span></li>",
                tip = escape(&format!(
                    "{label}|Oxigen default={}|Oxigen --jit={}|Oxigen interpreter={}|CPython={}|Node={}|Bun={}",
                    tip_ms(Some(r.default)), tip_ms(Some(r.jit)), tip_ms(Some(r.no_jit)), tip_ms(Some(r.python)), tip_ms(r.node), tip_ms(r.bun)
                )),
                src = repo_blob(&format!("example/{}.oxi", r.name)),
                what = escape(what),
                dw = d / top * 100.0,
                jw = j / top * 100.0,
                dt = times(d),
                jt = times(j),
            ));
        }
        format!(
            "<figure class=\"bench-chart\" data-mode=\"default\">\
<figcaption><span class=\"chart-title\">Times faster than CPython {python}</span><span class=\"chart-sub\">Fastest of {runs} runs after {warmups} warm-ups. Longer is better.</span></figcaption>\
<div class=\"segmented\" role=\"group\" aria-label=\"Oxigen build\"><button type=\"button\" aria-pressed=\"true\" data-mode=\"default\">Default build</button><button type=\"button\" aria-pressed=\"false\" data-mode=\"jit\">Eager JIT <code>--jit</code></button></div>\
<ol class=\"bars\" style=\"--ticks:{n}\">{bars}</ol>\
<div class=\"bar-axis\" aria-hidden=\"true\">{ticks}</div>\
<div class=\"chart-tip\" hidden></div></figure>",
            python = escape(&self.python),
            runs = escape(&self.runs),
            warmups = escape(&self.warmups),
            n = (top / step) as usize,
        )
    }

    /// Every measurement, in milliseconds.
    pub fn table(&self) -> String {
        let mut body = String::new();
        for r in &self.rows {
            let (label, what) = describe(&r.name);
            body.push_str(&format!(
                "<tr><th scope=\"row\">{label}<small>{what}</small></th><td>{}</td><td class=\"hi\">{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td class=\"hi\">{}</td></tr>",
                ms(Some(r.no_jit)), ms(Some(r.default)), ms(Some(r.jit)), ms(Some(r.python)), ms(r.node), ms(r.bun),
                times(ratio(r.python, r.default)),
                what = escape(what),
            ));
        }
        format!(
            "<div class=\"table-wrap\"><table class=\"bench-table\"><caption>Fastest run in milliseconds (lower is better) on {platform}, {date}.</caption><thead><tr><th scope=\"col\">Benchmark</th><th scope=\"col\">Oxigen interpreter <code>--no-jit</code></th><th scope=\"col\">Oxigen default</th><th scope=\"col\">Oxigen <code>--jit</code></th><th scope=\"col\">CPython {py}</th><th scope=\"col\">Node {node}</th><th scope=\"col\">Bun {bun}</th><th scope=\"col\">Default vs CPython</th></tr></thead><tbody>{body}</tbody></table></div>",
            platform = escape(&self.platform),
            date = escape(&self.generated),
            py = escape(&self.python),
            node = escape(&self.node),
            bun = escape(&self.bun),
        )
    }
}
