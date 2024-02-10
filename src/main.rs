use std::{
    fmt,
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

macro_rules! fatal {
    ($($tt:tt)*) => {{
        eprintln!($($tt)*);
        std::process::exit(1)
    }};
}

#[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
struct Window {
    reverse: bool,
    position: Position,
    anchor: String,
    source_bytes_max: usize,
    target_bytes_max: usize,
    target_lines_max: usize,
    filter_in: Vec<FilterClause>,
    filter_out: Vec<FilterClause>,
}

impl Window {
    pub fn parse(text: &str) -> Result<Window, toml::de::Error> {
        toml::from_str(text)
    }

    pub fn display(&self) -> String {
        toml::to_string_pretty(self).unwrap()
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(untagged)]
enum FilterClause {
    String(String),
    Vec(Vec<String>),
}

impl FilterClause {
    fn literals(&self) -> &[String] {
        match self {
            FilterClause::String(it) => std::slice::from_ref(it),
            FilterClause::Vec(it) => it.as_slice(),
        }
    }
}

#[allow(non_upper_case_globals)]
const KiB: usize = 1024;
#[allow(non_upper_case_globals)]
const MiB: usize = 1024 * KiB;

struct Args {
    path: PathBuf,
}

fn parse_args() -> Args {
    let mut path: Option<PathBuf> = None;
    let mut args_os = std::env::args_os();
    _ = args_os.next();

    for arg in args_os {
        if arg.to_str() == Some("-h") || arg.to_str() == Some("--help") {
            println!(
                "\
Usage:
  window <file>

window filters <file> according to the query specified in window.toml.
The result is written to <file>.window.
"
            );
            std::process::exit(0);
        }
        if path.is_some() {
            fatal!("expected a single argument");
        }
        path = Some(PathBuf::from(arg));
    }
    let Some(path) = path else {
        fatal!("window requires a <file> argument");
    };
    Args { path }
}

fn main() {
    let delay = std::time::Duration::from_millis(100);

    let args = parse_args();
    let source_path = args.path.as_path();
    let target_path = target_path_for_source(source_path);
    let control_path = Path::new("window.toml");

    let file = File::open(&source_path)
        .unwrap_or_else(|err| fatal!("can't open {}: {}", source_path.display(), err));
    let mmap = unsafe { memmap2::Mmap::map(&file) }
        .unwrap_or_else(|err| fatal!("can't mmap {}: {}", source_path.display(), err));

    let window_default = Window {
        reverse: false,
        position: Position::Relative(0.0),
        anchor: String::new(),
        source_bytes_max: 100 * MiB,
        target_bytes_max: 100 * KiB,
        target_lines_max: 50,
        filter_in: Vec::new(),
        filter_out: Vec::new(),
    };
    let window_existing = std::fs::read_to_string(control_path)
        .ok()
        .and_then(|it| Window::parse(&it).ok());

    let mut window = window_existing.unwrap_or(window_default);

    std::fs::write(&control_path, &window.display())
        .unwrap_or_else(|err| fatal!("can't write {}: {}", target_path.display(), err));

    eprintln!("Control file: {}", control_path.display());
    eprintln!("Results file: {}", target_path.display());

    let mut error = String::new();
    let mut buf = Vec::new();
    loop {
        let mut ctx = Context {
            window: &window,
            source: &mmap,
            first_match: 0,
            target: &mut buf,
        };

        ctx.compute();
        ctx.write(&target_path, &error);

        loop {
            // Inner loop --- re-read config file until it is syntactically valid.

            std::thread::sleep(delay);
            let control = std::fs::read_to_string(control_path)
                .unwrap_or_else(|err| fatal!("can't read {}: {}", control_path.display(), err));
            match Window::parse(&control) {
                Ok(new_window) => {
                    if !error.is_empty() {
                        error.clear();
                        ctx.write(&target_path, &error);
                    }
                    if new_window != window {
                        window = new_window;
                        break;
                    }
                }
                Err(err) => {
                    let new_error = err.to_string();
                    if error != new_error {
                        error = new_error;
                        ctx.write(&target_path, &error)
                    }
                }
            }
        }
    }
}

struct Context<'a, 'b> {
    window: &'a Window,
    source: &'a [u8],
    first_match: usize,
    target: &'b mut Vec<u8>,
}

impl<'a, 'b> Context<'a, 'b> {
    fn compute(&mut self) {
        self.target.clear();

        let source = self.source_slice();

        let mut scratch = vec![0; self.window.target_bytes_max];

        let (mut source_pos, mut scratch_pos) = if self.window.reverse {
            (source.len(), scratch.len())
        } else {
            (0, 0)
        };

        let mut first_match_pos: Option<usize> = None;
        let mut anchored = false;
        while (self.window.reverse && source_pos > 0)
            || (!self.window.reverse && source_pos < source.len())
        {
            let line = next_line(source, &mut source_pos, self.window.reverse);
            let line = std::str::from_utf8(line).unwrap_or("���\n");
            anchored = anchored || line.contains(&self.window.anchor);
            if !anchored {
                continue;
            }
            if filter_out(line, &self.window.filter_out) {
                continue;
            }
            if !filter_in(line, &self.window.filter_in) {
                continue;
            }
            if first_match_pos.is_none() {
                first_match_pos = Some(line.as_ptr() as usize - self.source.as_ptr() as usize);
            }

            match copy(
                &mut scratch,
                &mut scratch_pos,
                line.as_bytes(),
                self.window.reverse,
            ) {
                CopyResult::Copied => {}
                CopyResult::NoSpace => break,
            }
        }

        let result = if self.window.reverse {
            &scratch[scratch_pos..]
        } else {
            &scratch[..scratch_pos]
        };

        let result = trim_lines(result, self.window.target_lines_max, self.window.reverse);
        self.first_match = first_match_pos.unwrap_or(0);
        self.target.extend(result);
    }

    fn write(&self, target: &Path, error: &str) {
        fs::File::create(target)
            .and_then(|mut file| self.write_inner(&mut file, error))
            .unwrap_or_else(|err| fatal!("can't write {}: {}", target.display(), err))
    }

    fn write_inner(&self, target: &mut dyn Write, error: &str) -> io::Result<()> {
        if !error.is_empty() {
            target.write_all(format!("error: {}\n", error).as_bytes())?;
        }
        target.write_all(format!("pos: {}\n", self.first_match).as_bytes())?;
        target.write_all(&self.target)?;
        Ok(())
    }

    fn source_slice(&self) -> &'a [u8] {
        let raw_index = self.window.position.to_absolute(self.source.len());
        let source_slice_semi = if self.window.reverse {
            &self.source[0..raw_index]
        } else {
            &self.source[raw_index..]
        };
        let source_slice = trim_length(
            source_slice_semi,
            self.window.source_bytes_max,
            self.window.reverse,
        );
        if self.window.reverse {
            if raw_index == self.source.len() || self.source[raw_index + 1] == b'\n' {
                return source_slice;
            }
        } else {
            if raw_index == 0 || self.source[raw_index - 1] == b'\n' {
                return source_slice;
            }
        }
        trim_newline(source_slice, self.window.reverse)
    }
}

fn next_line<'a>(source: &'a [u8], source_pos: &mut usize, reverse: bool) -> &'a [u8] {
    if reverse {
        match source[0..*source_pos].iter().rposition(|&it| it == b'\n') {
            Some(index) => {
                let line = &source[index..*source_pos];
                *source_pos = index;
                line
            }
            None => {
                let line = &source[0..*source_pos];
                *source_pos = 0;
                line
            }
        }
    } else {
        match source[*source_pos..].iter().position(|&it| it == b'\n') {
            Some(index) => {
                let line = &source[*source_pos..*source_pos + index + 1];
                *source_pos = *source_pos + index + 1;
                line
            }
            None => {
                let line = &source[*source_pos..];
                *source_pos = source.len();
                line
            }
        }
    }
}

fn filter_out(line: &str, clauses: &[FilterClause]) -> bool {
    for clause in clauses {
        if contains_all(line, clause.literals()) {
            return true;
        }
    }
    false
}

fn filter_in(line: &str, clauses: &[FilterClause]) -> bool {
    if clauses.is_empty() {
        return true;
    }
    for clause in clauses {
        if contains_all(line, clause.literals()) {
            return true;
        }
    }
    false
}

fn contains_all(line: &str, clauses: &[String]) -> bool {
    clauses.iter().all(|it| line.contains(it))
}

enum CopyResult {
    Copied,
    NoSpace,
}

fn copy(target: &mut [u8], target_pos: &mut usize, source: &[u8], reverse: bool) -> CopyResult {
    if reverse {
        if *target_pos < source.len() {
            return CopyResult::NoSpace;
        }
        target[*target_pos - source.len()..*target_pos].copy_from_slice(source);
        *target_pos -= source.len();
    } else {
        if *target_pos + source.len() > target.len() {
            return CopyResult::NoSpace;
        }
        target[*target_pos..*target_pos + source.len()].copy_from_slice(source);
        *target_pos += source.len();
    }
    CopyResult::Copied
}

fn trim_length(source: &[u8], limit: usize, reverse: bool) -> &[u8] {
    if reverse {
        &source[source.len().saturating_sub(limit)..]
    } else {
        &source[0..source.len().min(limit)]
    }
}

fn trim_newline(source: &[u8], reverse: bool) -> &[u8] {
    if reverse {
        match source.iter().rposition(|&it| it == b'\n') {
            Some(index) => &source[0..index + 1],
            None => source,
        }
    } else {
        match source.iter().position(|&it| it == b'\n') {
            Some(index) => &source[index + 1..],
            None => source,
        }
    }
}

fn target_path_for_source(source: &Path) -> PathBuf {
    let parent = source.parent().unwrap_or(source);
    match source.file_stem() {
        Some(file_stem) => match source.extension().and_then(|it| it.to_str()) {
            Some(it) => parent
                .join(file_stem)
                .with_extension(format!("{it}.window")),
            None => parent.join(file_stem).with_extension(".window"),
        },
        None => return source.join("output.window"),
    }
}

fn trim_lines(source: &[u8], limit: usize, reverse: bool) -> &[u8] {
    if reverse {
        match source
            .iter()
            .copied()
            .enumerate()
            .rev()
            .filter(|it| it.1 == b'\n')
            .nth(limit)
        {
            Some((index, _)) => &source[index + 1..],
            None => source,
        }
    } else {
        match source
            .iter()
            .copied()
            .enumerate()
            .filter(|it| it.1 == b'\n')
            .nth(limit)
        {
            Some((index, _)) => &source[..index + 1],
            None => source,
        }
    }
}

#[derive(
    Clone, Copy, PartialEq, Debug, serde_with::DeserializeFromStr, serde_with::SerializeDisplay,
)]
pub enum Position {
    Relative(f64),
    Absolute(usize),
}

impl FromStr for Position {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(percentage) = s.strip_suffix('%') {
            let percentage = percentage
                .parse::<f64>()
                .map_err(|it| it.to_string())?
                .clamp(0.0, 100.0);
            return Ok(Position::Relative(percentage / 100.0));
        }
        let mut multiplier = 1;
        for suffixes in ["kM", "mM", "gG"] {
            multiplier *= 1024;
            for suffix in suffixes.chars() {
                if let Some(value) = s.strip_suffix(suffix) {
                    let value = value.parse::<f64>().map_err(|it| it.to_string())?;
                    let value = (value * (multiplier as f64)) as usize;
                    return Ok(Position::Absolute(value));
                }
            }
        }
        let value = s.parse::<f64>().map_err(|it| it.to_string())?;
        return Ok(Position::Absolute(value as usize));
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Position::Relative(fraction) => write!(f, "{}%", fraction * 100.0),
            Position::Absolute(value) => write!(f, "{}", value),
        }
    }
}

impl Position {
    fn to_absolute(&self, len: usize) -> usize {
        match *self {
            Position::Relative(it) => ((len as f64) * it) as usize,
            Position::Absolute(it) => it,
        }
        .clamp(0, len)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    fn check(window: &str, input: &str, want: expect_test::Expect) {
        let window = Window::parse(window).unwrap();
        let mut target = Vec::new();
        let mut ctx = Context {
            window: &window,
            source: input.as_bytes(),
            target: &mut target,
            first_match: 0,
        };
        ctx.compute();
        let mut result = Vec::new();
        ctx.write_inner(&mut result, "").unwrap();
        let got = String::from_utf8(result).unwrap();
        want.assert_eq(&got);
    }

    #[test]
    fn empty() {
        check(
            r#"
reverse = false
position = "0%"
anchor = ""
source_bytes_max = 104857600
target_bytes_max = 102400
target_lines_max = 50
filter_in = []
filter_out = []
        "#,
            "",
            expect![[r#"
                pos: 0
            "#]],
        );
    }

    #[test]
    fn all() {
        check(
            r#"
reverse = false
position = "0%"
anchor = ""
source_bytes_max = 104857600
target_bytes_max = 102400
target_lines_max = 50
filter_in = []
filter_out = []
        "#,
            "aaa\nbbb\nccc\n",
            expect![[r#"
                pos: 0
                aaa
                bbb
                ccc
            "#]],
        );
    }

    #[test]
    fn filter_in() {
        check(
            r#"
reverse = false
position = "0%"
anchor = ""
source_bytes_max = 104857600
target_bytes_max = 102400
target_lines_max = 50
filter_in = ["b"]
filter_out = []
        "#,
            "aaa\nbbb\nccc",
            expect![[r#"
                pos: 4
                bbb
            "#]],
        );
    }

    #[test]
    fn anchor() {
        check(
            r#"
reverse = false
position = "0%"
anchor = "b"
source_bytes_max = 104857600
target_bytes_max = 102400
target_lines_max = 50
filter_in = []
filter_out = []
        "#,
            "aaa\nbbb\nccc",
            expect![[r#"
                pos: 4
                bbb
                ccc"#]],
        );

        check(
            r#"
reverse = false
position = "4"
anchor = ""
source_bytes_max = 104857600
target_bytes_max = 102400
target_lines_max = 50
filter_in = []
filter_out = []
        "#,
            "aaa\nbbb\nccc",
            expect![[r#"
                pos: 4
                bbb
                ccc"#]],
        );
    }
}
