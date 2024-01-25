use std::{
    fmt,
    fs::File,
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
    filter_in: Vec<Vec<String>>,
    filter_out: Vec<Vec<String>>,
}

#[allow(non_upper_case_globals)]
const KiB: usize = 1024;
#[allow(non_upper_case_globals)]
const MiB: usize = 1024 * KiB;

fn main() {
    let delay = std::time::Duration::from_millis(100);

    xflags::xflags! {
        cmd w {
            required path: PathBuf
        }

    };
    let flags = W::from_env_or_exit();
    let source_path = flags.path.as_path();
    let target_path = target_path_for_source(source_path);
    let control_path = Path::new("window.toml");

    let file = File::open(&source_path)
        .unwrap_or_else(|err| fatal!("can't open {}: {}", source_path.display(), err));
    let mmap = unsafe { memmap2::Mmap::map(&file) }
        .unwrap_or_else(|err| fatal!("can't mmap {}: {}", source_path.display(), err));

    let mut window = Window {
        reverse: false,
        position: Position::Relative(0.0),
        anchor: String::new(),
        source_bytes_max: 100 * MiB,
        target_bytes_max: 100 * KiB,
        target_lines_max: 50,
        filter_in: Vec::new(),
        filter_out: Vec::new(),
    };
    let mut error = String::new();
    let window_toml = toml::to_string(&window).unwrap();
    std::fs::write(&control_path, &window_toml)
        .unwrap_or_else(|err| fatal!("can't write {}: {}", target_path.display(), err));

    let mut buf = Vec::new();
    loop {
        let mut ctx = Context { window: &window, source: &mmap, target: &mut buf };
        ctx.compute();
        std::fs::write(&target_path, &ctx.target)
            .unwrap_or_else(|err| fatal!("can't write {}: {}", target_path.display(), err));
        loop {
            std::thread::sleep(delay);
            let control = std::fs::read_to_string(control_path)
                .unwrap_or_else(|err| fatal!("can't read {}: {}", control_path.display(), err));
            match toml::from_str(&control) {
                Ok(new_window) if new_window != window => {
                    window = new_window;
                    eprintln!("{window:?}");
                    break;
                }
                Ok(_) => continue,
                Err(err) => {
                    let new_error = err.to_string();
                    if error != new_error {
                        eprintln!("{new_error}");
                        error = new_error;
                    }
                    continue;
                }
            }
        }
    }
}

struct Context<'a, 'b> {
    window: &'a Window,
    source: &'a [u8],
    target: &'b mut Vec<u8>,
}

impl<'a, 'b> Context<'a, 'b> {
    fn compute(&mut self) {
        self.target.clear();

        let source = self.source_slice();

        let mut scratch = vec![0; self.window.target_bytes_max];

        let (mut source_pos, mut scratch_pos) =
            if self.window.reverse { (source.len(), scratch.len()) } else { (0, 0) };

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
            match copy(&mut scratch, &mut scratch_pos, line.as_bytes(), self.window.reverse) {
                CopyResult::Copied => {}
                CopyResult::NoSpace => break,
            }
        }

        let result =
            if self.window.reverse { &scratch[scratch_pos..] } else { &scratch[..scratch_pos] };

        let result = trim_lines(result, self.window.target_lines_max, self.window.reverse);
        self.target.extend(result);
    }

    fn source_slice(&self) -> &'a [u8] {
        let raw_index = self.window.position.to_absolute(self.source.len());
        let source_slice_semi = if self.window.reverse {
            &self.source[0..raw_index]
        } else {
            &self.source[raw_index..]
        };
        trim_newline(
            trim_length(source_slice_semi, self.window.source_bytes_max, self.window.reverse),
            self.window.reverse,
        )
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

fn filter_out(line: &str, patterns: &[Vec<String>]) -> bool {
    for clauses in patterns {
        if contains_all(line, clauses) {
            return true;
        }
    }
    false
}

fn filter_in(line: &str, patterns: &[Vec<String>]) -> bool {
    if patterns.is_empty() {
        return true;
    }
    for clauses in patterns {
        if contains_all(line, clauses) {
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
            Some(it) => parent.join(file_stem).with_extension(format!("{it}.window")),
            None => parent.join(file_stem).with_extension(".window"),
        },
        None => return source.join("output.window"),
    }
}

fn trim_lines(source: &[u8], limit: usize, reverse: bool) -> &[u8] {
    if reverse {
        match source.iter().copied().enumerate().rev().filter(|it| it.1 == b'\n').nth(limit) {
            Some((index, _)) => &source[index + 1..],
            None => source,
        }
    } else {
        match source.iter().copied().enumerate().filter(|it| it.1 == b'\n').nth(limit) {
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
            let percentage =
                percentage.parse::<f64>().map_err(|it| it.to_string())?.clamp(0.0, 100.0);
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
