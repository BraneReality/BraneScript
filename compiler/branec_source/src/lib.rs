use anyhow::{anyhow, bail};
use std::{collections::HashMap, fmt::Display, ops::Range, path::PathBuf, sync::Arc};

#[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Debug)]
pub enum Uri {
    Unknown,
    File(PathBuf),
}

impl Default for Uri {
    fn default() -> Self {
        Uri::Unknown
    }
}

impl Display for Uri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Uri::Unknown => write!(f, "unknown"),
            Uri::File(path_buf) => write!(f, "file://{}", path_buf.to_string_lossy()),
        }
    }
}

pub struct SourceManager {
    sources: HashMap<Uri, String>,
}

impl SourceManager {
    pub fn new() -> SourceManager {
        SourceManager {
            sources: HashMap::new(),
        }
    }

    pub fn refresh(&mut self, key: Uri) -> anyhow::Result<()> {
        match key {
            Uri::Unknown => {
                bail!("cannot refresh unknown uri");
            }
            Uri::File(path_buf) => {
                self.load_from_file(path_buf)?;
            }
        }
        Ok(())
    }

    pub fn load_from_file(&mut self, path: impl Into<PathBuf>) -> anyhow::Result<Uri> {
        let path = path.into();
        let text = std::fs::read_to_string(&path)?;
        let key = Uri::File(path);

        let _ = self.sources.insert(key.clone(), text);
        Ok(key)
    }

    pub fn get(&self, key: &Uri) -> anyhow::Result<&String> {
        self.sources
            .get(key)
            .ok_or_else(|| anyhow!("source not found for {}", key))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Span {
    pub range: Range<usize>,
    pub source: Arc<Uri>,
}

impl chumsky::span::Span for Span {
    type Context = Arc<Uri>;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Span {
            source: context,
            range,
        }
    }

    fn context(&self) -> Self::Context {
        self.source.clone()
    }

    fn start(&self) -> Self::Offset {
        self.range.start
    }

    fn end(&self) -> Self::Offset {
        self.range.end
    }
}

