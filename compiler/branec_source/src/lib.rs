use anyhow::{anyhow, bail};
use std::{collections::HashMap, fmt::Display, ops::Range, path::PathBuf, sync::Arc};

#[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Debug)]
pub enum Uri {
    Unknown,
    StdLib,
    File(PathBuf),
    Custom(usize),
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
            Uri::StdLib => write!(f, "stdlib"),
            Uri::File(path_buf) => write!(f, "file://{}", path_buf.to_string_lossy()),
            Uri::Custom(id) => write!(f, "custom://{:X}", id),
        }
    }
}

pub struct SourceManager {
    sources: HashMap<Uri, String>,
    custom_count: usize,
}

impl SourceManager {
    pub fn new() -> SourceManager {
        SourceManager {
            sources: HashMap::new(),
            custom_count: 0,
        }
    }

    pub fn refresh(&mut self, key: Uri) -> anyhow::Result<()> {
        match key {
            Uri::File(path_buf) => {
                self.load_from_file(path_buf)?;
            }
            _ => {}
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

    pub fn add_custom(&mut self, content: String) -> anyhow::Result<Uri> {
        let id = Uri::Custom(self.custom_count);
        self.custom_count += 1;
        let _ = self.sources.insert(id.clone(), content);

        Ok(id)
    }

    pub fn update_custom(&mut self, key: &Uri, content: String) -> anyhow::Result<()> {
        let Uri::Custom(_) = key else {
            bail!("Can only manually update custom uris");
        };
        let source = self
            .sources
            .get_mut(key)
            .ok_or_else(|| anyhow!("Source for {} not found!", key))?;
        *source = content;

        Ok(())
    }

    pub fn get(&self, key: &Uri) -> anyhow::Result<&String> {
        self.sources
            .get(key)
            .ok_or_else(|| anyhow!("Source for {} not found!", key))
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
