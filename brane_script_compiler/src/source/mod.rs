use anyhow::anyhow;
use std::{collections::HashMap, fmt::Display, ops::Range, path::PathBuf, sync::Arc};

#[derive(Clone, Eq, PartialEq, PartialOrd, Hash)]
pub enum Uri {
    File(PathBuf),
}

impl Display for Uri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

#[derive(PartialEq, Eq, Clone)]
pub struct TextSource {
    pub source: Arc<Uri>,
    pub span: Range<usize>,
}
