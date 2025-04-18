use crate::source::{SourceManager, Uri};
use anyhow::anyhow;
use type_sitter::{Parser, Tree};

use super::nodes::SourceFile;

pub struct Ast {
    parser: Parser<SourceFile<'static>>,
    tree: Tree<SourceFile<'static>>,
    source: Uri,
}

impl Ast {
    pub fn new(source: Uri, sm: &SourceManager) -> anyhow::Result<Ast> {
        let mut parser = Parser::new(&tree_sitter_branescript::LANGUAGE.into())?;
        let text = sm.get(&source)?;
        let tree = parser
            .parse(text, None)
            .map_err(|_| anyhow!("Failed to parse {}", source))?;
        Ok(Ast {
            parser,
            tree,
            source,
        })
    }

    pub fn update(&mut self, sm: &SourceManager) -> anyhow::Result<()> {
        self.tree = self
            .parser
            .parse(sm.get(&self.source)?, Some(&self.tree))
            .map_err(|_| anyhow!("Failed to parse document"))?;
        Ok(())
    }

    pub fn tree(&self) -> &Tree<SourceFile<'static>> {
        &self.tree
    }

    pub fn source(&self) -> &Uri {
        return &self.source;
    }
}
