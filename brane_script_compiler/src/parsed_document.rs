use crate::{
    document_context::{DocumentContext, TextSource},
    parser::{nodes::SourceFile, DocumentParser},
};
use anyhow::anyhow;
use std::{path::PathBuf, sync::Arc};
use type_sitter::{Node, Parser, Tree};

pub use crate::ToolchainMessage;

pub struct ParsedDocument {
    parser: Parser<SourceFile<'static>>,
    tree: Option<Tree<SourceFile<'static>>>,
    path: PathBuf,
    source: String,
    cached_ctx: Option<DocumentContext>,
    messages: Vec<ToolchainMessage>,
}

impl ParsedDocument {
    pub fn new(path: PathBuf, current_source: Option<String>) -> anyhow::Result<ParsedDocument> {
        let current_source = if let Some(source_text) = current_source {
            source_text
        } else {
            std::fs::read_to_string(&path)?
        };

        let parser = Parser::new(&tree_sitter_branescript::LANGUAGE.into())?;
        Ok(ParsedDocument {
            parser,
            tree: None,
            path,
            source: current_source,
            cached_ctx: None,
            messages: Vec::new(),
        })
    }

    pub fn source(&self) -> &str {
        return self.source.as_str();
    }

    pub fn get_document_context(&mut self) -> anyhow::Result<&DocumentContext> {
        let source = Arc::new(TextSource {
            uri: format!("file://{}", self.path.to_string_lossy()),
        });

        self.tree = Some(
            self.parser
                .parse(&self.source, self.tree.as_ref())
                .map_err(|_: ()| anyhow!("Failed to parse document"))?,
        );
        let tree = self.tree.clone().unwrap();

        let source_file = tree.root_node().map_err(|err| {
            anyhow!(format!(
                "Was expecting {} but found \"{}\" with content: {}",
                err.kind,
                err.node.kind(),
                &self.source[err.node.byte_range()]
            ))
        })?;
        let parseResult = DocumentParser::parse_document(&self.source, source_file, source);
        if parseResult.document.is_some() {
            self.cached_ctx = parseResult.document;
        }

        return Ok(self.cached_ctx.as_ref().unwrap());
    }
}
