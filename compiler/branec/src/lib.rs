use anyhow::{anyhow, bail};
use brane_core::ir::IRModule;
use branec_emitter::{self as emt, Diagnostic, DiagnosticEmitter};
use branec_parser::ast;
use branec_source::{SourceManager, Span, Uri};
use chumsky::input::Input;
use chumsky::span::Span as _;
use chumsky::Parser;
use std::collections::HashMap;
use std::sync::Arc;

/// Information about how to compile brane script projects
pub struct CompileContext<E: DiagnosticEmitter> {
    pub emitter: E,
    pub sources: SourceManager,
    pub loaded: HashMap<Uri, Vec<ast::Def>>,
}

impl<E: DiagnosticEmitter> CompileContext<E> {
    pub fn emit_module(&mut self, module_uri: &Uri) -> anyhow::Result<()> {
        let defs = {
            self.sources.refresh(module_uri.clone())?;
            let source = self.sources.get(module_uri)?;
            let uri = Arc::new(module_uri.clone());
            let source =
                source.map_span(move |span| Span::new(uri.clone(), span.start()..span.end()));
            match branec_parser::parser().parse(source).into_result() {
                Ok(defs) => defs,
                Err(errors) => {
                    let err_msg = format!("Failed to parse {}:", module_uri);
                    let diag =
                        errors
                            .iter()
                            .fold(emt::error(&err_msg, &self.sources), |diag, error| {
                                // Build the top-level message
                                let span = error.span().clone();
                                let message = match error.reason() {
                                    chumsky::error::RichReason::ExpectedFound {
                                        expected,
                                        found: _,
                                    } => {
                                        let expected: Vec<_> =
                                            expected.iter().map(|e| format!("'{}'", e)).collect();

                                        if expected.is_empty() {
                                            format!("Unexpected")
                                        } else {
                                            let expected = expected.join(", ");
                                            format!("Was expecting {}", expected)
                                        }
                                    }
                                    chumsky::error::RichReason::Custom(msg) => msg.clone(),
                                };
                                diag.err_at(span, message)
                            });
                    diag.emit(&self.emitter)?;
                    bail!(err_msg);
                }
            }
        };

        for def in defs {
            println!(
                "found def {}",
                match &def.kind {
                    ast::DefKind::Struct(s) => s.ident.text.as_str(),
                    ast::DefKind::Enum(e) => e.ident.text.as_str(),
                    ast::DefKind::Function(function) => function.ident.text.as_str(),
                    ast::DefKind::Pipeline(pipeline) => pipeline.ident.text.as_str(),
                    ast::DefKind::Link(ident) => ident.text.as_str(),
                    ast::DefKind::Use(path) => "path",
                    ast::DefKind::Namespace(ident, defs) => ident.text.as_str(),
                }
            )
        }

        Ok(())
    }
}
