use anyhow::Result;
use brane_script_compiler::parsed_document::ParsedDocument;
use clap::{Parser, Subcommand};
use tree_sitter_branescript::NODE_TYPES;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Documents to parse
    sources: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    for doc in cli.sources {
        let mut doc = ParsedDocument::new(doc.into(), None)?;
        let doc_ctx = doc.get_document_context()?;
        println!("Was able to parse out document context!");
        for module in doc_ctx.modules.iter() {
            println!("found module {}", module.0);
            println!("with pipelines:");
            for pipe in module.1.pipelines.iter() {
                println!("{}", pipe.0);
            }
        }
    }

    Ok(())
}
