//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use branec::CompileContext;
use branec_source::{SourceManager, Uri};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Script to evaluate
    input: String,
}

fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let mut ctx = CompileContext {
        emitter: branec_emitter::ConsoleEmitter::new(),
        sources: SourceManager::new(),
        loaded: Default::default(),
    };

    ctx.emit_module(&Uri::File(cli.input.into()))?;

    Ok(())
}
