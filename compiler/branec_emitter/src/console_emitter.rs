use super::DiagnosticEmitter;
use annotate_snippets::{Message, Renderer};

pub struct ConsoleEmitter {
    r: Renderer,
}

impl ConsoleEmitter {
    pub fn new() -> Self {
        Self {
            r: Renderer::styled(),
        }
    }
}

impl DiagnosticEmitter for ConsoleEmitter {
    fn emit(&self, diagnostic: Message) {
        anstream::println!("{}", self.r.render(diagnostic));
    }
}
