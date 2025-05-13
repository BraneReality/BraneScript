use super::DiagnosticEmitter;
use annotate_snippets::{Message, Renderer};

pub struct ConsoleEmiter {
    r: Renderer,
}

impl ConsoleEmiter {
    pub fn new() -> ConsoleEmiter {
        ConsoleEmiter {
            r: Renderer::styled(),
        }
    }
}

impl DiagnosticEmitter for ConsoleEmiter {
    fn emit(&self, diagnostic: Message) {
        anstream::println!("{}", self.r.render(diagnostic));
    }
}
