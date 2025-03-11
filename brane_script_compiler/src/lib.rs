pub mod compiler;
pub mod document_context;
pub mod parsed_document;
pub mod parser;

use std::ops::Range;

use annotate_snippets::Message;

pub enum MessageType {
    Verbose,
    Log,
    Warning,
    Error,
}

pub struct ToolchainMessage {
    pub message: String,
    pub r#type: MessageType,
}
