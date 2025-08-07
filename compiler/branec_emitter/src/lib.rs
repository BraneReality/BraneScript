mod console_emitter;

pub use annotate_snippets::{Level, Message};
use branec_source::{SourceManager, Span};
pub use console_emitter::ConsoleEmitter;
use std::ops::Range;

pub trait DiagnosticEmitter {
    fn emit(&self, message: Message);
}

pub trait Diagnostic {
    fn emit(&self, emitter: &impl DiagnosticEmitter) -> anyhow::Result<()>;
}

#[derive(Clone)]
struct Annotation {
    pub text: String,
    pub level: Level,
    pub span: Span,
}

pub struct DiagnosticBuilder<'src> {
    level: Level,
    sources: &'src SourceManager,
    annotations: Vec<Annotation>,
    max_line_gap: usize,
    title: String,
}

#[derive(Clone, Copy)]
struct TextPos {
    buf_index: usize,
    line: usize,
}

pub fn log<'src>(title: &str, sources: &'src SourceManager) -> DiagnosticBuilder<'src> {
    DiagnosticBuilder::new(title, Level::Info, sources)
}

pub fn warning<'src>(title: &str, sources: &'src SourceManager) -> DiagnosticBuilder<'src> {
    DiagnosticBuilder::new(title, Level::Warning, sources)
}

pub fn error<'src>(title: &str, sources: &'src SourceManager) -> DiagnosticBuilder<'src> {
    DiagnosticBuilder::new(title, Level::Error, sources)
}

impl<'src> DiagnosticBuilder<'src> {
    #[must_use = "must call emit() to see diagnostic"]
    pub fn new(title: &str, level: Level, sources: &'src SourceManager) -> Self {
        DiagnosticBuilder {
            annotations: Vec::new(),
            title: title.to_string(),
            max_line_gap: 4,
            sources,
            level,
        }
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn info_at(self, span: Span, text: impl ToString) -> Self {
        self.annotate(span, Level::Info, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn warning_at(self, span: Span, text: impl ToString) -> Self {
        self.annotate(span, Level::Warning, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn err_at(self, span: Span, text: impl ToString) -> Self {
        self.annotate(span, Level::Error, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn annotate(mut self, span: Span, level: Level, text: impl ToString) -> Self {
        self.annotations.push(Annotation {
            text: text.to_string(),
            level,
            span,
        });
        self
    }

    fn line_of(char_index: usize, start: TextPos, source_text: &str) -> TextPos {
        let mut current = start.clone();
        let mut index = current.buf_index;
        for c in source_text[start.buf_index..char_index].chars() {
            index += 1;
            if c == '\n' {
                current.buf_index = index;
                current.line += 1;
            }
        }
        current
    }

    fn advance_lines(mut lines: usize, start: TextPos, source_text: &str) -> TextPos {
        let mut current = start;
        let mut index = current.buf_index;
        for c in source_text[start.buf_index..source_text.len()].chars() {
            index += 1;
            if c == '\n' {
                current.buf_index = index;
                current.line += 1;
                lines -= 1;
                if lines == 0 {
                    return current;
                }
            }
        }
        current
    }
}

impl<'src> Diagnostic for DiagnosticBuilder<'src> {
    fn emit(&self, emitter: &impl DiagnosticEmitter) -> anyhow::Result<()> {
        let message = self.level.title(self.title.as_str());
        let mut cursor = TextPos {
            buf_index: 0,
            line: 0,
        };
        struct Snippet<'src> {
            annotations: Vec<Annotation>,
            start_line: usize,
            end_line: usize,
            span: Span,
            source_text: &'src str,
            origin: String,
        }
        let mut snippets = Vec::new();
        let mut current_snippet: Option<Snippet> = None;
        for a in self.annotations.iter() {
            let mut a = a.clone();
            if let Some(s) = current_snippet.as_ref() {
                if s.span.source != a.span.source {
                    snippets.push(current_snippet.take().unwrap());
                    cursor = TextPos {
                        buf_index: 0,
                        line: 0,
                    };
                }
            }
            let source_text = self.sources.get(&a.span.source)?;

            let cursor = Self::line_of(a.span.range.start, cursor, source_text);

            if let Some(s) = current_snippet.as_ref() {
                if cursor.line - s.end_line > self.max_line_gap {
                    snippets.push(current_snippet.take().unwrap());
                }
            }

            if current_snippet.is_none() {
                current_snippet = Some(Snippet {
                    start_line: cursor.line,
                    end_line: cursor.line,
                    annotations: Vec::new(),
                    source_text,
                    origin: a.span.source.to_string(),
                    span: Span {
                        source: a.span.source.clone(),
                        range: Range {
                            start: cursor.buf_index,
                            end: cursor.buf_index,
                        },
                    },
                })
            }

            let s = current_snippet.as_mut().unwrap();
            let span = a.span.clone();
            a.span = Span {
                source: a.span.source,
                range: Range {
                    start: span.range.start - s.span.range.start,
                    end: span.range.end - s.span.range.start,
                },
            };
            s.annotations.push(a.clone());
            s.end_line = cursor.line; // TODO make end line account for mult-line annotations
            // Find next line, and then backtrack past the newline to get the end of the current
            // line
            let next_line = Self::advance_lines(1, cursor, &source_text);
            s.span.range.end = span.range.end.max(next_line.buf_index - 2);
        }

        if let Some(s) = current_snippet {
            snippets.push(s);
        }

        let message = snippets.iter().fold(message, |message, snippet| {
            message.snippet(
                snippet.annotations.iter().fold(
                    annotate_snippets::Snippet::source(
                        &snippet.source_text[snippet.span.range.clone()],
                    )
                    .origin(&snippet.origin)
                    .line_start(snippet.start_line + 1)
                    .fold(false),
                    |s, a| s.annotation(a.level.span(a.span.range.clone()).label(a.text.as_str())),
                ),
            )
        });
        emitter.emit(message);
        Ok(())
    }
}
