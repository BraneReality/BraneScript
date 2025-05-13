pub mod console_emiter;
use std::ops::Range;

pub use annotate_snippets;
use annotate_snippets::{Level, Message};

pub trait DiagnosticEmitter {
    fn emit(&self, diagnostic: Message);

    #[must_use = "must call emit() to see diagnostic"]
    fn error<'e>(
        &'e self,
        title: impl ToString,
        origin: impl ToString,
    ) -> DiagnosticBuilder<'e, Self> {
        DiagnosticBuilder {
            emitter: self,
            annotations: Vec::new(),
            title: title.to_string(),
            origins: vec![origin.to_string()],
            current_origin: 0,
            max_line_gap: 4,
            level: Level::Error,
        }
    }
}

struct Annotation {
    pub text: String,
    pub origin: usize,
    pub level: Level,
    pub span: Range<usize>,
}

pub struct DiagnosticBuilder<'e, E: DiagnosticEmitter + ?Sized> {
    emitter: &'e E,
    level: Level,
    annotations: Vec<Annotation>,
    max_line_gap: usize,
    title: String,
    origins: Vec<String>,
    current_origin: usize,
}

#[derive(Clone, Copy)]
struct TextPos {
    buf_index: usize,
    line: usize,
}

impl<'e, E: DiagnosticEmitter> DiagnosticBuilder<'e, E> {
    #[must_use = "must call emit() to see diagnostic"]
    pub fn info_at(self, span: Range<usize>, text: impl ToString) -> Self {
        self.annotate(span, Level::Info, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn warning_at(self, span: Range<usize>, text: impl ToString) -> Self {
        self.annotate(span, Level::Warning, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn err_at(self, span: Range<usize>, text: impl ToString) -> Self {
        self.annotate(span, Level::Error, text)
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn annotate(mut self, span: Range<usize>, level: Level, text: impl ToString) -> Self {
        self.annotations.push(Annotation {
            text: text.to_string(),
            level,
            span,
            origin: self.current_origin,
        });
        self
    }

    #[must_use = "must call emit() to see diagnostic"]
    pub fn change_origin(mut self, origin: impl ToString) -> Self {
        let origin = origin.to_string();
        if let Some(po) = self
            .origins
            .iter()
            .enumerate()
            .find_map(|(i, o)| if o == &origin { Some(i) } else { None })
        {
            self.current_origin = po;
            return self;
        }
        self.current_origin = self.origins.len();
        self.origins.push(origin);
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

    /*
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
    }*/

    pub fn emit(mut self, source_text: &str) {
        let message = self.level.title(self.title.as_str());
        self.annotations.sort_by(|a, b| {
            if a.origin != b.origin {
                return a.origin.cmp(&b.origin);
            }
            return a.span.start.cmp(&b.span.start);
        });
        let mut cursor = TextPos {
            buf_index: 0,
            line: 0,
        };
        struct Snippet {
            annotations: Vec<Annotation>,
            start_line: usize,
            end_line: usize,
            span: Range<usize>,
            origin: usize,
        }
        let mut snippets = Vec::new();
        let mut current_snippet: Option<Snippet> = None;
        for mut a in self.annotations {
            if let Some(s) = current_snippet.as_ref() {
                if s.origin != a.origin {
                    snippets.push(current_snippet.take().unwrap());
                    cursor = TextPos {
                        buf_index: 0,
                        line: 0,
                    };
                }
            }

            let cursor = Self::line_of(a.span.start, cursor, source_text);

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
                    origin: a.origin,
                    span: Range {
                        start: cursor.buf_index,
                        end: 0,
                    },
                })
            }

            let s = current_snippet.as_mut().unwrap();
            let span = a.span.clone();
            a.span = Range {
                start: span.start - s.span.start,
                end: span.end - s.span.start,
            };
            s.annotations.push(a);
            s.end_line = cursor.line; // TODO make end line account for mult-line annotations
            s.span.end = span.end.max(s.span.end);
        }

        if let Some(s) = current_snippet {
            snippets.push(s);
        }

        self.emitter
            .emit(snippets.iter().fold(message, |message, snippet| {
                message.snippet(
                    snippet.annotations.iter().fold(
                        annotate_snippets::Snippet::source(&source_text[snippet.span.clone()])
                            .line_start(snippet.start_line)
                            .fold(false),
                        |s, a| s.annotation(a.level.span(a.span.clone()).label(a.text.as_str())),
                    ),
                )
            }))
    }
}
