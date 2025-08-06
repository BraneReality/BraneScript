use std::{ops::Range, sync::Arc};

#[derive(Clone)]
pub struct Span {
    pub range: Range<usize>,
    pub source: Arc<String>,
}

impl chumsky::span::Span for Span {
    type Context = Arc<String>;

    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            range,
            source: context,
        }
    }

    fn context(&self) -> Self::Context {
        self.source.clone()
    }

    fn start(&self) -> Self::Offset {
        self.range.start.clone()
    }

    fn end(&self) -> Self::Offset {
        self.range.end.clone()
    }
}

pub enum LiteralKind {
    Float(f64),
    Int(i128),
    String(String),
}

pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

pub struct Ident {
    pub span: Span,
    pub text: String,
}

pub struct TemplateParam(pub Ident);

pub struct TemplateArg(pub Ty);

pub struct PathSegment {
    pub ident: Ident,
    pub template_args: Vec<TemplateArg>,
}

#[derive(PartialEq)]
pub enum NativeTy {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    String,
}

pub enum TyKind {
    Native(NativeTy),
    /// (is mut, type)
    Ptr(bool, Option<Box<Ty>>),
    /// (is mut, type)
    Slice(bool, Option<Box<Ty>>),
    Path(Path),
    Tuple(Vec<Box<Ty>>),
    Struct(Vec<(Ident, Box<Ty>)>),
    //TODO Fn,Pipe
}

pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

pub enum ExprKind {
    Literal(Literal),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Struct(Option<Path>, Vec<(Ident, Expr)>),
    Path(Path),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Field(Box<Expr>, PathSegment),
    Call(Box<Expr>, Vec<Expr>),
}

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

pub struct Struct {
    pub ident: Ident,
    pub span: Span,
    pub fields: Vec<(Ident, Ty)>,
    pub template_params: Vec<TemplateParam>,
}

pub struct Enum {
    pub ident: Ident,
    pub span: Span,
    pub variants: Vec<(Ident, Option<Ty>)>,
    pub template_params: Vec<TemplateParam>,
}

pub struct Function {
    pub ident: Ident,
    pub span: Span,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub body: Block,
    pub template_params: Vec<TemplateParam>,
}

pub enum CaseKind {
    Int(i128),
    EnumVariant(Ident, Option<Ident>),
}

pub struct MatchBranch {
    pub span: Span,
    pub case: CaseKind,
    pub body: Stmt,
}

pub enum StmtKind {
    Expression(Expr),
    Assign(Expr, Expr),
    VariableDef(Ty, Ident, Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Match(Span, Expr, Vec<MatchBranch>),
    Block(Block),
}

pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

pub enum PipelineStage {
    Block(Block),
    Fn(Function),
}

pub struct Pipeline {
    pub ident: Ident,
    pub span: Span,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub stages: Vec<PipelineStage>,
}

pub enum DefKind {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Pipeline(Pipeline),
    Namespace(Ident, Vec<Def>),
}

pub struct Def {
    pub kind: DefKind,
    pub span: Span,
}
