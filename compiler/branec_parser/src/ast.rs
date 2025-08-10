use branec_source::Span;

#[derive(Clone)]
pub enum LiteralKind {
    Float(f64),
    Int(i128),
    String(String),
}

#[derive(Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Clone)]
pub struct Ident {
    pub span: Span,
    pub text: String,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

#[derive(PartialEq, Clone)]
pub struct TemplateParam(pub Ident);

#[derive(PartialEq, Clone)]
pub struct TemplateArg(pub Ty);

#[derive(PartialEq, Clone, Copy, Debug)]
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

#[derive(PartialEq, Clone)]
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

#[derive(Clone)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, PartialEq)]
pub struct PathSegment {
    pub ident: Ident,
    pub template_args: Vec<TemplateArg>,
}

#[derive(Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

use std::fmt;
impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let segments: Vec<String> = self
            .segments
            .iter()
            .map(|seg| seg.ident.text.clone())
            .collect();
        write!(f, "{}", segments.join("."))
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

#[derive(Clone)]
pub struct Struct {
    pub ident: Ident,
    pub span: Span,
    pub fields: Vec<(Ident, Ty)>,
    pub template_params: Vec<TemplateParam>,
}

#[derive(Clone)]
pub struct Enum {
    pub ident: Ident,
    pub span: Span,
    pub variants: Vec<(Ident, Option<Ty>)>,
    pub template_params: Vec<TemplateParam>,
}

#[derive(Clone)]
pub struct Function {
    pub ident: Ident,
    pub span: Span,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub body: Block,
    pub template_params: Vec<TemplateParam>,
}

#[derive(Clone)]
pub enum CaseKind {
    Int(i128),
    EnumVariant(Ident, Option<Ident>),
}

#[derive(Clone)]
pub struct MatchBranch {
    pub span: Span,
    pub case: CaseKind,
    pub body: Stmt,
}

#[derive(Clone)]
pub enum StmtKind {
    Expression(Expr),
    Assign(Expr, Expr),
    VariableDef(Ty, Ident, Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Match(Span, Expr, Vec<MatchBranch>),
    Block(Block),
    Return(Option<Expr>),
}

#[derive(Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone)]
pub enum PipelineStage {
    Block(Block),
    Fn(Function),
}

#[derive(Clone)]
pub struct Pipeline {
    pub ident: Ident,
    pub span: Span,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub stages: Vec<PipelineStage>,
}

#[derive(Clone)]
pub enum DefKind {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Pipeline(Pipeline),
    Link(Ident),
    Use(Path),
    Namespace(Ident, Vec<Def>),
}

#[derive(Clone)]
pub struct Def {
    pub kind: DefKind,
    pub span: Span,
}
impl Def {
    pub fn ident(&self) -> Option<&Ident> {
        match &self.kind {
            DefKind::Struct(s) => Some(&s.ident),
            DefKind::Enum(e) => Some(&e.ident),
            DefKind::Function(f) => Some(&f.ident),
            DefKind::Pipeline(p) => Some(&p.ident),
            DefKind::Link(l) => Some(l),
            DefKind::Namespace(n, _) => Some(n),
            _ => None,
        }
    }
}
