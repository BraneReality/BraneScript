use brane_core::ir::Uri;
use branec_source::Span;
use std::{
    fmt::{self, Display},
    sync::Arc,
};

#[derive(Clone)]
pub enum LiteralKind {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    String(String),
}

impl LiteralKind {
    pub fn as_i8(&self) -> Option<i8> {
        if let LiteralKind::I8(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_i16(&self) -> Option<i16> {
        if let LiteralKind::I16(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        if let LiteralKind::I32(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        if let LiteralKind::I64(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        if let LiteralKind::U8(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_u16(&self) -> Option<u16> {
        if let LiteralKind::U16(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        if let LiteralKind::U32(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        if let LiteralKind::U64(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_f32(&self) -> Option<f32> {
        if let LiteralKind::F32(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        if let LiteralKind::F64(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_signed_int(&self) -> Option<i64> {
        match self {
            LiteralKind::I8(v) => Some(*v as i64),
            LiteralKind::I16(v) => Some(*v as i64),
            LiteralKind::I32(v) => Some(*v as i64),
            LiteralKind::I64(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_unsigned_int(&self) -> Option<u64> {
        match self {
            LiteralKind::U8(v) => Some(*v as u64),
            LiteralKind::U16(v) => Some(*v as u64),
            LiteralKind::U32(v) => Some(*v as u64),
            LiteralKind::U64(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i128> {
        match self {
            LiteralKind::U8(v) => Some(*v as i128),
            LiteralKind::U16(v) => Some(*v as i128),
            LiteralKind::U32(v) => Some(*v as i128),
            LiteralKind::U64(v) => Some(*v as i128),
            LiteralKind::I8(v) => Some(*v as i128),
            LiteralKind::I16(v) => Some(*v as i128),
            LiteralKind::I32(v) => Some(*v as i128),
            LiteralKind::I64(v) => Some(*v as i128),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            LiteralKind::F32(v) => Some(*v as f64),
            LiteralKind::F64(v) => Some(*v),
            _ => None,
        }
    }
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::I8(v) => write!(f, "{}i8", v),
            LiteralKind::I16(v) => write!(f, "{}i16", v),
            LiteralKind::I32(v) => write!(f, "{}i32", v),
            LiteralKind::I64(v) => write!(f, "{}i64", v),
            LiteralKind::U8(v) => write!(f, "{}u8", v),
            LiteralKind::U16(v) => write!(f, "{}u16", v),
            LiteralKind::U32(v) => write!(f, "{}u32", v),
            LiteralKind::U64(v) => write!(f, "{}u64", v),
            LiteralKind::F32(v) => write!(f, "{}f32", v),
            LiteralKind::F64(v) => write!(f, "{}f64", v),
            LiteralKind::Bool(v) => write!(f, "{}", v),
            LiteralKind::Char(c) => write!(f, "'{}'", c),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub text: String,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

#[derive(PartialEq, Clone)]
pub struct TemplateParam(pub Ident);

impl Display for TemplateParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct TemplateArg(pub Ty);

impl Display for TemplateArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
}

impl Display for NativeTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NativeTy::I8 => write!(f, "i8"),
            NativeTy::I16 => write!(f, "i16"),
            NativeTy::I32 => write!(f, "i32"),
            NativeTy::I64 => write!(f, "i64"),
            NativeTy::U8 => write!(f, "u8"),
            NativeTy::U16 => write!(f, "u16"),
            NativeTy::U32 => write!(f, "u32"),
            NativeTy::U64 => write!(f, "u64"),
            NativeTy::F32 => write!(f, "f32"),
            NativeTy::F64 => write!(f, "f64"),
            NativeTy::Bool => write!(f, "bool"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum TyKind {
    Native(NativeTy),
    /// (element type, length, span)
    Array(Box<Ty>, u32, Span),
    /// (is mut, type)
    Ptr(bool, Option<Box<Ty>>),
    /// (is mut, type)
    Slice(bool, Option<Box<Ty>>),
    Tuple(Vec<Box<Ty>>),
    Struct(Vec<(Ident, Box<Ty>)>),
    Path(Path),
    //TODO Fn,Pipe
}

impl Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Native(n) => write!(f, "{}", n),
            TyKind::Array(ty, len, _) => write!(f, "[{}; {}]", ty, len),
            TyKind::Ptr(is_mut, ty) => match (is_mut, ty) {
                (true, Some(t)) => write!(f, "*mut {}", t),
                (false, Some(t)) => write!(f, "*const {}", t),
                (true, None) => write!(f, "*mut ?"),
                (false, None) => write!(f, "*const ?"),
            },
            TyKind::Slice(is_mut, ty) => match (is_mut, ty) {
                (true, Some(t)) => write!(f, "*mut [{}]", t),
                (false, Some(t)) => write!(f, "*[{}]", t),
                (true, None) => write!(f, "*mut [?]"),
                (false, None) => write!(f, "*[?]"),
            },
            TyKind::Path(p) => write!(f, "{}", p),
            TyKind::Tuple(items) => {
                let mut s = String::new();
                s.push('(');
                for (i, it) in items.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", it));
                }
                s.push(')');
                write!(f, "{}", s)
            }
            TyKind::Struct(fields) => {
                let mut s = String::new();
                s.push('{');
                for (i, (id, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}: {}", id, ty));
                }
                s.push('}');
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
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
    /// (Function Value, Args)
    Call(Box<Expr>, Vec<Expr>),
    /// (Self, Function Name, Args)
    ImplcitSelfCall(Box<Expr>, PathSegment, Vec<Expr>),
}

#[derive(Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PathSegment {
    pub ident: Ident,
    pub template_args: Vec<TemplateArg>,
}
impl Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.template_args.is_empty() {
            let args: Vec<String> = self
                .template_args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect();
            write!(f, "{}<{}>", self.ident, args.join(", "))
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

impl Path {
    pub fn empty() -> Self {
        Path {
            segments: vec![],
            span: Span {
                range: Default::default(),
                source: Arc::new(Uri::Unknown),
            },
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let segments: Vec<String> = self.segments.iter().map(|seg| seg.to_string()).collect();
        write!(f, "{}", segments.join("::"))
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
    pub is_node_def: bool,
}

#[derive(Clone)]
pub enum CaseKind {
    Int(usize),
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
pub enum DefKind {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    /// (span of "using" token, ident path, module URI string)
    /// MODULE URI STRING IS TEMPORARY FOR TESTING UNTIL BETTER SOLUTION
    Using(Span, Option<Path>, Option<String>),
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
            DefKind::Namespace(n, _) => Some(n),
            _ => None,
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprKind::Literal(l) => write!(f, "{}", l),
            ExprKind::Array(items) => {
                let mut s = String::new();
                s.push('[');
                for (i, it) in items.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", it));
                }
                s.push(']');
                write!(f, "{}", s)
            }
            ExprKind::Tuple(items) => {
                let mut s = String::new();
                s.push('(');
                for (i, it) in items.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", it));
                }
                s.push(')');
                write!(f, "{}", s)
            }
            ExprKind::Struct(path, fields) => {
                let mut s = String::new();
                if let Some(p) = path {
                    s.push_str(&format!("{} ", p));
                }
                s.push('{');
                for (i, (id, ex)) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}: {}", id, ex));
                }
                s.push('}');
                write!(f, "{}", s)
            }
            ExprKind::Path(p) => write!(f, "{}", p),
            ExprKind::Ref(e) => write!(f, "&{}", e),
            ExprKind::Deref(e) => write!(f, "*{}", e),
            ExprKind::Field(e, seg) => write!(f, "{}.{}", e, seg),
            ExprKind::Call(e, args) => {
                write!(f, "{}(", e)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
            ExprKind::ImplcitSelfCall(expr, path_segment, exprs) => {
                write!(f, "{}.{}(", expr, path_segment)?;
                for (i, a) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("struct {}", self.ident));
        if !self.template_params.is_empty() {
            s.push('<');
            for (i, tp) in self.template_params.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", tp));
            }
            s.push('>');
        }
        s.push_str(" { ");
        for (i, (id, ty)) in self.fields.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&format!("{}: {}", id, ty));
        }
        s.push_str(" }");
        write!(f, "{}", s)
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("enum {}", self.ident));
        if !self.template_params.is_empty() {
            s.push('<');
            for (i, tp) in self.template_params.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", tp));
            }
            s.push('>');
        }
        s.push_str(" { ");
        for (i, (id, ty)) in self.variants.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            if let Some(t) = ty {
                s.push_str(&format!("{}({})", id, t));
            } else {
                s.push_str(&format!("{}", id));
            }
        }
        s.push_str(" }");
        write!(f, "{}", s)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("fn {}", self.ident));
        if !self.template_params.is_empty() {
            s.push('<');
            for (i, tp) in self.template_params.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", tp));
            }
            s.push('>');
        }
        s.push('(');
        for (i, (id, ty)) in self.params.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&format!("{}: {}", id, ty));
        }
        s.push(')');
        if let Some(ret) = &self.ret_ty {
            s.push_str(&format!(" -> {}", ret));
        }
        s.push(' ');
        s.push_str(&format!("{}", self.body));
        write!(f, "{}", s)
    }
}

impl Display for CaseKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CaseKind::Int(v) => write!(f, "{}", v),
            CaseKind::EnumVariant(id, inner) => {
                if let Some(i) = inner {
                    write!(f, "{}({})", id, i)
                } else {
                    write!(f, "{}", id)
                }
            }
        }
    }
}

impl Display for MatchBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} => {}", self.case, self.body)
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StmtKind::Expression(e) => write!(f, "{}", e),
            StmtKind::Assign(l, r) => write!(f, "{} = {}", l, r),
            StmtKind::VariableDef(ty, id, ex) => write!(f, "let {}: {} = {}", id, ty, ex),
            StmtKind::If(cond, then_b, else_b) => {
                if let Some(e) = else_b {
                    write!(f, "if ({}) {} else {}", cond, then_b, e)
                } else {
                    write!(f, "if ({}) {}", cond, then_b)
                }
            }
            StmtKind::While(cond, body) => write!(f, "while ({}) {}", cond, body),
            StmtKind::Match(_, expr, branches) => {
                let mut s = String::new();
                s.push_str(&format!("match {} {{ ", expr));
                for (i, b) in branches.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}", b));
                }
                s.push_str(" }");
                write!(f, "{}", s)
            }
            StmtKind::Block(b) => write!(f, "{}", b),
            StmtKind::Return(None) => write!(f, "return"),
            StmtKind::Return(Some(e)) => write!(f, "return {}", e),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push('{');
        if !self.statements.is_empty() {
            s.push(' ');
        }
        for (i, st) in self.statements.iter().enumerate() {
            if i > 0 {
                s.push(' ');
            }
            s.push_str(&format!("{};", st));
        }
        if !self.statements.is_empty() {
            s.push(' ');
        }
        s.push('}');
        write!(f, "{}", s)
    }
}

impl Display for DefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DefKind::Struct(s) => write!(f, "{}", s),
            DefKind::Enum(e) => write!(f, "{}", e),
            DefKind::Function(fn_) => write!(f, "{}", fn_),
            DefKind::Using(_, p, from) => write!(
                f,
                "using {}{};",
                if let Some(p) = p {
                    format!("{}", p)
                } else {
                    "*".into()
                },
                if let Some(from) = from {
                    format!(" from {}", from)
                } else {
                    "".into()
                }
            ),
            DefKind::Namespace(id, defs) => {
                let mut s = String::new();
                s.push_str(&format!("namespace {} {{ ", id));
                for (i, d) in defs.iter().enumerate() {
                    if i > 0 {
                        s.push(' ');
                    }
                    s.push_str(&format!("{}", d));
                }
                s.push_str(" }");
                write!(f, "{}", s)
            }
        }
    }
}

impl Display for Def {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
