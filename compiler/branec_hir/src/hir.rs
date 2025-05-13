use branec_source::Span;
use std::{collections::HashMap, fmt::Display, rc::Rc};

type HirId = u64;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub id: HirId,
}

#[derive(Clone, Debug)]
pub enum GenericArg {
    Path(Rc<Path>),
    Const(Rc<ConstValue>),
}

#[derive(Clone, Debug)]
pub struct PathSegment {
    pub ident: Ident,
    pub args: Option<Vec<GenericArg>>,
    pub infer_args: bool,
}

#[derive(Clone, Debug)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<Box<PathSegment>>,
}

#[derive(Clone, Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BorrowKind {
    /// A normal borrow, `&$expr` or `&mut $expr`.
    /// The resulting type is either `&'a T` or `&'a mut T`
    /// where `T = typeof($expr)` and `'a` is some lifetime.
    Ref,
    /// A raw borrow, `&raw const $expr` or `&raw mut $expr`.
    /// The resulting type is either `*const T` or `*mut T`
    /// where `T = typeof($expr)`.
    Raw,
}

#[derive(Clone, Debug)]
pub enum Mutability {
    // N.B. Order is deliberate, so that Not < Mut
    Not,
    Mut,
}

#[derive(Clone, Debug)]
pub struct MutTy {
    pub ty: Box<Ty>,
    pub mutability: Mutability,
}

#[derive(Clone, Debug)]
pub enum TyKind {
    /// A variable-length slice (`[T]`).
    // TODO Slice(P<Ty>),

    /// A fixed length array (`[T; n]`).
    // TODO Array(P<Ty>, AnonConst),

    /// A raw pointer (`*const T` or `*mut T`).
    Ptr(MutTy),
    /// A reference (`&'a T` or `&'a mut T`).
    Ref(MutTy),

    /// An anonymous struct
    Struct(Vec<Param>),

    /// A path (`module::module::...::Type`)
    Path(Path),

    /// Inferred type of a `self` or `&self` argument in a method.
    ImplicitSelf,
}

#[derive(Clone, Debug)]
pub enum TypeModifiers {
    MutRef,
    ConstRef,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub span: Span,
    pub base_type: Box<Path>,
    pub modifiers: Vec<TypeModifiers>,
}

#[derive(Clone, Debug)]
pub struct Value {
    pub span: Span,
    pub label: Option<Ident>,
    pub r#type: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct Let {
    pub span: Span,
    pub defined_value: Rc<Value>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub expressions: Vec<Rc<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ident: Rc<Ident>, // TODO upgrade to pattern
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FnRetTy {
    /// Returns type is not specified.
    ///
    /// Functions default to `()` and pipeline stages/closures
    /// default to inference.
    /// Span points to where return type would be inserted.
    Default(Span),
    /// Everything else.
    Ty(Rc<Ty>),
}

#[derive(Clone, Debug)]
pub struct CallSig {
    pub span: Span,
    pub inputs: Vec<Param>,
    pub output: FnRetTy,
}

#[derive(Clone, Debug)]
pub struct PipelineStage {
    pub span: Span,
    pub ident: Option<Ident>,
    pub call_sig: CallSig,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub span: Span,
    pub dest: Rc<Expr>,
    pub hir: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub enum ConstValueData {
    Bool(bool),
    Char(char),
    I64(i64),
    U64(u64),
    F64(f64),
    Str(String),
}

#[derive(Clone, Debug)]
pub struct ConstValue {
    span: Span,
    data: ConstValueData,
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            ConstValueData::Bool(value) => write!(f, "{}", value),
            ConstValueData::Char(value) => write!(f, "{}", value),
            ConstValueData::I64(value) => write!(f, "{}", value),
            ConstValueData::U64(value) => write!(f, "{}", value),
            ConstValueData::F64(value) => write!(f, "{}", value),
            ConstValueData::Str(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub span: Span,
    pub base_expression: Rc<Expr>,
    pub member: usize,
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub ident: Rc<Ident>,
    pub expression: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub span: Span,
    /// If type is non, this is an anon struct
    pub r#type: Option<Rc<Path>>,
    pub members: Vec<Rc<FieldDef>>,
}

#[derive(Clone, Debug)]
pub struct Call {
    pub span: Span,
    pub callable: Rc<Expr>,
    pub args: Rc<StructDef>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub span: Span,
    pub ident: Option<Rc<Ident>>,
    pub members: Vec<Rc<Value>>,
    pub packed: bool,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub span: Rc<Span>,
    pub ident: Rc<Ident>,
    pub call_sig: Rc<CallSig>,
    pub body: Rc<Block>,
}

#[derive(Clone, Debug)]
pub enum TraitMember {
    Fn(Rc<CallSig>),
    Type(Rc<Ident>),
    Const(Rc<Ident>, Rc<Path>),
}

#[derive(Clone, Debug)]
pub struct TraitDef {
    pub ident: Rc<Ident>,
    pub members: HashMap<String, Rc<TraitMember>>,
}

#[derive(Clone, Debug)]
pub enum ImplMembers {
    Fn(Rc<Function>),
    Type(Rc<Ident>, Rc<Path>),
    Const(Rc<Ident>, Rc<ConstValue>),
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub r#trait: Option<Rc<Path>>,
    pub r#type: Rc<Path>,
    pub members: HashMap<String, ImplMembers>,
}

#[derive(Clone, Debug)]
pub struct Pipeline {
    pub span: Rc<Span>,
    pub ident: Rc<Ident>,
    pub call_sig: Rc<CallSig>,
    pub stages: Vec<Rc<PipelineStage>>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub span: Span,
    pub ident: Rc<Ident>,
    pub links: Vec<Rc<Path>>,
    pub structs: HashMap<String, Rc<Struct>>,
    pub functions: HashMap<String, Rc<Function>>,
    pub pipelines: HashMap<String, Rc<Pipeline>>,
}

#[derive(Clone, Debug)]
pub struct Hir {
    pub modules: HashMap<String, Rc<Module>>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Scope(Rc<Block>),
    Assignment(Rc<Assignment>),
    Let(Rc<Let>),
    Identifier(Rc<Ident>),
    ConstValue(Rc<ConstValue>),
    Path(Rc<Path>),
    Field(Rc<Field>),
    Struct(Rc<Struct>),
    Call(Rc<Call>),
}

impl Expr {
    pub fn span(&self) -> &Span {
        use Expr::*;
        match self {
            Assignment(c) => &c.span,
            Let(c) => &c.span,
            ConstValue(c) => &c.span,
            Path(c) => &c.span,
            Field(c) => &c.span,
            Struct(c) => &c.span,
            Call(c) => &c.span,
            Scope(block) => &block.span,
            Identifier(ident) => &ident.span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Value(Rc<Value>),
    Type(Rc<Type>),
    CallSig(Rc<CallSig>),
    Expr(Rc<Expr>),
    PipelineStage(Rc<PipelineStage>),
    FieldDef(Rc<FieldDef>),
    Function(Rc<Function>),
    Pipeline(Rc<Pipeline>),
    StructDef(Rc<StructDef>),
    Module(Rc<Module>),
}
