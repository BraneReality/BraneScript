use branec_ast::{BinOp, FloatTy, IntTy, LitKind, UintTy, UnOp};
use branec_source::Span;
use branec_symbols::Symbol;
use std::rc::Rc;

pub type ItemLocalId = u64;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LocalDefId {
    pub local_def_index: DefIndex,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct OwnerId {
    pub def_id: LocalDefId,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct HirId {
    pub owner: OwnerId,
    pub local_id: ItemLocalId,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum DefKind {
    // Type namespace
    Group,
    /// Refers to the struct itself, [`DefKind::Ctor`] refers to its constructor if it exists.
    Struct,
    //Union,
    Enum,
    /// Refers to the variant itself, [`DefKind::Ctor`] refers to its constructor if it exists.
    Variant,
    Trait,
    /// Type alias: `type Foo = Bar;`
    //TODO TyAlias,
    /// Type from an `extern` block.
    //TODO? ForeignTy,
    /// Trait alias: `trait IntIterator = Iterator<Item = i32>;`
    //TODO? TraitAlias,
    /// Associated type: `trait MyTrait { type Assoc; }`
    //TODO AssocTy,
    /// Type parameter: the `T` in `struct Vec<T> { ... }`
    TyParam,

    // Value namespace
    Fn,
    Const,
    /// Constant generic parameter: `struct Foo<const N: usize> { ... }`
    ConstParam,
    /// Associated function: `impl MyStruct { fn associated() {} }`
    /// or `trait Foo { fn associated() {} }`
    AssocFn,
    /// Associated constant: `trait MyTrait { const ASSOC: usize; }`
    AssocConst,

    // Macro namespace
    //TODO Macro(MacroKind),

    // Not namespaced (or they are, but we don't treat them so)
    //TODO ExternMod,
    Use,
    /// An `extern` block.
    //TODO? ForeignGroup,
    /// Anonymous constant, e.g. the `1 + 2` in `[u8; 1 + 2]`.
    ///
    /// Not all anon-consts are actually still relevant in the HIR. We lower
    /// trivial const-arguments directly to `hir::ConstArgKind::Path`, at which
    /// point the definition for the anon-const ends up unused and incomplete.
    ///
    /// We do not provide any a `Span` for the definition and pretty much all other
    /// queries also ICE when using this `DefId`. Given that the `DefId` of such
    /// constants should only be reachable by iterating all definitions of a
    /// given crate, you should not have to worry about this.
    //TODO? AnonConst,
    /// An inline constant, e.g. `const { 1 + 2 }`
    //TODO? InlineConst,
    /// Opaque type, aka `impl Trait`.
    OpaqueTy,
    /// A field in a struct, enum or union. e.g.
    /// - `bar` in `struct Foo { bar: u8 }`
    /// - `Foo::Bar::0` in `enum Foo { Bar(u8) }`
    Field,
    Impl {
        of_trait: bool,
    },
    // A closure, coroutine, or coroutine-closure.
    //
    // These are all represented with the same `ExprKind::Closure` in the AST and HIR,
    // which makes it difficult to distinguish these during def collection. Therefore,
    // we treat them all the same, and code which needs to distinguish them can match
    // or `hir::ClosureKind` or `type_of`.
    //TODO Closure,
}

pub type ModNum = u64;
pub type DefIndex = u64;

pub const LOCAL_MODULE: ModNum = 0;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefId {
    pub index: DefIndex,
    pub module: ModNum,
}

// A resolved hir node
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Res {
    Def(DefKind, DefId),
    PrimTy(PrimTy),
    Local(HirId),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PrimTy {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
    Bool,
    Char,
}

pub type Lit = (Span, LitKind);

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ItemId {
    pub owner_id: OwnerId,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BodyId {
    pub hir_id: HirId,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct StageId {
    pub hir_id: HirId,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub id: HirId,
    pub sym: Symbol,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Path {
    pub span: Span,
    pub res: Res,
    pub segments: Vec<PathSegment>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PathSegment {
    pub ident: Ident,
    pub hir_id: HirId,
    pub res: Res,
    //pub args: Option<GenericArgs>,
    //pub infer_args: bool
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum QPath {
    /// Path to a definition, optionally "fully-qualified" with a `Self`
    /// type, if the path points to an associated item in a trait.
    ///
    /// E.g., an unqualified path like `Clone::clone` has `None` for `Self`,
    /// while `<Vec<T> as Clone>::clone` has `Some(Vec<T>)` for `Self`,
    /// even though they both have the same two-segment `Clone::clone` `Path`.
    Resolved(Option<Rc<Ty>>, Rc<Path>),

    /// Type-related paths (e.g., `<T>::default` or `<T>::Output`).
    /// Will be resolved by type-checking to an associated item.
    ///
    /// UFCS source paths can desugar into this, with `Vec::new` turning into
    /// `<Vec>::new`, and `T::X::Y::method` into `<<<T>::X>::Y>::method`,
    /// the `X` and `Y` nodes each being a `TyKind::Path(QPath::TypeRelative(..))`.
    TypeRelative(Rc<Ty>, PathSegment),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConstArgKind {
    Path(QPath),
    //Anon(AnonConst),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConstArg {
    pub hir_id: HirId,
    pub kind: ConstArgKind,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct MutTy {
    pub ty: Rc<Ty>,
    pub mutbl: Mutability,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TyKind {
    /// A variable length slice (i.e., `[T]`).
    Slice(Rc<Ty>),
    /// A fixed length array (i.e., `[T; n]`).
    Array(Rc<Ty>, Rc<ConstArg>),
    /// A raw pointer (i.e., `*const T` or `*mut T`).
    Ptr(MutTy),
    /// A reference (i.e., `&T` or `&mut T`).
    Ref(MutTy),
    /// A bare function (e.g., `fn(usize) -> bool`).
    //BareFn(Rc<BareFnTy>),
    /// An anonymous struct (foo: A, bar: B)
    Struct(Vec<Rc<Variant>>),
    /// A path to a type definition (`module::group::...::Type`), or an
    /// associated type (e.g., `<Vec<T> as Trait>::Type` or `<T>::Target`).
    ///
    /// Type parameters may be stored in each `PathSegment`.
    Path(QPath),
    // Pattern types (`pattern_type!(u32 is 1..)`)
    //Pat(&'hir Ty<'hir>, &'hir TyPat<'hir>),
    // `TyKind::Infer` means the type should be inferred instead of it having been
    // specified. This can appear anywhere in a type.
    //
    // This variant is not always used to represent inference types, sometimes
    // [`GenericArg::Infer`] is used instead.
    //Infer(Unambig),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ty {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: TyKind,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub ident: Ident,
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub ty: Rc<Ty>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VariantData {
    Struct { fields: Vec<FieldDef> },
    Tuple(Vec<FieldDef>, HirId, LocalDefId),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Variant {
    pub ident: Ident,
    pub hir_id: HirId,
    pub def_id: LocalDefId,
    pub data: VariantData,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ExprField {
    pub hir_id: HirId,
    pub ident: Ident,
    pub expr: Rc<Expr>,
    pub span: Span,
    pub is_shorthand: bool,
}

// For use in if statements only
#[derive(Clone, Debug)]
pub struct LetExpr {
    pub span: Span,
    pub ident: Rc<Ident>,
    //TODO pub pat: Rc<Pat>,
    pub ty: Option<Rc<Ty>>,
    pub init: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    // Allow anonymous constants from an inline `const` block
    //ConstBlock(ConstBlock),
    /// An array (e.g., `[a, b, c, d]`).
    //Array(&'hir [Expr<'hir>]),
    /// A function call.
    ///
    /// The first field resolves to the function itself (usually an `ExprKind::Path`),
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(Rc<Expr>, Vec<Rc<Expr>>),
    /// A method call (e.g., `x.foo::<'static, Bar, Baz>(a, b, c, d)`).
    ///
    /// The `PathSegment` represents the method name and its generic arguments
    /// (within the angle brackets).
    /// The `&Expr` is the expression that evaluates
    /// to the object on which the method is being called on (the receiver),
    /// and the `&[Expr]` is the rest of the arguments.
    /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
    /// `ExprKind::MethodCall(PathSegment { foo, [Bar, Baz] }, x, [a, b, c, d], span)`.
    /// The final `Span` represents the span of the function and arguments
    /// (e.g. `foo::<Bar, Baz>(a, b, c, d)` in `x.foo::<Bar, Baz>(a, b, c, d)`
    ///
    /// To resolve the called method to a `DefId`, call [`type_dependent_def_id`] with
    /// the `hir_id` of the `MethodCall` node itself.
    ///
    /// [`type_dependent_def_id`]: ../../rustc_middle/ty/struct.TypeckResults.html#method.type_dependent_def_id
    MethodCall(Rc<PathSegment>, Rc<Expr>, Vec<Expr>, Span),
    /// An use expression (e.g., `var.use`).
    //Use(Rc<Expr>, Span),
    /// A tuple (e.g., `(a, b, c, d)`).
    //Tup(&'hir [Expr<'hir>]),
    /// A binary operation (e.g., `a + b`, `a * b`).
    Binary(BinOp, Rc<Expr>, Rc<Expr>),
    /// A unary operation (e.g., `!x`, `*x`).
    Unary(UnOp, Rc<Expr>),
    /// A literal (e.g., `1`, `"foo"`).
    Lit(Rc<Lit>),
    /// A cast (e.g., `foo as f64`).
    Cast(Rc<Expr>, Rc<Ty>),
    /// A type ascription (e.g., `x: Foo`). See RFC 3307.
    Type(Rc<Expr>, Rc<Ty>),
    /// Wraps the expression in a terminating scope.
    /// This makes it semantically equivalent to `{ let _t = expr; _t }`.
    ///
    /// This construct only exists to tweak the drop order in AST lowering.
    /// An example of that is the desugaring of `for` loops.
    DropTemps(Rc<Expr>),
    /// A `let $pat = $expr` expression.
    ///
    /// These are not [`LetStmt`] and only occur as expressions.
    /// The `let Some(x) = foo()` in `if let Some(x) = foo()` is an example of `Let(..)`.
    Let(Rc<LetExpr>),
    /// An `if` block, with an optional else block.
    ///
    /// I.e., `if <expr> { <expr> } else { <expr> }`.
    ///
    /// The "then" expr is always `ExprKind::Block`. If present, the "else" expr is always
    /// `ExprKind::Block` (for `else`) or `ExprKind::If` (for `else if`).
    /// Note that using an `Expr` instead of a `Block` for the "then" part is intentional,
    /// as it simplifies the type coercion machinery.
    If(Rc<Expr>, Rc<Expr>, Option<Rc<Expr>>),
    /// A conditionless loop (can be exited with `break`, `continue`, or `return`).
    ///
    /// I.e., `'label: loop { <block> }`.
    ///
    /// The `Span` is the loop header (`for x in y`/`while let pat = expr`).
    //TODO Loop(Rc<Block>, Option<Label>, LoopSource, Span),
    /// A `match` block, with a source that indicates whether or not it is
    /// the result of a desugaring, and if so, which kind.
    //TODO Match(Rc<Expr>, &'hir [Arm<'hir>], MatchSource),
    /// A closure (e.g., `move |a, b, c| {a + b + c}`).
    ///
    /// The `Span` is the argument block `|...|`.
    ///
    /// This may also be a coroutine literal or an `async block` as indicated by the
    /// `Option<Movability>`.
    //TODO Closure(&'hir Closure<'hir>),
    /// A block (e.g., `'label: { ... }`).
    Block(Rc<Block>),

    /// An assignment (e.g., `a = foo()`).
    Assign(Rc<Expr>, Rc<Expr>, Span),
    /// An assignment with an operator.
    ///
    /// E.g., `a += 1`.
    //TODO AssignOp(AssignOp, Rc<Expr>, Rc<Expr>),
    /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct or tuple field.
    Field(Rc<Expr>, Ident),
    /// An indexing operation (`foo[2]`).
    /// Similar to [`ExprKind::MethodCall`], the final `Span` represents the span of the brackets
    /// and index.
    Index(Rc<Expr>, Rc<Expr>, Span),

    /// Path to a definition, possibly containing lifetime or type parameters.
    Path(QPath),

    /// A referencing operation (i.e., `&a` or `&mut a`).
    //TODO AddrOf(BorrowKind, Mutability, Rc<Expr>),
    /// A `break`, with an optional label to break.
    //TODO Break(Destination, Option<Rc<Expr>>),
    /// A `continue`, with an optional label.
    //TODO Continue(Destination),
    /// A `return`, with an optional value to be returned.
    //TODO Ret(Option<Rc<Expr>>),

    /// Field offset (`offset_of!`)
    //TODO OffsetOf(&'hir Ty<'hir>, &'hir [Ident]),

    /// A struct or struct-like variant literal expression.
    ///
    /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1, .. base}`,
    /// where `base` is the `Option<Expr>`.
    Struct(
        Option<Rc<QPath>>,
        Vec<ExprField>,
        //TODO StructTailExpr<'hir>,
    ),
    // An array literal constructed from one repeated element.
    //
    // E.g., `[1; 5]`. The first expression is the element
    // to be repeated; the second is the number of times to repeat it.
    //TODO Repeat(Rc<Expr>, &'hir ConstArg<'hir>),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
    pub span: Span,
}

// For use in if statements only
#[derive(Clone, Debug)]
pub struct LetStmt {
    pub ident: Rc<Ident>,
    //TODO pub pat: Rc<Pat>,
    pub ty: Option<Rc<Ty>>,
    pub init: Option<Rc<Expr>>,
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(LetStmt),
    // Expression without a trailing semi-colon
    Expr(Rc<Expr>),
    // Expression with a trailing semi-colon
    Semi(Rc<Expr>),
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub hir_id: HirId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    /// An expression at the end of the block
    /// without a semicoln, if any.
    pub expr: Option<Rc<Expr>>,
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Body {
    pub params: Vec<Param>,
    pub value: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub hir_id: HirId,
    pub ident: Rc<Ident>,
    //TODO upgrade to pat pub pat: Rc<Pat>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CallDecl {
    pub inputs: Vec<Ty>, // Make this a struct instead
    pub outputs: Vec<Ty>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct CallSig {
    /// Placeholder for when we have stuff like `inline`
    //pub header: (),
    pub decl: Rc<CallDecl>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Group {
    /// A group may be defined in multiple places
    pub spans: Vec<Span>,
    pub item_ids: Vec<ItemId>,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Fn {
        ident: Ident,
        sig: CallSig,
        body: BodyId,
    },
    Pipe {
        ident: Ident,
        call_sig: CallSig,
        stages: Vec<StageId>,
    },
    Group(Ident, Group),
}

#[derive(Clone, Debug)]
pub struct Item {
    pub owner_id: OwnerId,
    pub kind: ItemKind,
    pub span: Span,
}
