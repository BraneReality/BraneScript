use branec_tokens::LiteralKind;
pub use branec_tokens::tree::Ident;

use branec_source::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub span: Span,
    /// The segments in the path: the things separated by `::`.
    pub segments: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
    //pub args: Option<Box<GenericArgs>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QSelf {
    pub ty: Box<Ty>,
    pub as_trait: Option<Path>,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Mutability {
    // N.B. Order is deliberate, so that Not < Mut
    Not,
    Mut,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MutTy {
    pub ty: Box<Ty>,
    pub mutability: Mutability,
}

/// Unary operator.
///
/// Note that `&data` is not an operator, it's an `AddrOf` expression.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnOpKind {
    /// The `*` operator for dereferencing
    Deref,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

pub type UnOp = (Span, UnOpKind);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

pub type BinOp = (Span, BinOpKind);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignOpKind {
    /// The `+=` operator (addition)
    AddAssign,
    /// The `-=` operator (subtraction)
    SubAssign,
    /// The `*=` operator (multiplication)
    MulAssign,
    /// The `/=` operator (division)
    DivAssign,
    /// The `%=` operator (modulus)
    RemAssign,
    /// The `^=` operator (bitwise xor)
    BitXorAssign,
    /// The `&=` operator (bitwise and)
    BitAndAssign,
    /// The `|=` operator (bitwise or)
    BitOrAssign,
    /// The `<<=` operator (shift left)
    ShlAssign,
    /// The `>>=` operator (shift right)
    ShrAssign,
}

pub type AssignOp = (Span, AssignOpKind);

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    /// An array (e.g, `[a, b, c, d]`).
    // TODO Array(Vec<Box<Expr>>),
    /// Allow anonymous constants from an inline `const` block.
    // TODO ConstBlock(AnonConst),

    /// A function call.
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(Box<Expr>, Vec<FieldExpr>),

    /// A method call (e.g., `x.foo::<Bar, Baz>(a, b, c)`).
    MethodCall(Box<MethodCall>),

    /// A binary operation (e.g., `a + b`, `a * b`).
    Binary(BinOp, Box<Expr>, Box<Expr>),

    /// A unary operation (e.g., `!x`, `*x`).
    Unary(UnOp, Box<Expr>),

    /// A literal (e.g., `1`, `"foo"`).
    Lit(LiteralKind),

    /// A cast (e.g., `foo as f64`).
    Cast(Box<Expr>, Box<Ty>),

    /// a let statement
    ///
    /// `Span` represents the whole `let pat = expr` statement.
    Let(Box<Ident>, Box<Expr>, Span),

    /// An `if` block, with an optional `else` block.
    ///
    /// `if expr { block } else { expr }`
    If(Box<Expr>, Box<Block>, Option<Box<Expr>>),

    /// A while loop.
    ///
    /// `'while expr { block }`
    While(Box<Expr>, Box<Block>),

    /// A `for` loop, with an optional label.
    ///
    /// `'label: for await? pat in iter { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    /* TODO ForLoop {
        pat: P<Pat>,
        iter: P<Expr>,
        body: P<Block>,
        label: Option<Label>,
        kind: ForLoopKind,
    },*/

    /// Conditionless loop (can be exited with `break`, `continue`, or `return`).
    ///
    /// `'label: loop { block }`
    // TODO Loop(P<Block>, Option<Label>, Span),

    /// A `match` block.
    // TODO Match(P<Expr>, ThinVec<Arm>, MatchKind),

    /// A closure (e.g., `move |a, b, c| a + b + c`).
    // TODO Closure(Box<Closure>),

    /// A block (`{ ... }`).
    ///
    Block(Box<Block>),

    /// A use expression (`x.use`). Span is of use keyword.
    Use(Box<Expr>, Span),

    /// An assignment (`a = foo()`).
    /// The `Span` argument is the span of the `=` token.
    Assign(Box<Expr>, Box<Expr>, Span),

    /// An assignment with an operator.
    ///
    /// E.g., `a += 1`.
    AssignOp(AssignOp, Box<Expr>, Box<Expr>),

    /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct field.
    Field(Box<Expr>, Ident),

    /// An indexing operation (e.g., `foo[2]`).
    /// The span represents the span of the `[2]`, including brackets.
    // TODO Index(Box<Expr>, Box<Expr>, Span),

    /// A range (e.g., `1..2`, `1..`, `..2`, `1..=2`, `..=2`; and `..` in destructuring assignment).
    // TODO Range(Option<Box<Expr>>, Option<Box<Expr>, RangeLimits),

    /// An underscore, used in destructuring assignment to ignore a value.
    // TODO Underscore,

    /// Variable reference, possibly containing `::` and/or type
    /// parameters (e.g., `foo::bar::<baz>`).
    ///
    /// Optionally "qualified" (e.g., `<Vec<T> as SomeTrait>::SomeType`).
    Path(Option<Box<QSelf>>, Path),

    /// A referencing operation (`&a`, `&mut a`, `&raw const a` or `&raw mut a`).
    AddrOf(BorrowKind, Mutability, Box<Expr>),

    /// A `break`, with an optional label to break, and an optional expression.
    // TODO Break(Option<Label>, Option<P<Expr>>),

    /// A `continue`, with an optional label.
    // TODO Continue(Option<Label>),

    /// A `return`, with an optional value to be returned.
    Ret(Option<Box<Expr>>),

    /// Output of the `asm!()` macro. Keeping this here as a "would be really cool"
    // TODO? InlineAsm(P<InlineAsm>),

    /// An `offset_of` expression (e.g., `builtin # offset_of(Struct, field)`).
    ///
    /// Usually not written directly in user code but
    /// indirectly via the macro `core::mem::offset_of!(...)`.
    // TODO OffsetOf(Box<Ty>, Ident),

    /// A macro invocation; pre-expansion.
    // TODO MacCall(P<MacCall>),

    /// A struct literal expression.
    ///
    /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1}`.
    Struct(Box<StructExpr>),

    /// An array literal constructed from one repeated element.
    ///
    /// E.g., `[1; 5]`. The expression is the element to be
    /// repeated; the constant is the number of times to repeat it.
    // TODO Repeat(P<Expr>, AnonConst),

    /// No-op: used solely so we can pretty-print faithfully.
    Paren(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldExpr {
    pub span: Span,
    pub ident: Ident,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructExpr {
    pub span: Span,
    pub path: Option<Path>,
    pub fields: Vec<FieldExpr>, // TODO pub rest: Box<Expr>
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodCall {
    /// The method name and its generic arguments, e.g. `foo::<Bar, Baz>`.
    pub seg: PathSegment,
    /// The receiver, e.g. `x`.
    pub receiver: Box<Expr>,
    /// The arguments, e.g. `a, b, c`.
    pub args: Vec<Box<Expr>>,
    /// The span of the function, without the dot and receiver e.g. `foo::<Bar,
    /// Baz>(a, b, c)`.
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Local {
    pub span: Span,
    pub ident: Ident,
    pub ty: Box<Ty>,
    pub kind: LocalKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalKind {
    /// Local declaration.
    /// Example: `let x;`
    //TODO Decl,

    /// Local declaration with an initializer.
    /// Example: `let x = y;`
    Init(Box<Expr>),
    // Local declaration with an initializer and an `else` clause.
    // `let Some(x) = y else { return };`
    //TODO InitElse(P<Expr>, P<Block>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    /// A local (let) binding.
    Let(Box<Local>),
    /// Expr without trailing semi-colon.
    Final(Box<Expr>),
    /// Expr with a trailing semi-colon.
    Expr(Box<Expr>),
    /// Just a trailing semi-colon.
    Empty,
    // Macro.
    //TODO MacCall(P<MacCallStmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub ident: Ident, // TODO upgrade to pattern
    pub ty: Box<Ty>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnRetTy {
    /// Returns type is not specified.
    ///
    /// Functions default to `()` and pipeline stages/closures
    /// default to inference.
    /// Span points to where return type would be inserted.
    Default(Span),
    /// Everything else.
    Ty(Box<Ty>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallSig {
    pub span: Span,
    pub inputs: Vec<Param>,
    pub output: FnRetTy,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fn {
    pub span: Span,
    pub ident: Ident,
    pub sig: CallSig,
    pub body: Option<Box<Block>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PipeStage {
    pub span: Span,
    pub ident: Option<Ident>,
    pub sig: Option<CallSig>,
    pub body: Box<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pipe {
    pub span: Span,
    pub ident: Ident,
    pub sig: CallSig,
    pub stages: Vec<PipeStage>, //TODO contract: Option<Box<CallContract>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub span: Span,
    pub ident: Ident,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    /// A use declaration item (`use`).
    ///
    /// E.g., `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`.
    //TODO Use(UseTree),

    /// A static item (`static`).
    ///
    /// E.g., `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`.
    // TODO Static(Box<StatcItem>),

    /// A constant item (`const`).
    ///
    /// E.g., `const FOO: i32 = 42;`.
    //TODO Const(Box<ConstItem>),

    /// A function declaration (`fn`).
    ///
    /// E.g., `fn foo(bar: usize) -> usize { .. }`.
    //TODO Fn(Box<Fn>),

    /// A pipeline declaration (`pipe`).
    ///
    /// E.g., `pipe foo(bar: usize) -> (res: usize) [ .. ]`.
    Pipe(Box<Pipe>),

    /// A type alias (`type`).
    ///
    /// E.g., `type Foo = Bar<u8>;`.
    //TODO TyAlias(Box<TyAlias>),

    /// An enum definition (`enum`).
    ///
    /// E.g., `enum Foo<A, B> { C<A>, D<B> }`.
    //TODO Enum(Ident, EnumDef, Generics),

    /// A struct definition (`struct`).
    ///
    /// E.g., `struct Foo<A> { x: A }`.
    //TODO Struct(Ident, VariantData, Generics),

    /// A union definition (`union`).
    ///
    /// E.g., `union Foo<A, B> { x: A, y: B }`.
    //TODO Union(Ident, VariantData, Generics),

    /// A trait declaration (`trait`).
    ///
    /// E.g., `trait Foo { .. }`, `trait Foo<T> { .. }` or `auto trait Foo {}`.
    //TODO Trait(Box<Trait>),

    /// Trait alias.
    ///
    /// E.g., `trait Foo = Bar + Quux;`.
    //TODO TraitAlias(Ident, Generics, GenericBounds),

    /// An implementation.
    ///
    /// E.g., `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`.
    //TODO Impl(Box<Impl>),

    /// A namespace declaration (`group`).
    ///
    /// E.g. `group foo { .. }`.
    Group(Box<Group>),
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub span: Span,
    pub items: Vec<Item>,
}
