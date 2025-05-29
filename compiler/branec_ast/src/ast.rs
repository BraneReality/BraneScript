pub use branec_source::Span;
use branec_symbols::Symbol;
pub use branec_tokens::tree::Ident;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FloatTy {
    F16,
    F32,
    F64,
    F128,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LitIntType {
    Signed(IntTy),
    Unsigned(UintTy),
    Unsuffixed,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LitFloatType {
    Suffixed(FloatTy),
    Unsuffixed,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LitKind {
    Str(Symbol),
    Char(char),
    Int(i128, LitIntType),
    Float(Symbol, LitFloatType),
    Bool(bool),
}

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
    Lit(LitKind),

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

use std::fmt::{self, Display, Formatter};

impl Display for FloatTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for IntTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for UintTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for LitIntType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Signed(t) => write!(f, "{}", t),
            Self::Unsigned(t) => write!(f, "{}", t),
            Self::Unsuffixed => write!(f, "unsuffixed"),
        }
    }
}

impl Display for LitFloatType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Suffixed(t) => write!(f, "{}", t),
            Self::Unsuffixed => write!(f, "unsuffixed"),
        }
    }
}

impl Display for LitKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Int(i, ty) => write!(f, "{}_{}", i, ty),
            Self::Float(s, ty) => write!(f, "{}_{}", s, ty),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let segments = self
            .segments
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{}", segments)
    }
}

impl Display for PathSegment {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.ident.sym)
    }
}

impl Display for QSelf {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(trait_path) = &self.as_trait {
            write!(f, "<{} as {}>", self.ty, trait_path)
        } else {
            write!(f, "<{}>", self.ty)
        }
    }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Ptr(mt) => write!(f, "*{}", mt),
            Self::Ref(mt) => write!(f, "&{}", mt),
            Self::Struct(params) => {
                let fields = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {{ {} }}", fields)
            }
            Self::Path(p) => write!(f, "{}", p),
            Self::ImplicitSelf => write!(f, "self"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for BorrowKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Not => write!(f, ""),
            Self::Mut => write!(f, "mut "),
        }
    }
}

impl Display for MutTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.mutability, self.ty)
    }
}

impl Display for UnOpKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let sym = match self {
            Self::Deref => "*",
            Self::Not => "!",
            Self::Neg => "-",
        };
        write!(f, "{}", sym)
    }
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let sym = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::BitXor => "^",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Ne => "!=",
            Self::Ge => ">=",
            Self::Gt => ">",
        };
        write!(f, "{}", sym)
    }
}

impl Display for AssignOpKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let sym = match self {
            Self::AddAssign => "+=",
            Self::SubAssign => "-=",
            Self::MulAssign => "*=",
            Self::DivAssign => "/=",
            Self::RemAssign => "%=",
            Self::BitXorAssign => "^=",
            Self::BitAndAssign => "&=",
            Self::BitOrAssign => "|=",
            Self::ShlAssign => "<<=",
            Self::ShrAssign => ">>=",
        };
        write!(f, "{}", sym)
    }
}

impl std::fmt::Display for CallSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, input) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{input}")?;
        }
        write!(f, ")")?;
        if let FnRetTy::Ty(ty) = &self.output {
            write!(f, " -> {ty}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for PipeStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ident) = &self.ident {
            write!(f, "{} ", ident.sym)?;
        }
        if let Some(sig) = &self.sig {
            write!(f, "{} ", sig)?;
        }
        write!(f, "{{{}}}", self.body)
    }
}

impl std::fmt::Display for Pipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "pipe {}{}", self.ident.sym, self.sig)?;
        for stage in &self.stages {
            writeln!(f, "  {stage}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "group {}", self.ident.sym)?;
        for item in &self.items {
            writeln!(f, "  {item}")?;
        }
        Ok(())
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(func, args) => {
                let args = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "call {}({})", func, args)
            }
            Self::MethodCall(call) => write!(f, "{}", call),
            Self::Binary((_, op), lhs, rhs) => write!(f, "(l={} op={} r={})", lhs, op, rhs),
            Self::Unary((_, op), expr) => write!(f, "(op={} expr={})", op, expr),
            Self::Lit(lit) => write!(f, "{}", lit),
            Self::Cast(expr, ty) => write!(f, "({} as {})", expr, ty),
            Self::Let(ident, expr, _) => write!(f, "let {} = {}", ident.sym, expr),
            Self::If(cond, then, else_) => {
                if let Some(else_expr) = else_ {
                    write!(f, "if {} {} else {}", cond, then, else_expr)
                } else {
                    write!(f, "if {} {}", cond, then)
                }
            }
            Self::While(cond, body) => write!(f, "while {} {}", cond, body),
            Self::Block(block) => write!(f, "{}", block),
            Self::Use(expr, _) => write!(f, "{}.use", expr),
            Self::Assign(lhs, rhs, _) => write!(f, "{} = {}", lhs, rhs),
            Self::AssignOp((_, op), lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            Self::Field(expr, ident) => write!(f, "{}.{}", expr, ident.sym),
            Self::Path(Some(qself), path) => write!(f, "<{}>::{}", qself, path),
            Self::Path(None, path) => write!(f, "{}", path),
            Self::AddrOf(borrow, mutability, expr) => {
                write!(f, "&{}{}{}", borrow, mutability, expr)
            }
            Self::Ret(Some(expr)) => write!(f, "return {}", expr),
            Self::Ret(None) => write!(f, "return"),
            Self::Struct(se) => write!(f, "{}", se),
            Self::Paren(expr) => write!(f, "({})", expr),
        }
    }
}

impl Display for FieldExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident.sym, self.expr)
    }
}

impl Display for StructExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let path = self.path.as_ref().map_or("".to_string(), |p| p.to_string());
        let fields = self
            .fields
            .iter()
            .map(|f| f.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{} {{ {} }}", path, fields)
    }
}

impl Display for MethodCall {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}.{}({})", self.receiver, self.seg, args)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let stmts = self
            .stmts
            .iter()
            .enumerate()
            .map(|(i, s)| format!("expr{} = {}", i, s))
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{{ {} }}", stmts)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "let {}: {}", self.ident.sym, self.ty)?;
        match &self.kind {
            LocalKind::Init(expr) => {
                write!(f, " = {}", expr)?;
            }
        }
        Ok(())
    }
}

impl Display for LocalKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Init(expr) => write!(f, "= {}", expr),
        }
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Let(local) => write!(f, "{}", local),
            Self::Final(expr) => write!(f, "{}", expr),
            Self::Expr(expr) => write!(f, "{};", expr),
            Self::Empty => write!(f, ";"),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident.sym, self.ty)
    }
}

impl Display for FnRetTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Default(_) => write!(f, "-> ()"),
            Self::Ty(ty) => write!(f, "-> {}", ty),
        }
    }
}

impl Display for Fn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "fn {} {} ", self.ident.sym, self.sig)?;
        if let Some(body) = &self.body {
            write!(f, "{}", body)
        } else {
            write!(f, ";")
        }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in &self.items {
            writeln!(f, "{item}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Pipe(pipe) => write!(f, "{}", pipe),
            Item::Group(group) => write!(f, "{}", group),
        }
    }
}
