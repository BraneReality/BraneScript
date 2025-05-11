use brane_script_compiler_macro::arena_with_types;
use typed_arena::Arena;

use crate::source::Span;
use std::{collections::HashMap, fmt::Display, marker::PhantomData};

#[derive(PartialEq, Eq, Clone)]
pub struct Identifier<'hir> {
    pub source: Span,
    pub text: String,
    pub _lifetime: PhantomData<&'hir Identifier<'hir>>,
}

#[derive(Clone)]
pub enum GenericArg<'hir> {
    Path(&'hir Path<'hir>),
    Const(&'hir ConstValue),
}

impl<'hir> Display for GenericArg<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericArg::Path(path) => write!(f, "{}", path),
            GenericArg::Const(const_value) => write!(f, "{}", const_value),
        }
    }
}

#[derive(Clone)]
pub enum PathSeg<'hir> {
    Id(&'hir Identifier<'hir>),
    Generics(Vec<GenericArg<'hir>>),
    TraitImpl {
        r#type: &'hir Path<'hir>,
        r#trait: Path<'hir>,
    },
}

#[derive(Clone)]
pub struct Path<'hir> {
    pub source: &'hir Span,
    pub parent: Option<&'hir Path<'hir>>,
    pub scopes: Vec<PathSeg<'hir>>,
}

impl<'hir> Display for Identifier<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.text)
    }
}

impl<'hir> Display for PathSeg<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathSeg::Id(identifier) => write!(f, "{}", identifier),
            PathSeg::Generics(generics) => {
                write!(
                    f,
                    "<{}>",
                    generics
                        .iter()
                        .map(|g| format!("{}", g))
                        .reduce(|a, b| format!("{}, {}", a, b))
                        .unwrap_or_default()
                )
            }
            PathSeg::TraitImpl { r#type, r#trait } => write!(f, "<{} as {}>", r#type, r#trait),
        }
    }
}

impl<'hir> Display for Path<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.scopes.len() < 1 {
            return Ok(());
        }
        write!(f, "{}", self.scopes[0])?;
        for scope in self.scopes[1..self.scopes.len()].iter() {
            write!(f, "::{}", scope)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum TypeModifiers {
    MutRef,
    ConstRef,
}

#[derive(Clone)]
pub struct Type<'hir> {
    pub source: &'hir Span,
    pub base_type: &'hir Path<'hir>,
    pub modifiers: Vec<TypeModifiers>,
}

#[derive(Clone)]
pub struct Value<'hir> {
    pub source: &'hir Span,
    pub label: Option<Identifier<'hir>>,
    pub r#type: Option<Type<'hir>>,
}

#[derive(Clone)]
pub struct Let<'hir> {
    pub source: &'hir Span,
    pub defined_value: &'hir Value<'hir>,
}

#[derive(Clone)]
pub struct Block<'hir> {
    pub source: &'hir Span,
    pub local_variables: Vec<Value<'hir>>,
    pub expressions: Vec<&'hir Expr<'hir>>,
}

#[derive(Clone)]
pub struct CallSig<'hir> {
    pub source: Span,
    pub input: &'hir StructDef<'hir>,
    pub output: &'hir StructDef<'hir>,
}

#[derive(Clone)]
pub struct PipelineStage<'hir> {
    pub source: Span,
    pub identifier: Option<Identifier<'hir>>,
    pub call_sig: CallSig<'hir>,
    pub body: Block<'hir>,
}

#[derive(Clone)]
pub struct Assignment<'hir> {
    pub source: Span,
    pub dest: &'hir Expr<'hir>,
    pub src: &'hir Expr<'hir>,
}

#[derive(Clone)]
pub enum ConstValueData {
    Bool(bool),
    Char(char),
    I64(i64),
    U64(u64),
    F64(f64),
    Str(String),
}

#[derive(Clone)]
pub struct ConstValue {
    source: Span,
    data: ConstValueData,
}

impl<'hir> Display for ConstValue {
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

#[derive(Clone)]
pub struct Field<'hir> {
    pub source: Span,
    pub base_expression: &'hir Expr<'hir>,
    pub member: usize,
}

#[derive(Clone)]
pub struct FieldDef<'hir> {
    pub source: Span,
    pub id: &'hir Identifier<'hir>,
    pub expression: &'hir Expr<'hir>,
}

#[derive(Clone)]
pub struct Struct<'hir> {
    pub source: Span,
    /// If type is non, this is an anon struct
    pub r#type: Option<&'hir Path<'hir>>,
    pub members: Vec<&'hir FieldDef<'hir>>,
}

#[derive(Clone)]
pub struct Call<'hir> {
    pub source: Span,
    pub callable: &'hir Expr<'hir>,
    pub args: &'hir StructDef<'hir>,
}

#[derive(Clone)]
pub struct StructDef<'hir> {
    pub source: Span,
    pub identifier: Option<&'hir Identifier<'hir>>,
    pub members: Vec<&'hir Value<'hir>>,
    pub packed: bool,
}

#[derive(Clone)]
pub struct Function<'hir> {
    pub source: &'hir Span,
    pub identifier: &'hir Identifier<'hir>,
    pub call_sig: &'hir CallSig<'hir>,
    pub body: &'hir Block<'hir>,
}

pub enum TraitMember<'hir> {
    Fn(&'hir CallSig<'hir>),
    Type(&'hir Identifier<'hir>),
    Const(&'hir Identifier<'hir>, &'hir Path<'hir>),
}

pub struct TraitDef<'hir> {
    pub identifier: &'hir Identifier<'hir>,
    pub members: HashMap<String, &'hir TraitMember<'hir>>,
}

pub enum ImplMembers<'hir> {
    Fn(&'hir Function<'hir>),
    Type(&'hir Identifier<'hir>, &'hir Path<'hir>),
    Const(&'hir Identifier<'hir>, &'hir ConstValue),
}

pub struct Impl<'hir> {
    pub r#trait: Option<&'hir Path<'hir>>,
    pub r#type: &'hir Path<'hir>,
    pub members: HashMap<String, ImplMembers<'hir>>,
}

#[derive(Clone)]
pub struct Pipeline<'hir> {
    pub source: &'hir Span,
    pub identifier: &'hir Identifier<'hir>,
    pub call_sig: &'hir CallSig<'hir>,
    pub stages: Vec<&'hir PipelineStage<'hir>>,
}

pub struct Module<'hir> {
    pub source: Span,
    pub identifier: &'hir mut Identifier<'hir>,
    pub links: Vec<&'hir mut Path<'hir>>,
    pub structs: HashMap<String, &'hir mut Struct<'hir>>,
    pub functions: HashMap<String, &'hir mut Function<'hir>>,
    pub pipelines: HashMap<String, &'hir mut Pipeline<'hir>>,
}

pub struct Hir<'hir> {
    pub modules: HashMap<String, &'hir mut Module<'hir>>,
}

arena_with_types!(Hir, Identifier, Module);

/*
pub struct HirArena<'hir>(Arena<HirKind<'hir>>);

pub enum HirKind<'hir> {
    Identifier(Identifier),
    Module(Module<'hir>),
}

impl<'hir> HirArena<'hir> {
    pub fn alloc_identifier(&self, value: Identifier) -> &mut Identifier {
        if let HirKind::Identifier(value) = &mut *self.0.alloc(HirKind::Identifier(value)) {
            value
        } else {
            unreachable!("variant is known")
        }
    }

    pub fn alloc_module(&self, value: Module<'hir>) -> &mut Module<'hir> {
        if let HirKind::Module(value) = &mut *self.0.alloc(HirKind::Module(value)) {
            value
        } else {
            unreachable!("variant is known")
        }
    }
}
*/

/*
impl Hir {
    pub fn build(
        modules_builder: impl for<'this> FnOnce(
            &'this mut Rodeo,
        ) -> anyhow::Result<
            HashMap<String, &'this mut Module<'this>>,
        >,
    ) -> anyhow::Result<Hir> {
        Hir::try_new(Rodeo::new(), modules_builder)
    }
}

impl Default for Hir {
    fn default() -> Self {
        Hir::new(Rodeo::new(), |_| HashMap::new())
    }
}
*/

pub enum Expr<'hir> {
    Scope(&'hir mut Block<'hir>),
    Assignment(&'hir mut Assignment<'hir>),
    Let(&'hir mut Let<'hir>),
    Identifier(&'hir mut Identifier<'hir>),
    ConstValue(&'hir mut ConstValue),
    Path(&'hir mut Path<'hir>),
    Field(&'hir mut Field<'hir>),
    Struct(&'hir mut Struct<'hir>),
    Call(&'hir mut Call<'hir>),
}

impl<'hir> Expr<'hir> {
    pub fn source(&self) -> &Span {
        use Expr::*;
        match self {
            Assignment(c) => &c.source,
            Let(c) => &c.source,
            ConstValue(c) => &c.source,
            Path(c) => &c.source,
            Field(c) => &c.source,
            Struct(c) => &c.source,
            Call(c) => &c.source,
            Scope(block) => &block.source,
            Identifier(identifier) => &identifier.source,
        }
    }
}

pub enum Node<'hir> {
    Value(&'hir mut Value<'hir>),
    Type(&'hir mut Type<'hir>),
    CallSig(&'hir mut CallSig<'hir>),
    Expr(&'hir mut Expr<'hir>),
    PipelineStage(&'hir mut PipelineStage<'hir>),
    FieldDef(&'hir mut FieldDef<'hir>),
    Function(&'hir mut Function<'hir>),
    Pipeline(&'hir mut Pipeline<'hir>),
    StructDef(&'hir mut StructDef<'hir>),
    Module(&'hir mut Module<'hir>),
}

impl<'hir> Node<'hir> {
    pub fn label(&self) -> Option<&Identifier> {
        match self {
            Node::Value(source) => source.label.as_ref(),
            Node::Expr(_) => None,
            Node::Type(_) => None,
            Node::CallSig(_) => None,
            Node::FieldDef(_) => None,
            Node::PipelineStage(source) => source.identifier.as_ref(),
            Node::Function(source) => Some(&source.identifier),
            Node::Pipeline(source) => Some(&source.identifier),
            Node::StructDef(source) => source.identifier,
            Node::Module(source) => Some(&source.identifier),
        }
    }
}
