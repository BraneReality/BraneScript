use std::{collections::HashMap, fmt::Display, sync::Arc};

use type_sitter::Range;

use crate::ToolchainMessage;

#[derive(PartialEq, Eq)]
pub struct TextSource {
    pub uri: String,
}

#[derive(PartialEq, Eq, Clone)]
pub struct TextContext {
    pub source: Arc<TextSource>,
    pub range: Range,
}

#[derive(PartialEq, Eq, Clone)]
pub struct Identifier {
    pub ctx: TextContext,
    pub text: String,
}

#[derive(Clone)]
pub enum ScopeSegment {
    Id(Identifier),
}

#[derive(Clone)]
pub struct ScopedIdentifier {
    pub ctx: TextContext,
    pub scopes: Vec<ScopeSegment>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.text)
    }
}

impl Display for ScopeSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeSegment::Id(identifier) => write!(f, "{}", identifier),
        }
    }
}

impl Display for ScopedIdentifier {
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
pub struct TypeContext {
    pub ctx: TextContext,
    pub base_type: ScopedIdentifier,
    pub modifiers: Vec<TypeModifiers>,
}

#[derive(Clone)]
pub struct ValueContext {
    pub ctx: TextContext,
    pub label: Option<Identifier>,
    pub r#type: Option<TypeContext>,
}

#[derive(Clone)]
pub struct VariableDefinitionContext {
    pub ctx: TextContext,
    pub defined_value: ValueContext,
}

#[derive(Clone)]
pub struct BlockContext {
    pub ctx: TextContext,
    pub local_variables: Vec<ValueContext>,
    pub expressions: Vec<ExpressionContext>,
}

#[derive(Clone)]
pub struct CallSigContext {
    pub ctx: TextContext,
    pub input: AnonStructTypeContext,
    pub output: AnonStructTypeContext,
}

#[derive(Clone)]
pub struct PipelineStageContext {
    pub ctx: TextContext,
    pub identifier: Option<Identifier>,
    pub call_sig: CallSigContext,
    pub body: BlockContext,
}

#[derive(Clone)]
pub struct AssignmentContext {
    pub ctx: TextContext,
    pub dest: ExpressionContext,
    pub src: ExpressionContext,
}

#[derive(Clone)]
pub enum ConstValue {
    Bool(bool),
    Char(u8),
    I64(i64),
    U64(i64),
    F64(f64),
    Str(String),
}

#[derive(Clone)]
pub struct ConstValueContext {
    pub ctx: TextContext,
    pub value: ConstValue,
}

#[derive(Clone)]
pub struct LabeledValueReferenceContext {
    pub ctx: TextContext,
    pub identifier: String,
}

#[derive(Clone)]
pub struct MemberAccessContext {
    pub ctx: TextContext,
    pub base_expression: ExpressionContext,
    pub member: usize,
}

#[derive(Clone)]
pub enum UnaryOperator {
    Ref,      // &var
    Deref,    // *var
    Negate,   // -var
    LogicNot, // !var
    BitNot,   // ~var
}

#[derive(Clone)]
pub struct UnaryOperatorContext {
    pub ctx: TextContext,
    pub op_type: UnaryOperator,
    pub expression: ExpressionContext,
}

#[derive(Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LogicAnd,
    LogicOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    BitshiftLeft,
    BitshiftRight,
}

#[derive(Clone)]
pub struct BinaryOperatorContext {
    pub ctx: TextContext,
    pub op_type: BinaryOperator,
    pub left: ExpressionContext,
    pub right: ExpressionContext,
}

#[derive(Clone)]
pub struct AnonStructTypeContext {
    pub ctx: TextContext,
    pub members: Vec<ValueContext>,
}

#[derive(Clone)]
pub struct MemberInitContext {
    pub ctx: TextContext,
    pub id: Identifier,
    pub expression: ExpressionContext,
}

#[derive(Clone)]
pub struct AnonStructContext {
    pub ctx: TextContext,
    pub members: Vec<MemberInitContext>,
}

#[derive(Clone)]
pub struct CallContext {
    pub ctx: TextContext,
    pub callable: ExpressionContext,
    pub args: AnonStructContext,
}

#[derive(Clone)]
pub struct StructContext {
    pub identifier: Identifier,
    pub members: Vec<ValueContext>,
    pub packed: bool,
}

#[derive(Clone)]
pub struct FunctionContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub call_sig: CallSigContext,
    pub body: BlockContext,
}

pub struct TraitContext {}
pub struct ImplContext {}

#[derive(Clone)]
pub struct PipelineContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub call_sig: CallSigContext,
    pub stages: Vec<PipelineStageContext>,
}

#[derive(Clone)]
pub struct ModuleContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub structs: HashMap<String, StructContext>,
    pub functions: HashMap<String, FunctionContext>,
    pub pipelines: HashMap<String, PipelineContext>,
}

#[derive(Clone)]
pub struct DocumentContext {
    pub ctx: TextContext,
    pub source: Arc<TextSource>,
    pub modules: HashMap<String, ModuleContext>,
}

pub struct ProjectContext {
    pub modules: HashMap<String, ModuleContext>,
}

impl ProjectContext {
    pub fn new(documents: &[DocumentContext]) -> Result<ProjectContext, Vec<ToolchainMessage>> {
        let mut modules = HashMap::<String, ModuleContext>::new();
        let mut messages = Vec::new();
        for doc in documents {
            for module in doc.modules.iter() {
                // TODO if there are any using directives, expand the types using them to full symbols here
                if let Some(combined) = modules.get_mut(module.0) {
                    for s in module.1.structs.iter() {
                        if let Some(_collision) = combined.structs.insert(s.0.clone(), s.1.clone())
                        {
                            messages.push(ToolchainMessage {
                                message: format!("Multiple structs with the name {} ", s.0),
                                r#type: crate::MessageType::Error,
                            })
                        }
                    }
                    for f in module.1.functions.iter() {
                        if let Some(_collision) =
                            combined.functions.insert(f.0.clone(), f.1.clone())
                        {
                            messages.push(ToolchainMessage {
                                message: format!("Multiple functions with the name {} ", f.0),
                                r#type: crate::MessageType::Error,
                            })
                        }
                    }
                    for p in module.1.pipelines.iter() {
                        if let Some(_collision) =
                            combined.pipelines.insert(p.0.clone(), p.1.clone())
                        {
                            messages.push(ToolchainMessage {
                                message: format!("Multiple pipelines with the name {} ", p.0),
                                r#type: crate::MessageType::Error,
                            })
                        }
                    }
                } else {
                    modules.insert(module.0.clone(), module.1.clone());
                }
            }
        }

        if !messages.is_empty() {
            return Err(messages);
        }

        Ok(ProjectContext { modules })
    }
}

#[derive(Clone)]
pub enum ExpressionContext {
    Scope(Box<BlockContext>),
    Assignment(Box<AssignmentContext>),
    VariableDefinition(Box<VariableDefinitionContext>),
    ConstValue(Box<ConstValueContext>),
    ScopedId(Box<ScopedIdentifier>),
    MemberAccess(Box<MemberAccessContext>),
    UnaryOperator(Box<UnaryOperatorContext>),
    BinaryOperator(Box<BinaryOperatorContext>),
    AnonStruct(Box<AnonStructContext>),
    Call(Box<CallContext>),
}

impl ExpressionContext {
    pub fn ctx(&self) -> TextContext {
        match self {
            ExpressionContext::Scope(c) => c.ctx.clone(),
            ExpressionContext::Assignment(c) => c.ctx.clone(),
            ExpressionContext::VariableDefinition(c) => c.ctx.clone(),
            ExpressionContext::ConstValue(c) => c.ctx.clone(),
            ExpressionContext::ScopedId(c) => c.ctx.clone(),
            ExpressionContext::MemberAccess(c) => c.ctx.clone(),
            ExpressionContext::UnaryOperator(c) => c.ctx.clone(),
            ExpressionContext::BinaryOperator(c) => c.ctx.clone(),
            ExpressionContext::AnonStruct(c) => c.ctx.clone(),
            ExpressionContext::Call(c) => c.ctx.clone(),
        }
    }
}

#[derive(Copy, Clone)]
pub enum ContextRef<'a> {
    Value(&'a ValueContext),
    ConstValue(&'a ConstValueContext),
    Identifier(&'a Identifier),
    ScopedIdentifier(&'a ScopedIdentifier),
    Type(&'a TypeContext),
    BinaryOperator(&'a BinaryOperatorContext),
    VariableDefinition(&'a VariableDefinitionContext),
    Assignment(&'a AssignmentContext),
    AnonStruct(&'a AnonStructContext),
    AnonStructType(&'a AnonStructTypeContext),
    Call(&'a CallContext),
    CallSig(&'a CallSigContext),
    Scope(&'a BlockContext),
    PipelineStage(&'a PipelineStageContext),
    MemberInit(&'a MemberInitContext),
    Function(&'a FunctionContext),
    Pipeline(&'a PipelineContext),
    Struct(&'a StructContext),
    Module(&'a ModuleContext),
    Document(&'a DocumentContext),
    ProjectContext(&'a ProjectContext),
}

impl<'a> ContextRef<'a> {
    pub fn label(&self) -> Option<&Identifier> {
        match self {
            ContextRef::Value(ctx) => ctx.label.as_ref(),
            ContextRef::ConstValue(_) => None,
            ContextRef::Identifier(_) => None,
            ContextRef::ScopedIdentifier(_) => None,
            ContextRef::Type(_) => None,
            ContextRef::BinaryOperator(_) => None,
            ContextRef::VariableDefinition(_) => None,
            ContextRef::Assignment(_) => None,
            ContextRef::AnonStruct(_) => None,
            ContextRef::AnonStructType(_) => None,
            ContextRef::Call(_) => None,
            ContextRef::CallSig(_) => None,
            ContextRef::Scope(_) => None,
            ContextRef::MemberInit(_) => None,
            ContextRef::PipelineStage(ctx) => ctx.identifier.as_ref(),
            ContextRef::Function(ctx) => Some(&ctx.identifier),
            ContextRef::Pipeline(ctx) => Some(&ctx.identifier),
            ContextRef::Struct(ctx) => Some(&ctx.identifier),
            ContextRef::Module(ctx) => Some(&ctx.identifier),
            ContextRef::Document(_) => None,
            ContextRef::ProjectContext(_) => None,
        }
    }
}
