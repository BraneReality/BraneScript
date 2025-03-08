use std::{collections::HashMap, sync::Arc};

use type_sitter::Range;

#[derive(PartialEq, Eq)]
pub struct TextSource {
    pub uri: String,
}

#[derive(PartialEq, Eq, Clone)]
pub struct TextContext {
    pub source: Arc<TextSource>,
    pub range: Range,
}

#[derive(PartialEq, Eq)]
pub struct Identifier {
    pub ctx: TextContext,
    pub text: String,
}

pub enum ScopeSegment {
    Id(Identifier),
}

pub struct ScopedIdentifier {
    pub ctx: TextContext,
    pub scopes: Vec<ScopeSegment>,
}

pub enum TypeModifiers {
    MutRef,
    ConstRef,
}

pub struct TypeContext {
    pub ctx: TextContext,
    pub base_type: ScopedIdentifier,
    pub modifiers: Vec<TypeModifiers>,
}

pub struct ValueContext {
    pub ctx: TextContext,
    pub label: Option<Identifier>,
    pub r#type: Option<TypeContext>,
}

pub struct VariableDefinitionContext {
    pub ctx: TextContext,
    pub defined_value: ValueContext,
}

pub struct BlockContext {
    pub ctx: TextContext,
    pub local_variables: Vec<ValueContext>,
    pub expressions: Vec<ExpressionContext>,
}

pub struct CallSigContext {
    pub ctx: TextContext,
    pub input: AnonStructTypeContext,
    pub output: AnonStructTypeContext,
}

pub struct PipelineStageContext {
    pub ctx: TextContext,
    pub call_sig: CallSigContext,
    pub body: BlockContext,
}

pub struct AssignmentContext {
    pub ctx: TextContext,
    pub dest: ExpressionContext,
    pub src: ExpressionContext,
}

pub enum ConstValue {
    Bool(bool),
    Char(u8),
    I64(i64),
    U64(i64),
    F64(f64),
    Str(String),
}

pub struct ConstValueContext {
    pub ctx: TextContext,
    pub value: ConstValue,
}

pub struct LabeledValueReferenceContext {
    pub ctx: TextContext,
    pub identifier: String,
}

pub struct MemberAccessContext {
    pub ctx: TextContext,
    pub base_expression: ExpressionContext,
    pub member: usize,
}

pub enum UnaryOperator {
    Deref,    // &var
    Def,      // *var
    Negate,   // -var
    LogicNot, // !var
    BitNot,   // ~var
}

pub struct UnaryOperatorContext {
    pub ctx: TextContext,
    pub op_type: UnaryOperator,
    pub arg: ExpressionContext,
}

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

pub struct BinaryOperatorContext {
    pub ctx: TextContext,
    pub op_type: BinaryOperator,
    pub left: ExpressionContext,
    pub right: ExpressionContext,
}

pub struct AnonStructTypeContext {
    pub ctx: TextContext,
    pub members: Vec<ValueContext>,
}

pub struct MemberInitContext {
    pub ctx: TextContext,
    pub id: Identifier,
    pub expression: ExpressionContext,
}

pub struct AnonStructContext {
    pub ctx: TextContext,
    pub members: Vec<MemberInitContext>,
}

pub struct CallContext {
    pub ctx: TextContext,
    pub callable: ExpressionContext,
    pub args: AnonStructContext,
}

pub struct StructContext {
    pub identifier: Identifier,
    pub membrs: Vec<ValueContext>,
    pub packed: bool,
}

pub struct FunctionContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub call_sig: CallSigContext,
    pub body: BlockContext,
}

pub struct TraitContext {}
pub struct ImplContext {}

pub struct PipelineContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub call_sig: CallSigContext,
    pub stages: Vec<PipelineStageContext>,
}

pub struct ModuleContext {
    pub ctx: TextContext,
    pub identifier: Identifier,
    pub structs: HashMap<String, StructContext>,
    pub fuctions: HashMap<String, FunctionContext>,
    pub pipelines: HashMap<String, PipelineContext>,
}

pub struct DocumentContext {
    pub ctx: TextContext,
    pub source: Arc<TextSource>,
    pub modules: HashMap<String, ModuleContext>,
}

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
}

pub enum ContextRefMut<'a> {
    Value(&'a mut ValueContext),
    ConstValue(&'a mut ConstValueContext),
    Identifier(&'a mut Identifier),
    ScopedIdentifier(&'a mut ScopedIdentifier),
    Type(&'a mut TypeContext),
    BinaryOperator(&'a mut BinaryOperatorContext),
    VariableDefinition(&'a mut VariableDefinitionContext),
    Assignment(&'a mut AssignmentContext),
    AnonStruct(&'a mut AnonStructContext),
    AnonStructType(&'a mut AnonStructTypeContext),
    Call(&'a mut CallContext),
    CallSig(&'a mut CallSigContext),
    Scope(&'a mut BlockContext),
    PipelineStage(&'a mut PipelineStageContext),
    MemberInit(&'a mut MemberInitContext),
    Function(&'a mut FunctionContext),
    Pipeline(&'a mut PipelineContext),
    Struct(&'a mut StructContext),
    Module(&'a mut ModuleContext),
    Document(&'a mut DocumentContext),
}
