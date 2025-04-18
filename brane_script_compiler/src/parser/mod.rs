use anyhow::anyhow;
use std::{collections::HashMap, str::FromStr, sync::Arc};
use type_sitter::{HasChild, Node};

use crate::{
    document_context::{
        AnonStructContext, AnonStructTypeContext, AssignmentContext, BinaryOperator,
        BinaryOperatorContext, BlockContext, CallContext, CallSigContext, ConstValue,
        ConstValueContext, DocumentContext, ExpressionContext, Identifier, MemberInitContext,
        ModuleContext, PipelineContext, PipelineStageContext, ScopeSegment, ScopedIdentifier,
        TextContext, TextSource, TypeContext, TypeModifiers, UnaryOperator, UnaryOperatorContext,
        ValueContext, VariableDefinitionContext,
    },
    MessageType, ToolchainMessage,
};

use nodes::*;

pub struct DocumentParserResult {
    pub document: Option<DocumentContext>,
    pub messages: Vec<ToolchainMessage>,
}

pub struct DocumentParser<'a> {
    source_text: &'a str,
    source: Arc<TextSource>,
    messages: Vec<ToolchainMessage>,

    local_variables: Vec<ValueContext>,
}

impl<'a, 'b> DocumentParser<'a> {
    fn record_err<E: Into<anyhow::Error>>(&mut self, err: E, ctx: TextContext) {
        self.messages.push(ToolchainMessage {
            message: format!(
                "[{}, {}] {}",
                ctx.range.start_point.row,
                ctx.range.start_point.column,
                err.into()
            ),
            r#type: MessageType::Error,
        });
    }

    fn record_log<T: Into<String>>(&mut self, text: T, ctx: TextContext) {
        self.messages.push(ToolchainMessage {
            message: format!(
                "[{}, {}] {}",
                ctx.range.start_point.row,
                ctx.range.start_point.column,
                text.into()
            ),
            r#type: MessageType::Log,
        });
    }

    fn node_text(&self, node: impl Node<'b>) -> String {
        let range = node.byte_range();
        self.source_text[range.start..range.end].into()
    }

    fn map_incorrect<'tree, T>(
        &self,
        result: type_sitter::NodeResult<'tree, T>,
    ) -> anyhow::Result<T> {
        result.map_err(|err| {
            anyhow!(format!(
                "Was expecting {} but found \"{}\" with content: {}",
                err.kind,
                err.node.kind(),
                self.node_text(err.node)
            ))
        })
    }

    fn node_ctx(&self, node: impl Node<'b>) -> TextContext {
        TextContext {
            source: self.source.clone(),
            range: node.range(),
        }
    }

    fn parse_identifier(&mut self, node: nodes::Identifier<'b>) -> anyhow::Result<Identifier> {
        Ok(Identifier {
            ctx: self.node_ctx(node),
            text: self.node_text(node),
        })
    }

    fn parse_scoped_identifier(
        &mut self,
        node: nodes::Scopedidentifier<'b>,
    ) -> anyhow::Result<ScopedIdentifier> {
        let ctx = self.node_ctx(node);
        let mut scopes = Vec::new();

        let mut cursor = node.walk();
        for scope in node.scopedidentifiersegments(&mut cursor) {
            let scope = self.map_incorrect(scope)?;
            let id = self.parse_identifier(self.map_incorrect(scope.id())?)?;
            match scope.generic() {
                Some(_generic) => todo!(),
                None => scopes.push(ScopeSegment::Id(id)),
            }
        }

        Ok(ScopedIdentifier { ctx, scopes })
    }

    fn parse_type(&mut self, node: nodes::Type<'b>) -> anyhow::Result<TypeContext> {
        let ctx = self.node_ctx(node);

        let base_type = self.parse_scoped_identifier(self.map_incorrect(node.id())?)?;

        let mut modifiers = Vec::new();

        let mut cursor = node.walk();
        for ref_op in node.refOps(&mut cursor) {
            let ref_op = self.map_incorrect(ref_op)?;
            if let Some(mut_op) = ref_op.r#mut() {
                self.map_incorrect(mut_op)?;
                modifiers.push(TypeModifiers::MutRef);
            } else if let Some(const_op) = ref_op.r#const() {
                self.map_incorrect(const_op)?;
                modifiers.push(TypeModifiers::ConstRef);
            } else {
                modifiers.push(TypeModifiers::ConstRef);
            }
        }

        Ok(TypeContext {
            ctx,
            base_type,
            modifiers,
        })
    }

    fn parse_value_def(&mut self, node: nodes::Valuedef<'b>) -> anyhow::Result<ValueContext> {
        let ctx = self.node_ctx(node);

        let label = Some(self.parse_identifier(self.map_incorrect(node.id())?)?);

        let r#type = if let Some(r#type) = node.r#type() {
            Some(self.parse_type(self.map_incorrect(r#type)?)?)
        } else {
            None
        };

        Ok(ValueContext { ctx, label, r#type })
    }

    fn parse_member_init(&mut self, node: Memberinit) -> anyhow::Result<MemberInitContext> {
        Ok(MemberInitContext {
            ctx: self.node_ctx(node),
            id: self.parse_identifier(self.map_incorrect(node.id())?)?,
            expression: self.parse_expression(self.map_incorrect(node.value())?)?,
        })
    }

    fn parse_anon_struct_type(
        &mut self,
        node: Anonstructtype<'b>,
    ) -> anyhow::Result<AnonStructTypeContext> {
        let ctx = self.node_ctx(node);

        let mut members = Vec::new();

        let mut cursor = node.walk();
        for member in node.valuedefs(&mut cursor) {
            members.push(self.parse_value_def(self.map_incorrect(member)?)?);
        }

        Ok(AnonStructTypeContext { ctx, members })
    }

    fn parse_anon_struct(&mut self, node: Anonstruct<'b>) -> anyhow::Result<AnonStructContext> {
        let ctx = self.node_ctx(node);

        let mut members = Vec::new();

        let mut cursor = node.walk();
        for member in node.memberinits(&mut cursor) {
            members.push(self.parse_member_init(self.map_incorrect(member)?)?);
        }

        Ok(AnonStructContext { ctx, members })
    }

    fn parse_binary_op(
        &mut self,
        node: nodes::BinaryOperator<'b>,
    ) -> anyhow::Result<BinaryOperatorContext> {
        let left = self.parse_expression(self.map_incorrect(node.left())?)?;
        let right = self.parse_expression(self.map_incorrect(node.right())?)?;
        use nodes::anon_unions::NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr::*;
        Ok(BinaryOperatorContext {
            ctx: self.node_ctx(node),
            left,
            right,
            op_type: match self.map_incorrect(node.operator())? {
                NotEq(_) => BinaryOperator::NotEqual,
                Mod(_) => BinaryOperator::Mod,
                And(_) => BinaryOperator::BitwiseAnd,
                AndAnd(_) => BinaryOperator::LogicAnd,
                Mul(_) => BinaryOperator::Mul,
                Add(_) => BinaryOperator::Add,
                Sub(_) => BinaryOperator::Sub,
                SubGt(_) => todo!(), // -> pointer member access
                Dot(_) => todo!(),   // . member access
                Div(_) => BinaryOperator::Div,
                Lt(_) => BinaryOperator::Less,
                LtLt(_) => BinaryOperator::BitshiftLeft,
                LtEq(_) => BinaryOperator::LessEqual,
                EqEq(_) => BinaryOperator::Equal,
                Gt(_) => BinaryOperator::Greater,
                GtEq(_) => BinaryOperator::GreaterEqual,
                GtGt(_) => BinaryOperator::BitshiftRight,
                BitXor(_) => BinaryOperator::BitwiseXOr,
                Or(_) => BinaryOperator::BitwiseOr,
                OrOr(_) => BinaryOperator::LogicOr,
            },
        })
    }

    fn parse_unary_op(
        &mut self,
        node: nodes::UnaryOperator<'b>,
    ) -> anyhow::Result<UnaryOperatorContext> {
        let expression = self.parse_expression(self.map_incorrect(node.expression())?)?;
        use nodes::anon_unions::Not_And_Mul_Sub_BitNot::*;
        Ok(UnaryOperatorContext {
            ctx: self.node_ctx(node),
            expression,
            op_type: match self.map_incorrect(node.operator())? {
                Not(_) => UnaryOperator::LogicNot,
                And(_) => UnaryOperator::Ref,
                Mul(_) => UnaryOperator::Deref,
                Sub(_) => UnaryOperator::Negate,
                BitNot(_) => UnaryOperator::BitNot,
            },
        })
    }

    fn parse_expression(
        &mut self,
        node: nodes::Expression<'b>,
    ) -> anyhow::Result<ExpressionContext> {
        use anon_unions::Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition::*;
        Ok(match self.map_incorrect(node.child())? {
            BinaryOperator(bin_op) => {
                ExpressionContext::BinaryOperator(Box::new(self.parse_binary_op(bin_op)?))
            }
            UnaryOperator(una_op) => {
                ExpressionContext::UnaryOperator(Box::new(self.parse_unary_op(una_op)?))
            }
            Anonstruct(anonstruct) => {
                ExpressionContext::AnonStruct(Box::new(self.parse_anon_struct(anonstruct)?))
            }
            Assign(assign) => ExpressionContext::Assignment(Box::new(AssignmentContext {
                ctx: self.node_ctx(assign),
                dest: self.parse_expression(self.map_incorrect(assign.left())?)?,
                src: self.parse_expression(self.map_incorrect(assign.right())?)?,
            })),
            Call(call) => ExpressionContext::Call(Box::new(CallContext {
                ctx: self.node_ctx(call),
                callable: self.parse_expression(self.map_incorrect(call.func())?)?,
                args: self.parse_anon_struct(self.map_incorrect(call.args())?)?,
            })),
            Expression(expression) => self.parse_expression(expression)?,
            Number(number) => ExpressionContext::ConstValue(Box::new(ConstValueContext {
                ctx: self.node_ctx(number),
                value: ConstValue::F64(f64::from_str(self.node_text(number).as_str())?),
            })),
            Scopedidentifier(scopedidentifier) => ExpressionContext::ScopedId(Box::new(
                self.parse_scoped_identifier(scopedidentifier)?,
            )),
            Variabledefinition(variabledefinition) => {
                ExpressionContext::VariableDefinition(Box::new(VariableDefinitionContext {
                    ctx: self.node_ctx(variabledefinition),
                    defined_value: self
                        .parse_value_def(self.map_incorrect(variabledefinition.def())?)?,
                }))
            }
        })
    }

    fn parse_block(&mut self, node: nodes::Block) -> anyhow::Result<BlockContext> {
        let ctx = self.node_ctx(node);

        let mut expressions = Vec::new();
        let mut cursor = node.walk();
        for expression in node.expressionss(&mut cursor) {
            match self.map_incorrect(expression) {
                Err(err) => self.record_err(err, self.node_ctx(expression)),
                Ok(expr) => match expr {
                    anon_unions::Semicolon_Expression::Semicolon(_) => {}
                    anon_unions::Semicolon_Expression::Expression(expression) => {
                        match self.parse_expression(expression) {
                            Ok(expr) => expressions.push(expr),
                            Err(err) => self.record_err(err, self.node_ctx(expression)),
                        }
                    }
                },
            }
        }

        Ok(BlockContext {
            ctx,
            local_variables: Vec::new(),
            expressions,
        })
    }

    fn parse_call_sig(&mut self, node: Callsig<'b>) -> anyhow::Result<CallSigContext> {
        let ctx = self.node_ctx(node);

        let input = self.parse_anon_struct_type(self.map_incorrect(node.input())?)?;

        let output = match node.output() {
            Some(output) => self.parse_anon_struct_type(self.map_incorrect(output)?)?,
            None => AnonStructTypeContext {
                ctx: self.node_ctx(node),
                members: Vec::new(),
            },
        };

        Ok(CallSigContext { ctx, input, output })
    }

    fn parse_pipeline_stage(
        &mut self,
        node: Pipelinestage<'b>,
    ) -> anyhow::Result<PipelineStageContext> {
        let ctx = self.node_ctx(node);

        let identifier = None; // add something like this in when we need it self.parse_identifier(self.map_incorrect(ctx.))

        let call_sig = match node.callSig() {
            Some(call_sig) => self.parse_call_sig(self.map_incorrect(call_sig)?)?,
            None => return Err(anyhow!("call sig missing!")),
        };

        let body = self.parse_block(self.map_incorrect(node.body())?)?;

        Ok(PipelineStageContext {
            ctx,
            identifier,
            call_sig,
            body,
        })
    }

    fn parse_pipeline(&mut self, node: Pipeline<'b>) -> anyhow::Result<PipelineContext> {
        let ctx = self.node_ctx(node);

        let identifier = self.parse_identifier(self.map_incorrect(node.id())?)?;
        let call_sig = self.parse_call_sig(self.map_incorrect(node.callSig())?)?;

        let mut stages = Vec::new();

        let mut cursor = node.walk();
        for stage in node.stagess(&mut cursor) {
            match self.map_incorrect(stage) {
                Ok(stage) => {
                    stages.push(self.parse_pipeline_stage(stage)?);
                }
                Err(err) => {
                    self.record_err(err, self.node_ctx(stage));
                }
            }
        }

        Ok(PipelineContext {
            ctx,
            identifier,
            call_sig,
            stages,
        })
    }

    fn parse_module(&mut self, node: Module<'b>) -> anyhow::Result<ModuleContext> {
        let ctx = self.node_ctx(node);

        let identifier = self.parse_identifier(self.map_incorrect(node.id())?)?;

        let mut cursor = node.walk();

        let mut pipelines = HashMap::new();
        let mut modules = HashMap::new();

        for def in node.defss(&mut cursor) {
            let ctx = self.node_ctx(def);
            match self.map_incorrect(def) {
                Ok(def) => match def {
                    anon_unions::Function_Module_Pipeline::Function(function) => todo!(),
                    anon_unions::Function_Module_Pipeline::Module(module) => {
                        match self.parse_module(module) {
                            Ok(module) => {
                                modules.insert(module.identifier.text.clone(), module);
                            }
                            Err(err) => self.record_err(err, ctx),
                        }
                    }
                    anon_unions::Function_Module_Pipeline::Pipeline(pipeline) => {
                        match self.parse_pipeline(pipeline) {
                            Ok(pipe) => {
                                pipelines.insert(pipe.identifier.text.clone(), pipe);
                            }
                            Err(err) => self.record_err(err, ctx),
                        }
                    }
                },
                Err(err) => self.record_err(err, ctx.clone()),
            }
        }

        return Ok(ModuleContext {
            ctx,
            identifier,
            structs: HashMap::new(),
            functions: HashMap::new(),
            pipelines,
        });
    }

    pub fn parse_document(
        text: &'a str,
        root: nodes::SourceFile<'b>,
        source: Arc<TextSource>,
    ) -> DocumentParserResult {
        let mut doc_parser = DocumentParser {
            source_text: text,
            source: source.clone(),
            messages: Vec::new(),
            local_variables: Vec::new(),
        };
        let ctx = doc_parser.node_ctx(root);

        let mut modules = HashMap::new();

        let mut cursor = root.walk();
        for module in root.modules(&mut cursor) {
            match doc_parser
                .map_incorrect(module)
                .and_then(|module| doc_parser.parse_module(module))
            {
                Ok(new_mod) => {
                    modules.insert(new_mod.identifier.text.clone(), new_mod);
                }
                Err(mod_err) => {
                    doc_parser.record_err(mod_err, doc_parser.node_ctx(module));
                }
            };
        }

        let mut has_error = false;
        for m in doc_parser.messages.iter() {
            if let MessageType::Error = &m.r#type {
                has_error = true;
                break;
            }
        }

        let document = if !has_error {
            Some(DocumentContext {
                ctx,
                modules,
                source,
            })
        } else {
            None
        };

        DocumentParserResult {
            document,
            messages: doc_parser.messages,
        }
    }
}
