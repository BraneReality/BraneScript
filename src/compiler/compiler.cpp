#include "compiler.h"

#include <cassert>
#include <format>
#include <utility>
#include "enums/matchv.h"
#include "enums/result.h"
#include <tree_sitter/api.h>

namespace BraneScript
{
    struct CompilerContext : TextContext
    {
        std::unordered_map<std::string, Node<ModuleContext>> modules;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override
        {
            if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
            {
                auto mod = modules.find((*id)->text);
                if(mod != modules.end())
                    return Some<TextContextNode>(mod->second);
            }
            return None();
        }
    };

    struct CompilerPass
    {
        std::vector<CompilerMessage> messages;
        bool recordedError = false;

        void recordMessage(std::string message, Node<TextContext> ctx, CompilerMessageType type)
        {
            messages.push_back(CompilerMessage{
                .type = type,
                .source = CompilerSource{.uri = ctx->source->uri, .range = Some(ctx->range)},
                .message = std::move(message),
            });
        }

        void recordLog(std::string message, Node<TextContext> ctx)
        {
            recordMessage(std::move(message), std::move(ctx), CompilerMessageType::Log);
        }

        void recordWarning(std::string message, Node<TextContext> ctx)
        {
            recordMessage(std::move(message), std::move(ctx), CompilerMessageType::Log);
        }

        void recordError(std::string message, Node<TextContext> ctx)
        {
            recordMessage(std::move(message), std::move(ctx), CompilerMessageType::Log);
        }
    };

    struct DocumentCombinerPass : CompilerPass
    {
        Node<CompilerContext> compilerContext;

        static bool identiferTaken(const std::string& id, const Node<ModuleContext>& mod)
        {
            return mod->structs.contains(id) || mod->functions.contains(id) || mod->pipelines.contains(id);
        }

        void mergeModule(std::string modName, std::vector<Node<ModuleContext>> moduleSources)
        {
            auto mod = std::make_shared<ModuleContext>();
            mod->parent = Some(compilerContext->weak_from_this());

            for(auto& def : moduleSources)
            {
                if(!mod->identifier)
                    mod->identifier = def->identifier;

                for(auto& s : def->structs)
                {
                    if(identiferTaken(s.first, mod))
                        recordError(std::format("May not define {}::{} multiple times", mod->identifier->text, s.first),
                                    s.second);
                    mod->structs[s.first] = s.second;
                    s.second->parent = Some(mod->weak_from_this());
                }

                for(auto& f : def->functions)
                {
                    if(identiferTaken(f.first, mod))
                        recordError(std::format("May not define {}::{} multiple times", mod->identifier->text, f.first),
                                    f.second);
                    mod->functions[f.first] = f.second;
                    f.second->parent = Some(mod->weak_from_this());
                }

                for(auto& p : def->pipelines)
                {
                    if(identiferTaken(p.first, mod))
                        recordError(std::format("May not define {}::{} multiple times", mod->identifier->text, p.first),
                                    p.second);
                    mod->pipelines[p.first] = p.second;
                    p.second->parent = Some(mod->weak_from_this());
                }
                // TODO combine impl statements
            }

            compilerContext->modules.insert({mod->identifier->text, mod});
        }

        Option<Node<CompilerContext>> run(const std::vector<std::shared_ptr<ParsedDocument>>& sources)
        {
            std::unordered_map<std::string, std::vector<Node<ModuleContext>>> moduleSources;
            compilerContext = std::make_shared<CompilerContext>();

            for(auto& document : sources)
            {
                auto ctx = document->getDocumentContext();
                for(auto& mod : ctx.document->modules)
                    moduleSources[mod.first].emplace_back(mod.second);
            }

            for(auto& sourceList : moduleSources)
                mergeModule(sourceList.first, std::move(sourceList.second));

            return Some(std::move(compilerContext));
        }
    };

    struct DocumentToIRPass : CompilerPass
    {
        Option<IRModule*> currentModule;
        Option<IRPipeline*> currentPipeline;
        Option<IRFunction*> currentFunction;

        CompilerSource sourceFor(Node<TextContext> ctx)
        {
            return CompilerSource{.uri = ctx->source->uri, .range = Some(ctx->range)};
        }

        Option<IDRef> compileStruct(Node<StructContext> ctx)
        {
            int structId = currentModule.value()->structs.size();
            currentModule.value()->structs.push_back(IRStruct{.id = Some(ctx->identifier->text), .members = {}});
            auto& s = currentModule.value()->structs[structId];

            for(auto& m : ctx->members)
            {
                auto memberType = resolveType(m.second->type.value());
                if(!memberType)
                {
                    messages.push_back(memberType.err());
                    return None();
                }

                s.members.push_back(IRStructMember{.id = Some(m.first), .type = memberType.ok()});
            }
            return Some<IDRef>(structId);
        }

        Result<IRType, CompilerMessage> resolveType(Node<AnonStructTypeContext> typeCtx)
        {
            std::vector<IRStructMember> members;
            for(auto& m : typeCtx->members)
            {
                IRStructMember newMember;
                if(m->label)
                    newMember.id = Some(m->label.value()->text);
                else
                    newMember.id = None();
                auto typeRes = resolveType(m->type.value());
                if(!typeRes)
                    return typeRes;
                newMember.type = typeRes.ok();
                members.push_back(newMember);
            }
            return resolveType(members);
        }

        Result<IRType, CompilerMessage> resolveType(const std::vector<IRStructMember>& members)
        {
            auto& structs = currentModule.value()->structs;
            for(size_t i = 0; i < structs.size(); ++i)
            {
                auto& s = structs[i];
                if(s.id.isSome())
                    continue;
                if(s.members.size() != members.size())
                    continue;
                bool membersMatch = true;
                for(size_t j = 0; j < members.size(); ++j)
                {
                    auto& a = s.members[j];
                    auto& b = members[j];
                    if(a.id != b.id || a.type != b.type)
                    {
                        membersMatch = false;
                        break;
                    }
                }

                if(membersMatch)
                    return Ok<IRType>((int32_t)i);
            }

            IRType newId = (int32_t)structs.size();
            structs.push_back(IRStruct{.id = None(), .members = std::move(members)});

            return Ok(newId);
        }

        Result<IRType, CompilerMessage> resolveType(Node<TypeContext> typeCtx)
        {
            for(auto mod : typeCtx->modifiers)
            {
                switch(mod)
                {
                    case TypeModifiers::MutRef:
                    case TypeModifiers::ConstRef:
                        return Ok(IRNativeType::I32); // Since this is a reference type, treat it as an int
                    default:
                        assert(false && "Unhandled type modifier! This is a compiler bug!");
                        return Err(CompilerMessage{.type = CompilerMessageType::Error,
                                                   .source = sourceFor(typeCtx),
                                                   .message = "Unhandled type modifier! This is a compiler bug!"});
                }
            }

            // See if this is a native type, otherwise it's a struct or typedef
            if(typeCtx->baseType->scopes.size() == 1)
            {
                if(auto id = std::get_if<Node<Identifier>>(&typeCtx->baseType->scopes[0]))
                {
                    static std::unordered_map<std::string_view, IRType> nativeTypes = {
                        {"u8", IRNativeType::U8},
                        {"i8", IRNativeType::I8},
                        {"u16", IRNativeType::U16},
                        {"i16", IRNativeType::I16},
                        {"u32", IRNativeType::U32},
                        {"i32", IRNativeType::I32},
                        {"f32", IRNativeType::F32},
                        {"u64", IRNativeType::U64},
                        {"i64", IRNativeType::I64},
                        {"f64", IRNativeType::F64},
                        {"I128", IRNativeType::I128},
                    };
                    auto nativeType = nativeTypes.find((*id)->text);
                    if(nativeType != nativeTypes.end())
                        return Ok(nativeType->second);
                }
            }

            // Find identifier
            auto typeNode = typeCtx->searchFor(typeCtx->baseType, 0);
            if(!typeNode)
            {
                return Err(CompilerMessage{.type = CompilerMessageType::Error,
                                           .source = sourceFor(typeCtx),
                                           .message = std::format("{} was not found", typeCtx->baseType->longId())});
            }

            return MATCHV(typeNode.value(),
                          [&](Node<StructContext>& ctx) -> Result<IRType, CompilerMessage>
            {
                std::vector<IRStructMember> members;
                std::string id = ctx->longId();
                auto& structs = currentModule.value()->structs;
                for(int i = 0; i < structs.size(); ++i)
                {
                    auto& s = structs[i];
                    if(s.id && s.id.value() == id)
                        return Ok<IDRef>(i);
                }

                auto newStruct = compileStruct(ctx);
                if(!newStruct)
                    return Err(
                        CompilerMessage{.type = CompilerMessageType::Error,
                                        .source = sourceFor(typeCtx),
                                        .message = std::format("{} is not a type", typeCtx->baseType->longId())});
                return Ok(newStruct.value());
            },
                          [&](auto& notSupported) -> Result<IRType, CompilerMessage>
            {
                return Err(CompilerMessage{.type = CompilerMessageType::Error,
                                           .source = sourceFor(typeCtx),
                                           .message = std::format("{} is not a type", typeCtx->baseType->longId())});
            });
        }

        std::unordered_map<Node<ValueContext>, IRValue> localVarLookup;

        void appendOp(IROperation op) { currentFunction.value()->operations.push_back(op); }

        IRType getValueType(IRValue value) { return currentFunction.value()->localVars[value.id]; }

        IRValue allocValue(IRType type)
        {
            IRValue id{.id = (uint32_t)currentFunction.value()->localVars.size()};
            currentFunction.value()->localVars.push_back(type);
            return id;
        }

        Option<IRValue> compileAnonStruct(Node<AnonStructContext> ctx)
        {
            std::vector<IRValue> memberValues;
            for(auto& input : ctx->members)
            {
                // TODO validate argument names
                auto argExpr = compileExpression(input->expression);
                if(!argExpr)
                {
                    recordError("expected value", ctx);
                    return None();
                }
                memberValues.push_back(argExpr.value());
            }

            std::vector<IRStructMember> memberDefs;
            memberDefs.reserve(ctx->members.size());
            for(size_t i = 0; i < ctx->members.size(); ++i)
            {
                memberDefs.push_back(
                    IRStructMember{.id = Some(ctx->members[i]->id->text), .type = getValueType(memberValues[i])});
            }

            Result<IRType, CompilerMessage> structType = resolveType(memberDefs);
            if(!structType)
            {
                recordError("could not resolve anon struct type", ctx);
                return None();
            }
            // Alloc a struct, but the runtime will give us rootPtr as an I32 pointer
            IRValue rootPtr = allocValue(structType.ok());
            for(size_t i = 0; i < memberValues.size(); ++i)
            {
                MemAcc memberAccess{};
                memberAccess.ptr = rootPtr;
                memberAccess.dest = allocValue(IRNativeType::I32);
                memberAccess.index = (uint8_t)i;
                appendOp(memberAccess);

                MovOp mov{};
                mov.dest = memberAccess.dest;
                mov.src = memberValues[i];
                appendOp(mov);
            }
            return Some(rootPtr);
        }

        Option<IRValue> compileExpression(ExpressionContextNode expression)
        {
            return MATCHV(expression,
                          [&](Node<ExpressionErrorContext>& ctx) -> Option<IRValue>
            {
                recordError(ctx->message, ctx);
                return None();
            },
                          [&](Node<ScopeContext>& ctx) -> Option<IRValue> { return compileScope(ctx); },
                          [&](Node<VariableDefinitionContext>& ctx) -> Option<IRValue>
            {
                // TODO make variable referenceable only after it's defined
                auto variable = localVarLookup.find(ctx->definedValue);
                if(variable == localVarLookup.end())
                {
                    recordError("(compiler error) could not find " + ctx->definedValue->label.value()->text, ctx);
                    return None();
                }
                return Some(variable->second);
            },
                          [&](Node<IfContext>& ctx) -> Option<IRValue>
            {
                recordError("if expressions not implemented", ctx);
                return None();
            },
                          [&](Node<WhileContext>& ctx) -> Option<IRValue>
            {
                recordError("while expressions not implemented", ctx);
                return None();
            },
                          [&](Node<ForContext>& ctx) -> Option<IRValue>
            {
                recordError("for expressions not implemented", ctx);
                return None();
            },
                          [&](Node<AssignmentContext>& ctx) -> Option<IRValue>
            {
                auto destRes = compileExpression(ctx->lValue);
                if(!destRes)
                    return None();
                auto destV = destRes.value();

                auto srcRes = compileExpression(ctx->rValue);
                if(!srcRes)
                    return None();
                auto srcV = srcRes.value();

                MovOp mov{};
                mov.dest = destV;
                mov.src = srcV;
                appendOp(mov);

                return destRes;
            },
                          [&](Node<ConstValueContext>& ctx) -> Option<IRValue>
            {
                recordError("Const values not implemented", ctx);
                return None();
            },
                          [&](Node<ScopedIdentifier>& ctx) -> Option<IRValue>
            {
                auto searchRes = ctx->searchFor(ctx, 0);
                if(!searchRes)
                {
                    recordError(std::format("{} not found", ctx->longId()), ctx);
                    return None();
                }

                auto valueCast = std::get_if<Node<ValueContext>>(&searchRes.value());
                if(!valueCast)
                {
                    recordError(std::format("{} is not a value",
                                            MATCHV(searchRes.value(), [](const auto& ctx) { return ctx->longId(); })),
                                ctx);
                    return None();
                }

                auto localVarRes = localVarLookup.find(*valueCast);
                if(localVarRes == localVarLookup.end())
                {
                    recordError(std::format("{} is not in scope", valueCast->get()->longId()), ctx);
                    return None();
                }
                // TODO re-evaluate this when we implement struct member access
                return Some(localVarRes->second);
            },
                          [&](Node<MemberAccessContext>& ctx) -> Option<IRValue>
            {
                recordError("member access expressions not implemented", ctx);
                return None();
            },
                          [&](Node<CreateReferenceContext>& ctx) -> Option<IRValue>
            {
                recordError("reference semantics not implemented", ctx);
                return None();
            },
                          [&](Node<DereferenceContext>& ctx) -> Option<IRValue>
            {
                recordError("reference semantics not implemented", ctx);
                return None();
            },
                          [&](Node<UnaryOperatorContext>& ctx) -> Option<IRValue>
            {
                recordError("unary operators not implemented", ctx);
                return None();
            },
                          [&](Node<BinaryOperatorContext>& ctx) -> Option<IRValue>
            {
                auto leftRes = compileExpression(ctx->left);
                if(!leftRes)
                    return None();
                auto leftV = leftRes.value();
                auto leftT = getValueType(leftV);

                auto rightRes = compileExpression(ctx->right);
                if(!rightRes)
                    return None();
                auto rightV = rightRes.value();
                auto rightT = getValueType(rightV);

                switch(ctx->opType)
                {
                    case BinaryOperator::Add:
                    {
                        auto op = AddOp{};
                        op.left = leftV;
                        op.right = rightV;
                        op.out = allocValue(leftT);
                        appendOp(op);
                        return Some(op.out);
                    }
                    case BinaryOperator::Sub:
                    {
                        auto op = SubOp{};
                        op.left = leftV;
                        op.right = rightV;
                        op.out = allocValue(leftT);
                        appendOp(op);
                        return Some(op.out);
                    }
                    break;
                    case BinaryOperator::Mul:
                    {
                        auto op = SubOp{};
                        op.left = leftV;
                        op.right = rightV;
                        op.out = allocValue(leftT);
                        appendOp(op);
                        return Some(op.out);
                    }
                    break;
                    case BinaryOperator::Div:
                    {
                        auto op = DivOp{};
                        op.left = leftV;
                        op.right = rightV;
                        op.out = allocValue(leftT);
                        appendOp(op);
                        return Some(op.out);
                    }
                    break;
                    case BinaryOperator::Mod:
                    case BinaryOperator::Equal:
                    case BinaryOperator::NotEqual:
                    case BinaryOperator::Greater:
                    case BinaryOperator::GreaterEqual:
                    case BinaryOperator::Less:
                    case BinaryOperator::LessEqual:
                    case BinaryOperator::LogicAnd:
                    case BinaryOperator::LogicOr:
                    case BinaryOperator::BitwiseAnd:
                    case BinaryOperator::BitwiseOr:
                    case BinaryOperator::BitwiseXOr:
                    case BinaryOperator::BitshiftLeft:
                    case BinaryOperator::BitshiftRight:
                        recordError("operator type not implemented", ctx);
                        return None();
                }

                return None();
            },
                          [&](Node<AnonStructContext>& ctx) -> Option<IRValue> { return compileAnonStruct(ctx); },
                          [&](Node<CallContext>& ctx) -> Option<IRValue>
            {
                auto idCast = std::get_if<Node<ScopedIdentifier>>(&ctx->callable);
                if(!idCast)
                {
                    recordError(std::format("{} is not an identifier",
                                            MATCHV(ctx->callable, [](const auto& ctx) { return ctx->longId(); })),
                                ctx);
                    return None();
                }

                if((*idCast)->longId() == "continue")
                {
                    NextStageOp ns{};
                    auto inputStruct = compileAnonStruct(ctx->args);
                    if(!inputStruct)
                    {
                        recordError("Invalid call sig", ctx->args);
                        return None();
                    }
                    ns.input = inputStruct.value();
                    appendOp(ns);
                    return None();
                }
                CallOp call{};
                auto searchRes = ctx->searchFor(*idCast, 0);
                if(!searchRes)
                {
                    recordError(std::format("{} not found", (*idCast)->longId()), ctx);
                    return None();
                }

                auto functionCast = std::get_if<Node<FunctionContext>>(&searchRes.value());
                if(!functionCast)
                {
                    recordError(std::format("{} is not a function",
                                            MATCHV(searchRes.value(), [](const auto& ctx) { return ctx->longId(); })),
                                ctx);
                    return None();
                }

                call.function = functionCast->get()->longId();
                auto retType = resolveType(functionCast->get()->callSig->output);
                if(!retType)
                    return None();
                call.output = allocValue(retType.ok());

                auto inputStruct = compileAnonStruct(ctx->args);
                if(!inputStruct)
                {
                    recordError("Invalid call sig", ctx->args);
                    return None();
                }
                call.input = inputStruct.value();

                // TODO account for unit returns
                appendOp(call);
                return Some(call.output);
            });
        }

        Option<IRValue> compileScope(Node<ScopeContext> scope)
        {
            for(auto& localVar : scope->localVariables)
            {
                IRValue localId{(uint32_t)currentFunction.value()->localVars.size()};
                auto typeRes = resolveType(localVar->type.value());
                if(!typeRes)
                    return None();
                currentFunction.value()->localVars.push_back(typeRes.ok());
                localVarLookup.insert({localVar, localId});
            }

            Option<IRValue> lastExpression;
            for(auto& expr : scope->expressions)
                lastExpression = compileExpression(expr);

            return lastExpression;
        }

        Option<IDRef>
        compileFunction(Option<std::string> identifier, Node<CallSigContext> callSig, Node<ScopeContext> body)
        {
            int32_t funcId = (int32_t)currentModule.value()->functions.size();
            currentModule.value()->functions.push_back(IRFunction{});
            auto& func = currentModule.value()->functions[funcId];
            currentFunction = Some(&func);

            func.id = identifier ? identifier.value() : std::format("-f{}", funcId);

            auto inType = resolveType(callSig->input);
            if(!inType)
            {
                messages.push_back(inType.err());
                return None();
            }
            else
                func.input = std::get<IDRef>(inType.ok());
            auto outType = resolveType(callSig->output);
            if(!outType)
            {
                messages.push_back(outType.err());
                return None();
            }
            else
                func.output = std::get<IDRef>(outType.ok());

            localVarLookup.clear();
            for(auto& arg : callSig->input->members)
            {
                IRValue localId{(uint32_t)currentFunction.value()->localVars.size()};
                auto typeRes = resolveType(arg->type.value());
                if(!typeRes)
                    return None();
                currentFunction.value()->localVars.push_back(typeRes.ok());
                localVarLookup.insert({arg, localId});
            }

            compileScope(body);

            currentFunction = None();
            return Some<IDRef>(funcId);
        }

        void compilePipeline(Node<PipelineContext> ctx)
        {
            int32_t pipeId = (int32_t)currentModule.value()->pipelines.size();
            currentModule.value()->pipelines.push_back(IRPipeline{.id = ctx->identifier->text});
            auto& pipe = currentModule.value()->pipelines[pipeId];
            currentPipeline = Some(&pipe);


            auto inType = resolveType(ctx->callSig->input);
            if(!inType)
            {
                messages.push_back(inType.err());
                return;
            }
            else
                pipe.input = std::get<IDRef>(inType.ok());
            auto outType = resolveType(ctx->callSig->output);
            if(!outType)
            {
                messages.push_back(outType.err());
                return;
            }
            else
                pipe.output = std::get<IDRef>(outType.ok());

            std::vector<IRNode<IRFunction>> stageFunctions;
            for(auto& stage : ctx->stages)
            {
                if(stage->callSig->output->members.size() > 0)
                {
                    recordError("Pipeline stages cannot return a value", stage->callSig);
                    return;
                }

                auto res = compileFunction(None(), stage->callSig, stage->body);
                if(!res)
                    return;
                pipe.stages.push_back(res.value());
            }

            currentPipeline = None();
        }

        Option<IRModule> compileModule(Node<ModuleContext> ctx)
        {
            auto mod = IRModule{.id = ctx->identifier->text};
            currentModule = Some(&mod);

            for(auto& structCtx : ctx->structs)
            {
                // Do explicit struct things
            }

            for(auto& funcCtx : ctx->functions)
            {
                // Do explicit function things
            }

            for(auto& pipeCtx : ctx->pipelines)
                compilePipeline(pipeCtx.second);


            auto out = Some(std::move(*currentModule.value()));
            currentModule = None();
            return out;
        }

        Option<std::vector<IRModule>> run(Node<CompilerContext> ctx)
        {
            std::vector<IRModule> modules;

            for(auto& mod : ctx->modules)
            {
                auto res = compileModule(mod.second);
                if(res)
                    modules.push_back(std::move(res.value()));
            }

            return Some(std::move(modules));
        }
    };

    Option<std::vector<IRModule>> Compiler::compile(const std::vector<std::shared_ptr<ParsedDocument>>& documents)
    {
        DocumentCombinerPass combinerPass;
        DocumentToIRPass toIRPass;

        auto compilerContext = combinerPass.run(documents);
        _messages.insert(_messages.begin(), combinerPass.messages.begin(), combinerPass.messages.end());
        if(!compilerContext)
            return None();
        auto modules = toIRPass.run(compilerContext.value());
        _messages.insert(_messages.begin(), toIRPass.messages.begin(), toIRPass.messages.end());
        return modules;
    }

    const std::vector<CompilerMessage>& Compiler::messages() const { return _messages; }


} // namespace BraneScript
