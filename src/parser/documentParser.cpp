//
// Created by WireWhiz on 10/28/2024.
//

#include <cassert>
#include <charconv>
#include <format>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <stack>

#include "documentParser.h"
#include "enums/matchv.h"
#include "enums/option.h"
#include "parser/documentContext.h"
#include "parserEnums.h"
#include "tree-sitter-branescript.h"
#include <tree_sitter/api.h>

#define Expect(node, _optional, _errorMessage)                                                                         \
    if(!_optional)                                                                                                     \
    {                                                                                                                  \
        errorMessage(node, _errorMessage);                                                                             \
        return None();                                                                                                 \
    }

namespace BraneScript
{
    template<class... Ts>
    struct overloads : Ts...
    {
        using Ts::operator()...;
    };

    template<class... V1, class... V2>
    bool tryCastVariant(std::variant<V1...> value, Option<std::variant<V2...>>& result)
    {
        return std::visit(overloads{[&](auto&& inner) -> bool
        {
            if constexpr(((std::is_convertible<decltype(inner), V2>()) || ...))
            {
                result = Some(inner);
                return false;
            }
            else
            {
                result = None();
                return false;
            }
        }},
                          std::move(value));
    }

    void foreachNodeChild(TSNode node, const std::function<void(TSNode)>& f)
    {
        uint32_t childCount = ts_node_child_count(node);
        for(uint32_t i = 0; i < childCount; ++i)
            f(ts_node_child(node, i));
    }

    void foreachNamedNodeChild(TSNode node, const std::function<void(TSNode)>& f)
    {
        uint32_t childCount = ts_node_named_child_count(node);
        for(uint32_t i = 0; i < childCount; ++i)
            f(ts_node_named_child(node, i));
    }

    TSRange nodeRange(TSNode node)
    {
        return {ts_node_start_point(node), ts_node_end_point(node), ts_node_start_byte(node), ts_node_end_byte(node)};
    }

    std::string_view nodeName(TSNode node)
    {
        return ts_language_symbol_name(tree_sitter_branescript(), ts_node_symbol(node));
    }

    std::string_view symbolToName(ts_symbol_identifiers symbolId)
    {
        return ts_language_symbol_name(tree_sitter_branescript(), symbolId);
    }

    template<ts_symbol_identifiers... Types>
    bool nodeIsSymbol(TSNode node)
    {
        auto nt = ts_node_symbol(node);
        return ((nt == Types) || ...);
    }

    template<ts_symbol_identifiers... Types>
    void advanceWhileSymbol(TSNode node, const std::function<void(TSNode)>& f)
    {
        bool nodeIsCorrectType = true;
        if(!ts_node_is_named(node))
            node = ts_node_next_named_sibling(node);
        while(!ts_node_is_null(node) && nodeIsSymbol<Types...>(node))
        {
            f(node);
            node = ts_node_next_named_sibling(node);
        }
    }

    struct ScopedScope
    {
        std::list<Node<TextContext>>& scopes;
        Node<TextContext> node;

        ScopedScope(std::list<Node<TextContext>>& scopes, Node<TextContext> node) : scopes(scopes), node(node) {}

        ScopedScope(const ScopedScope&) = delete;

        ScopedScope(ScopedScope&& o) noexcept : scopes(o.scopes)
        {
            node = o.node;
            o.node = nullptr;
        }

        ~ScopedScope()
        {
            if(!node)
                return;
            assert(scopes.back() == node);
            scopes.pop_back();
        }
    };

    struct ParserAPI
    {
        std::filesystem::path path;
        std::string_view sourceText;
        Node<TextSource> source;
        std::shared_ptr<BraneScriptParser> parser;
        std::vector<ParserMessage> messages;
        TSTree* tree;

        std::list<Node<TextContext>> scopes;

        Option<Node<TextContext>> currentScope()
        {
            if(scopes.empty())
                return None();
            return Some(scopes.back());
        }

        ScopedScope pushScope(Node<TextContext> node)
        {
            scopes.emplace_back(node);
            return ScopedScope(scopes, node);
        }

        void verboseMessage(TSNode ctx, std::string message)
        {
            messages.emplace_back(MessageType::Verbose, nodeRange(ctx), std::move(message));
        }

        void logMessage(TSNode ctx, std::string message)
        {
            messages.emplace_back(MessageType::Log, nodeRange(ctx), std::move(message));
        }

        void warningMessage(TSNode ctx, std::string message)
        {
            messages.emplace_back(MessageType::Warning, nodeRange(ctx), std::move(message));
        }

        void errorMessage(TSNode ctx, std::string message)
        {
            messages.emplace_back(MessageType::Error, nodeRange(ctx), std::move(message));
        }

        Option<TSNode> getField(TSNode node, ts_field_identifiers field)
        {
            auto result = ts_node_child_by_field_id(node, field);
            if(ts_node_is_null(result))
                return None();
            return Some(result);
        }

        bool expectSymbol(TSNode node, ts_symbol_identifiers type)
        {
            auto nodeType = ts_node_symbol(node);
            if(nodeType && type == nodeType)
                return true;

            std::string message =
                std::format("Was expecting \"{}\" but found \"{}\" \n", symbolToName(type), nodeText(node));
            errorMessage(node, message);
            return false;
        }

        bool expectField(TSNode node, const std::vector<ts_symbol_identifiers>& types)
        {
            auto nodeType = ts_node_symbol(node);
            if(nodeType)
            {
                for(auto t : types)
                {
                    if(t == nodeType)
                        return true;
                }
            }

            std::string message =
                std::format("Found \"{}\", but was expecting one of the following:\n", nodeText(node));
            for(auto t : types)
                message += std::format("{}\n", symbolToName(t));
            errorMessage(node, message);
            return false;
        }

        std::string_view nodeText(TSNode node)
        {
            auto start = ts_node_start_byte(node);
            auto end = ts_node_end_byte(node);
            return {sourceText.data() + start, end - start};
        }

        template<typename T>
        Node<T> makeNode(TSNode context)
        {
            static_assert(std::is_base_of<TextContext, T>().value);

            auto new_node = std::make_shared<T>();
            new_node->range = nodeRange(context);
            new_node->parent = currentScope();
            new_node->source = source;

            return new_node;
        }

        Option<TextContextNode> parse(TSNode node)
        {
            switch((ts_symbol_identifiers)ts_node_symbol(node))
            {
                case sym_source_file:
                    assert(false && "Cannot call parse on root node!");
                    return None();
                case sym_module:
                    return parseModule(node);
                case sym_pipeline:
                    return parsePipeline(node);
                case sym_function:
                    return parseFunction(node);
                case sym_pipelineStage:
                    return parsePipelineStage(node);
                case sym_callSig:
                    return parseCallSig(node);
                case sym_call:
                    return parseCall(node);
                case sym_anonStruct:
                    return parseAnonStruct(node);
                case sym_variableDefinition:
                    return parseVariableDefinition(node);
                case sym_block:
                    return parseBlock(node);
                case sym_assign:
                    return parseAssign(node);
                case sym_div:
                    return parseDiv(node);
                case sym_mul:
                    return parseMul(node);
                case sym_sub:
                    return parseSub(node);
                case sym_add:
                    return parseAdd(node);
                case sym_type:
                    return parseType(node);
                case sym_scopedIdentifier:
                    return parseScopedIdentifier(node);
                case sym_identifier:
                    return parseIdentifier(node);
                case sym_valueDef:
                    return parseValueDef(node);
                case sym_number:
                    return parseNumber(node);
                case sym_anonStructType:
                    return parseAnonStructType(node);
                default:
                    if(ts_node_has_error(node))
                    {
                        errorMessage(node, std::format("Unexpected \"{}\"", nodeText(node)));
                        return None();
                    }
                    std::cout << std::format("Parser not implemented for symbol {} could not parse \"{}\"",
                                             nodeName(node),
                                             nodeText(node))
                              << std::endl;
                    return None();
            }
        }

        Option<ExpressionContextNode> parseExpression(TSNode node)
        {
            auto parsed = parse(node);
            if(!parsed)
                return None();

            Option<ExpressionContextNode> out;
            tryCastVariant(parsed.value(), out);
            if(!out)
                errorMessage(node, std::format("Expected expression, but found: {}", nodeText(node)));
            return out;
        }

        Option<Node<ConstValueContext>> parseNumber(TSNode root)
        {
            std::string_view text = nodeText(root);
            float value;
            /*auto [ptr, res] = std::from_chars(text.data(), text.data() + text.size(), value);*/
            /*if(res == std::errc())*/
            /*{*/
            /*    errorMessage(root, std::format("Error parsing float: {}", std::make_error_code(res).message()));*/
            /*    return None();*/
            /*}*/
            value = std::stof((std::string)text);
            auto constNode = makeNode<ConstValueContext>(root);
            constNode->value = value;
            return Some(constNode);
        }

        Option<Node<Identifier>> parseIdentifier(TSNode root)
        {
            if(!expectSymbol(root, sym_identifier))
                return None();
            auto ident = makeNode<Identifier>(root);
            ident->text = nodeText(root);
            return Some(ident);
        }

        Option<Node<ScopedIdentifier>> parseScopedIdentifier(TSNode root)
        {
            if(!expectSymbol(root, sym_scopedIdentifier))
                return None();
            auto scopedId = makeNode<ScopedIdentifier>(root);
            auto scope = pushScope(scopedId);
            foreachNamedNodeChild(root,
                                  [&](TSNode ident)
            {
                auto id = parseIdentifier(ident);
                if(!id)
                    return;
                scopedId->scopes.emplace_back(id.value());
            });
            if(scopedId->scopes.empty())
            {
                errorMessage(root, "Expected Identifier!");
                return None();
            }
            return Some(scopedId);
        }

        Option<Node<TypeContext>> parseType(TSNode root)
        {
            if(!expectSymbol(root, sym_type))
                return None();

            auto type = makeNode<TypeContext>(root);
            auto scope = pushScope(type);

            bool queuedReference = false;
            foreachNodeChild(root,
                             [&](TSNode node)
            {
                if(!ts_node_is_named(node))
                {
                    auto text = nodeText(node);
                    if(text == "&")
                    {
                        if(queuedReference)
                            type->modifiers.push_back(TypeModifiers::ConstRef);
                        queuedReference = true;
                    }
                    else if(text == "mut")
                    {
                        if(queuedReference)
                            type->modifiers.push_back(TypeModifiers::MutRef);
                        queuedReference = false;
                    }
                    else if(text == "const")
                    {
                        if(queuedReference)
                            type->modifiers.push_back(TypeModifiers::ConstRef);
                        queuedReference = false;
                    }
                }
                else
                {
                    if(queuedReference)
                        type->modifiers.push_back(TypeModifiers::ConstRef);
                    auto scopedId = parseScopedIdentifier(node);
                    if(!scopedId)
                    {
                        errorMessage(node, "Expected Identifier");
                        return;
                    }
                    type->baseType = scopedId.value();
                }
            });

            if(!type->baseType)
            {
                errorMessage(root, "Missing identifier");
                return None();
            }

            return Some(type);
        }

        // Option<Node<TemplateArgsContext>> parseTemplateArguments(TSNode root);
        // Option<Node<TemplateArgContext>> parseTemplateArgument(TSNode root);
        Option<Node<BinaryOperatorContext>> parseAdd(TSNode root)
        {
            if(!expectSymbol(root, sym_add))
                return None();
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Add;

            auto tsLeft = getField(root, field_left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(tsLeft.value());
            if(!leftNode)
                return None();
            opr->left = leftNode.value();

            auto tsRight = getField(root, field_right);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(tsRight.value());
            if(!rightNode)
                return None();
            opr->right = rightNode.value();

            return Some(opr);
        }

        Option<Node<BinaryOperatorContext>> parseSub(TSNode root)
        {
            if(!expectSymbol(root, sym_sub))
                return None();
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Sub;

            auto tsLeft = getField(root, field_left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(tsLeft.value());
            if(!leftNode)
                return None();
            opr->left = leftNode.value();

            auto tsRight = getField(root, field_left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(tsRight.value());
            if(!rightNode)
                return None();
            opr->right = rightNode.value();

            return Some(opr);
        }

        Option<Node<BinaryOperatorContext>> parseMul(TSNode root)
        {
            if(!expectSymbol(root, sym_mul))
                return None();
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Mul;

            auto tsLeft = getField(root, field_left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(tsLeft.value());
            if(!leftNode)
                return None();
            opr->left = leftNode.value();

            auto tsRight = getField(root, field_left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(tsRight.value());
            if(!rightNode)
                return None();
            opr->right = rightNode.value();

            return Some(opr);
        }

        Option<Node<BinaryOperatorContext>> parseDiv(TSNode root)
        {
            if(!expectSymbol(root, sym_div))
                return None();
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Div;

            auto tsLeft = getField(root, field_left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(tsLeft.value());
            if(!leftNode)
                return None();
            opr->left = leftNode.value();

            auto tsRight = getField(root, field_left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(tsRight.value());
            if(!rightNode)
                return None();
            opr->right = rightNode.value();

            return Some(opr);
        }

        Option<Node<VariableDefinitionContext>> parseVariableDefinition(TSNode root)
        {
            if(!expectSymbol(root, sym_variableDefinition))
                return None();
            auto def = makeNode<VariableDefinitionContext>(root);
            auto scope = pushScope(def);


            auto defField = getField(root, field_def);
            Expect(root, defField, "Expected variable definition");
            auto valueDef = parseValueDef(defField.value());
            if(!valueDef)
                return None();
            def->definedValue = valueDef.value();

            if(auto parentScope = def->getParent<ScopeContext>())
                parentScope.value()->localVariables.push_back(def->definedValue);

            return Some(def);
        }

        Option<Node<AssignmentContext>> parseAssign(TSNode root)
        {
            if(!expectSymbol(root, sym_assign))
                return None();
            auto assign = makeNode<AssignmentContext>(root);
            auto scope = pushScope(assign);

            auto tsLeft = getField(root, field_left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(tsLeft.value());
            if(!leftNode)
                return None();
            assign->lValue = leftNode.value();

            auto tsRight = getField(root, field_right);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(tsRight.value());
            if(!rightNode)
                return None();
            assign->rValue = rightNode.value();

            return Some(assign);
        }

        Option<Node<ScopeContext>> parseBlock(TSNode root)
        {
            if(!expectSymbol(root, sym_block))
                return None();
            auto block = makeNode<ScopeContext>(root);
            auto scope = pushScope(block);

            auto expressions = getField(root, field_expressions);
            if(expressions)
            {
                foreachNamedNodeChild(root,
                                      [&](TSNode node)
                {
                    logMessage(node, std::format("parsing block child: {}", nodeText(node)));
                    auto expr = parseExpression(node);
                    if(!expr)
                    {
                        errorMessage(node, std::format("Was expecting expression but found: {}", nodeText(node)));
                        return;
                    }
                    block->expressions.push_back(expr.value());
                });
            }


            return Some(block);
        }

        Option<Node<MemberInitContext>> parseMemberInit(TSNode root)
        {
            if(!expectSymbol(root, sym_memberInit))
                return None();

            auto member = makeNode<MemberInitContext>(root);
            auto scope = pushScope(member);

            auto idField = getField(root, field_id);
            Expect(root, idField, "Expected identifier");
            auto id = parseIdentifier(idField.value());
            if(!id)
                return None();
            member->id = id.value();

            auto valueField = getField(root, field_value);
            Expect(root, valueField, "Expected expression");

            auto value = parseExpression(valueField.value());


            if(!value)
                return None();
            member->expression = value.value();

            return Some(member);
        }

        Option<Node<AnonStructContext>> parseAnonStruct(TSNode root)
        {
            if(!expectSymbol(root, sym_anonStruct))
                return None();

            auto list = makeNode<AnonStructContext>(root);
            auto scope = pushScope(list);

            foreachNamedNodeChild(root,
                                  [&](TSNode tsSinkDefNode)
            {
                auto memberInit = parseMemberInit(tsSinkDefNode);
                if(!memberInit)
                    return;
                list->members.push_back(memberInit.value());
            });

            return Some(list);
        }

        Option<Node<AnonStructTypeContext>> parseAnonStructType(TSNode root)
        {
            if(!expectSymbol(root, sym_anonStructType))
                return None();

            auto structDef = makeNode<AnonStructTypeContext>(root);
            auto scope = pushScope(structDef);

            foreachNamedNodeChild(root,
                                  [&](TSNode tsSinkDefNode)
            {
                auto memberDef = parseValueDef(tsSinkDefNode);
                if(!memberDef)
                    return;
                structDef->members.push_back(memberDef.value());
            });

            return Some(structDef);
        }

        Option<Node<ValueContext>> parseValueDef(TSNode root)
        {
            if(!expectSymbol(root, sym_valueDef))
                return None();

            auto def = makeNode<ValueContext>(root);
            auto scope = pushScope(def);

            def->isLValue = true;

            auto mutField = getField(root, field_mut);
            def->isMut = mutField.isSome();

            auto tsIdNode = getField(root, field_id);
            Expect(root, tsIdNode, "Expected identifier");
            def->label = parseIdentifier(tsIdNode.value());

            auto tsTypeNode = getField(root, field_type);
            Expect(root, tsTypeNode, "Expected type");
            auto typeNode = parseType(tsTypeNode.value());
            if(!typeNode)
                return None();
            def->type = Some(typeNode.value());

            return Some(def);
        }

        Option<Node<CallSigContext>> parseCallSig(TSNode root)
        {
            if(!expectSymbol(root, sym_callSig))
                return None();

            auto callSig = makeNode<CallSigContext>(root);
            auto scope = pushScope(callSig);

            auto inputField = getField(root, field_input);
            Expect(root, inputField, "Expected call signature input args");
            auto input = parseAnonStructType(inputField.value());
            if(!input)
                return None();
            callSig->input = input.value();

            auto outputType = getField(root, field_output);
            if(outputType)
            {
                auto output = parseAnonStructType(outputType.value());
                if(!output)
                    return None();
                callSig->output = output.value();
            }
            else
                callSig->output = makeNode<AnonStructTypeContext>(root);

            return Some(callSig);
        }

        Option<Node<CallContext>> parseCall(TSNode root)
        {
            if(!expectSymbol(root, sym_call))
                return None();
            auto call = makeNode<CallContext>(root);
            auto scope = pushScope(call);

            auto funcField = getField(root, field_func);
            Expect(root, funcField, "Expected callable expression");
            auto callable = parseExpression(funcField.value());
            if(!callable)
                return None();
            call->callable = callable.value();

            auto argsField = getField(root, field_args);
            Expect(root, argsField, "Expected call args");
            auto args = parseAnonStruct(argsField.value());
            if(!args)
                return None();
            call->args = args.value();


            return Some(call);
        }

        Option<Node<PipelineStageContext>> parsePipelineStage(TSNode root)
        {
            if(!expectSymbol(root, sym_pipelineStage))
                return None();
            auto stage = makeNode<PipelineStageContext>(root);
            auto scope = pushScope(stage);

            auto callSigField = getField(root, field_callSig);
            if(callSigField)
            {
                auto callSig = parseCallSig(callSigField.value());
                if(!callSig)
                    return None();
                stage->callSig = callSig.value();
            }

            auto bodyField = getField(root, field_body);
            Expect(root, bodyField, "Expected pipeline body");
            auto body = parseBlock(bodyField.value());
            if(!body)
                return None();
            stage->body = body.value();
            return Some(stage);
        }

        Option<Node<FunctionContext>> parseFunction(TSNode root)
        {
            warningMessage(root, "Functions not implemented yet!");
            return None();
        }

        Option<Node<PipelineContext>> parsePipeline(TSNode root)
        {
            if(!expectSymbol(root, sym_pipeline))
                return None();
            auto pipe = makeNode<PipelineContext>(root);
            auto scope = pushScope(pipe);

            auto tsIdNode = getField(root, field_id);
            Expect(root, tsIdNode, "Identifier was not found");
            auto idNode = parseIdentifier(tsIdNode.value());

            idNode.value()->parent = Some<std::weak_ptr<TextContext>>(pipe);
            pipe->identifier = idNode.value();

            auto callSigField = getField(root, field_callSig);
            Expect(root, callSigField, "Expected call signature");
            auto callSig = parseCallSig(callSigField.value());
            if(!callSig)
                return None();
            pipe->callSig = callSig.value();

            auto tsStagesNode = getField(root, field_stages);
            Expect(root, tsStagesNode, "Pipeline must have at least one stage");
            advanceWhileSymbol<sym_pipelineStage>(tsStagesNode.value(),
                                                  [&](TSNode tsStageNode)
            {
                auto stageNode = parsePipelineStage(tsStageNode);
                if(!stageNode)
                    return;
                pipe->stages.push_back(stageNode.value());
            });

            return Some(pipe);
        }

        Option<Node<ModuleContext>> parseModule(TSNode root)
        {
            if(!expectSymbol(root, sym_module))
                return None();
            auto mod = makeNode<ModuleContext>(root);
            auto scope = pushScope(mod);

            auto identifier = getField(root, field_id);
            if(!identifier)
                return None();

            auto idNode = parseIdentifier(identifier.value());
            if(!idNode)
                return None();
            idNode.value()->parent = Some<std::weak_ptr<TextContext>>(mod);
            mod->identifier = idNode.value();

            auto firstDef = getField(root, field_defs);
            if(!firstDef)
                return Some(mod);
            for(auto currentDef = firstDef.value(); !ts_node_is_null(currentDef) && ts_node_is_named(currentDef);
                currentDef = ts_node_next_named_sibling(currentDef))
            {
                Option<TextContextNode> def = parse(currentDef);
                if(!def)
                    continue;
                MATCHV(def.value(),
                       [&](Node<PipelineContext>& pipeline)
                { mod->pipelines.insert({pipeline->identifier->text, std::move(pipeline)}); },
                       [&](Node<FunctionContext>& function)
                { mod->functions.insert({function->identifier->text, std::move(function)}); },
                       [&](auto& none)
                {
                    errorMessage(currentDef,
                                 std::string("Grammer was correct, but context created was wrong: ") +
                                     typeid(none).name());
                });
            }
            return Some(mod);
        }

        ParserResult<DocumentContext> parseDocument()
        {
            tree = ts_parser_parse_string(parser->parser(), nullptr, sourceText.data(), sourceText.size());

            auto doc = std::make_shared<DocumentContext>();
            auto scope = pushScope(doc);

            doc->source = path;

            TSNode root = ts_tree_root_node(tree);

            foreachNodeChild(root,
                             [&](TSNode node)
            {
                Option<Node<ModuleContext>> newMod;
                newMod = parseModule(node);
                if(newMod)
                    doc->modules.insert({newMod.value()->identifier->text, newMod.value()});
            });

            return {doc, std::move(messages)};
        }
    };

    BraneScriptParser::BraneScriptParser()
    {
        _value = ts_parser_new();
        ts_parser_set_language(_value, tree_sitter_branescript());
    }

    BraneScriptParser::BraneScriptParser(BraneScriptParser&& other) noexcept
    {
        _value = other._value;
        other._value = nullptr;
    }

    BraneScriptParser::~BraneScriptParser()
    {
        if(_value)
            ts_parser_delete(_value);
    }

    TSParser* BraneScriptParser::parser() const { return _value; }

    ParsedDocument::ParsedDocument(std::filesystem::path path,
                                   std::string source,
                                   std::shared_ptr<BraneScriptParser> parser)
        : _path(std::move(path)), _source(std::move(source)), _parser(std::move(parser))
    {}

    std::string_view ParsedDocument::source() const { return _source; }

    void ParsedDocument::update(TSRange updateRange, std::string newText)
    {
        // TODO implement
        assert(false && "Update not implemented!");
    }

    ParserResult<DocumentContext> ParsedDocument::getDocumentContext()
    {
        if(_cachedResult)
            return _cachedResult.value();
        ParserAPI ctx{_path,
                      _source,
                      std::make_shared<TextSource>(TextSource{std::format("file://{}", _path.string())}),
                      _parser,
                      {},
                      {},
                      {}};
        _cachedResult = Some(ctx.parseDocument());
        return _cachedResult.value();
    }


} // namespace BraneScript
