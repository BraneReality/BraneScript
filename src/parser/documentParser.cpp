//
// Created by WireWhiz on 10/28/2024.
//

#include "documentParser.h"

#include <cassert>
#include <charconv>
#include <expected>
#include <format>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <stack>
#include "parser/documentContext.h"
#include "tree_sitter_branescript.h"
#include <tree_sitter/api.h>


#define Expect(node, _optional, _errorMessage)                                                                         \
    if(!_optional)                                                                                                     \
    {                                                                                                                  \
        errorMessage(node, _errorMessage);                                                                             \
        return std::nullopt;                                                                                           \
    }

namespace BraneScript
{
    template<class... Ts>
    struct overloads : Ts...
    {
        using Ts::operator()...;
    };

    template<class... V1, class... V2>
    bool tryCastVariant(std::variant<V1...> value, std::optional<std::variant<V2...>>& result)
    {
        return std::visit(overloads{[&](auto&& inner) -> bool {
            if constexpr(((std::is_same_v<decltype(inner), V2>) || ...))
            {
                result = inner;
                return false;
            }
            result = std::nullopt;
            return false;
        }},
                          std::move(value));
    }

    enum class TSNodeType : uint16_t
    {
        Unknown = 0,
        Number,
        Identifier,
        ScopedIdentifier,
        Type,
        TemplateArgument,
        TemplateArguments,
        Add,
        Sub,
        Mul,
        Div,
        Assign,
        Block,
        VariableDefinition,
        SinkDef,
        SinkList,
        SourceDef,
        SourceList,
        Call,
        PipelineStage,
        AsyncOperation,
        Function,
        Pipeline,
        Module,
        SourceFile
    };

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

    class TSSymbolLookupTable
    {
      private:
        std::vector<TSNodeType> symbolToNodeType;
        std::vector<TSSymbol> nodeTypeToSymbol;

        void add_entry(const TSLanguage* lang, std::string_view ident, TSNodeType nodeType)
        {
            auto symbol = ts_language_symbol_for_name(lang, ident.data(), ident.size(), true);
            if(symbol >= symbolToNodeType.size())
                symbolToNodeType.resize(symbol + 1, (TSNodeType)UINT16_MAX);
            if((uint16_t)nodeType >= nodeTypeToSymbol.size())
                nodeTypeToSymbol.resize((uint16_t)nodeType + 1, (TSSymbol)UINT16_MAX);
            symbolToNodeType[symbol] = nodeType;
            nodeTypeToSymbol[(uint16_t)nodeType] = symbol;
        }

      public:
        TSSymbolLookupTable(const TSLanguage* lang)
        {
            add_entry(lang, "number", TSNodeType::Number);
            add_entry(lang, "module", TSNodeType::Module);
            add_entry(lang, "identifier", TSNodeType::Identifier);
            add_entry(lang, "scopedIdentifier", TSNodeType::ScopedIdentifier);
            add_entry(lang, "type", TSNodeType::Type);
            add_entry(lang, "templateArgument", TSNodeType::TemplateArgument);
            add_entry(lang, "templateArguments", TSNodeType::TemplateArguments);
            add_entry(lang, "add", TSNodeType::Add);
            add_entry(lang, "sub", TSNodeType::Sub);
            add_entry(lang, "mul", TSNodeType::Mul);
            add_entry(lang, "div", TSNodeType::Div);
            add_entry(lang, "assign", TSNodeType::Assign);
            add_entry(lang, "block", TSNodeType::Block);
            add_entry(lang, "variableDefinition", TSNodeType::VariableDefinition);
            add_entry(lang, "sinkDef", TSNodeType::SinkDef);
            add_entry(lang, "sinkList", TSNodeType::SinkList);
            add_entry(lang, "sourceDef", TSNodeType::SourceDef);
            add_entry(lang, "sourceList", TSNodeType::SourceList);
            add_entry(lang, "call", TSNodeType::Call);
            add_entry(lang, "pipelineStage", TSNodeType::PipelineStage);
            add_entry(lang, "asyncOperation", TSNodeType::AsyncOperation);
            add_entry(lang, "function", TSNodeType::Function);
            add_entry(lang, "pipeline", TSNodeType::Pipeline);
            add_entry(lang, "sourceFile", TSNodeType::SourceFile);
        }

        std::optional<TSNodeType> tryToNodeType(TSSymbol symbol) const
        {
            if(symbol > symbolToNodeType.size())
                return std::nullopt;
            auto nodeType = symbolToNodeType[symbol];
            return (uint16_t)nodeType == UINT16_MAX ? std::nullopt : std::make_optional(nodeType);
        }

        std::optional<TSSymbol> tryToSymbol(TSNodeType nodeType) const
        {
            if((uint16_t)nodeType > nodeTypeToSymbol.size())
                return std::nullopt;
            auto symbol = nodeTypeToSymbol[(uint16_t)nodeType];
            return symbol == UINT16_MAX ? std::nullopt : std::make_optional(symbol);
        }
    };

    enum class TSFieldName : uint16_t
    {
        Id = 0,
        Child,
        Type,
        Value,
        Mut,
        Defs,
        Left,
        Right,
        Sources,
        Sinks,
        Stages,
        TemplateArgs,
    };

    class TSFieldLookupTable
    {
      private:
        std::vector<TSFieldId> _fieldIds;

        void add_entry(const TSLanguage* lang, std::string_view textName, TSFieldName fieldName)
        {
            auto fID = ts_language_field_id_for_name(lang, textName.data(), textName.size());

            if((uint16_t)fieldName >= _fieldIds.size())
                _fieldIds.resize((uint16_t)fieldName + 1, (TSFieldId)0);
            _fieldIds[(uint16_t)fieldName] = fID;
        }

      public:
        TSFieldLookupTable(const TSLanguage* lang)
        {
            add_entry(lang, "id", TSFieldName::Id);
            add_entry(lang, "child", TSFieldName::Child);
            add_entry(lang, "type", TSFieldName::Type);
            add_entry(lang, "value", TSFieldName::Value);
            add_entry(lang, "mut", TSFieldName::Mut);
            add_entry(lang, "defs", TSFieldName::Defs);
            add_entry(lang, "left", TSFieldName::Left);
            add_entry(lang, "right", TSFieldName::Right);
            add_entry(lang, "sinks", TSFieldName::Sinks);
            add_entry(lang, "sources", TSFieldName::Sources);
            add_entry(lang, "stages", TSFieldName::Stages);
            add_entry(lang, "templateArgs", TSFieldName::TemplateArgs);
        }

        TSFieldId get(TSFieldName name) const
        {
            assert((uint16_t)name < _fieldIds.size() && "Make sure all fields are defined in the constructor");
            return _fieldIds[(uint16_t)name];
        }
    };

    const TSSymbolLookupTable symbolLookupTable = TSSymbolLookupTable(tree_sitter_branescript());
    const TSFieldLookupTable fieldLookupTable = TSFieldLookupTable(tree_sitter_branescript());

    std::string_view typeToName(TSNodeType type)
    {
        auto symbol = symbolLookupTable.tryToSymbol(type);
        if(!symbol)
            return "Unknown";
        return ts_language_symbol_name(tree_sitter_branescript(), symbol.value());
    }

    TSNodeType nodeType(TSNode node)
    {
        auto nodeType = symbolLookupTable.tryToNodeType(ts_node_symbol(node));
        if(!nodeType)
            return TSNodeType::Unknown;
        return *nodeType;
    }

    template<TSNodeType... Types>
    bool nodeIsType(TSNode node)
    {
        auto nt = nodeType(node);
        return ((nt == Types) || ...);
    }

    template<TSNodeType... Types>
    void advanceWhileType(TSNode node, const std::function<void(TSNode)>& f)
    {
        bool nodeIsCorrectType = true;
        if(!ts_node_is_named(node))
            node = ts_node_next_named_sibling(node);
        while(!ts_node_is_null(node) && nodeIsType<Types...>(node))
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
        std::string_view source;
        std::shared_ptr<BraneScriptParser> parser;
        std::vector<ParserMessage> messages;
        TSTree* tree;

        std::list<Node<TextContext>> scopes;

        std::optional<Node<TextContext>> currentScope()
        {
            if(scopes.empty())
                return std::nullopt;
            return scopes.back();
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

        std::optional<TSNode> getField(TSNode node, TSFieldName field)
        {
            auto result = ts_node_child_by_field_id(node, fieldLookupTable.get(field));
            if(ts_node_is_null(result))
                return std::nullopt;
            return result;
        }

        bool expectNode(TSNode node, TSNodeType type)
        {
            auto nodeType = symbolLookupTable.tryToNodeType(ts_node_symbol(node));
            if(nodeType && type == nodeType)
                return true;

            std::string message =
                std::format("Was expecting \"{}\" but found \"{}\" \n", typeToName(type), nodeText(node));
            errorMessage(node, message);
            return false;
        }

        bool expectField(TSNode node, const std::vector<TSNodeType>& types)
        {
            auto nodeType = symbolLookupTable.tryToNodeType(ts_node_symbol(node));
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
                message += std::format("{}\n", typeToName(t));
            errorMessage(node, message);
            return false;
        }

        std::string_view nodeText(TSNode node)
        {
            auto start = ts_node_start_byte(node);
            auto end = ts_node_end_byte(node);
            return {source.data() + start, end - start};
        }

        template<typename T>
        Node<T> makeNode(TSNode context)
        {
            static_assert(std::is_base_of<TextContext, T>().value);

            auto new_node = std::make_shared<T>();
            new_node->range = nodeRange(context);
            new_node->parent = currentScope();

            return new_node;
        }

        std::optional<TextContextNode> parse(TSNode node)
        {
            switch(nodeType(node))
            {
                case TSNodeType::SourceFile:
                    assert(false && "Cannot call parse on root node!");
                    return std::nullopt;
                case TSNodeType::Module:
                    return parseModule(node);
                case TSNodeType::Pipeline:
                    return parsePipeline(node);
                case TSNodeType::Function:
                    return parseFunction(node);
                case TSNodeType::PipelineStage:
                    return parsePipelineStage(node);
                case TSNodeType::AsyncOperation:
                    return parseAsyncOperation(node);
                case TSNodeType::Call:
                    return parseCall(node);
                case TSNodeType::SourceList:
                    return parseSourceList(node);
                case TSNodeType::SinkList:
                    return parseSinkList(node);
                case TSNodeType::VariableDefinition:
                    return parseVariableDefinition(node);
                case TSNodeType::Block:
                    return parseBlock(node);
                case TSNodeType::Assign:
                    return parseAssign(node);
                case TSNodeType::Div:
                    return parseDiv(node);
                case TSNodeType::Mul:
                    return parseMul(node);
                case TSNodeType::Sub:
                    return parseSub(node);
                case TSNodeType::Add:
                    return parseAdd(node);
                /*case TSNodeType::TemplateArguments:
                    return parseTemplateArguments(node);
                case TSNodeType::TemplateArgument:
                    return parseTemplateArgument(node);*/
                case TSNodeType::Type:
                    return parseType(node);
                case TSNodeType::ScopedIdentifier:
                    return parseScopedIdentifier(node);
                case TSNodeType::Identifier:
                    return parseIdentifier(node);
                case TSNodeType::Number:
                    return parseNumber(node);
                case TSNodeType::SourceDef:
                    return parseSourceDef(node);
                case TSNodeType::SinkDef:
                    return parseSinkDef(node);
                case TSNodeType::Unknown:
                default:
                    if(ts_node_has_error(node))
                    {
                        errorMessage(node, std::format("Unexpected \"{}\"", nodeText(node)));
                        return std::nullopt;
                    }
                    std::cout << "Unknown TSNode type for: " << nodeText(node) << std::endl;
                    assert(false && "TSNodeType unhandled!");
                    return std::nullopt;
            }
        }

        std::optional<ExpressionContextNode> parseExpression(TSNode node)
        {
            auto parsed = parse(node);
            if(!parsed)
                return std::nullopt;

            std::optional<ExpressionContextNode> out;
            tryCastVariant(*parsed, out);
            return out;
        }

        std::optional<Node<ConstValueContext>> parseNumber(TSNode root)
        {
            std::string_view text = nodeText(root);
            float value;
            auto [ptr, res] = std::from_chars(text.data(), text.data() + text.size(), value);
            if(res == std::errc())
            {
                errorMessage(root, std::format("Error parsing float: {}", std::make_error_code(res).message()));
                return std::nullopt;
            }
            auto constNode = makeNode<ConstValueContext>(root);
            constNode->value = value;
            return constNode;
        }

        std::optional<Node<Identifier>> parseIdentifier(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Identifier))
                return std::nullopt;
            auto ident = makeNode<Identifier>(root);
            ident->text = nodeText(root);
            return ident;
        }

        std::optional<Node<ScopedIdentifier>> parseScopedIdentifier(TSNode root)
        {
            if(!expectNode(root, TSNodeType::ScopedIdentifier))
                return std::nullopt;
            auto scopedId = makeNode<ScopedIdentifier>(root);
            auto scope = pushScope(scopedId);
            foreachNamedNodeChild(root, [&](TSNode ident) {
                auto id = parseIdentifier(ident);
                if(!id)
                    return;
                scopedId->scopes.emplace_back(*id);
            });
            if(scopedId->scopes.empty())
            {
                errorMessage(root, "Expected Identifier!");
                return std::nullopt;
            }
            return scopedId;
        }

        std::optional<Node<TypeContext>> parseType(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Type))
                return std::nullopt;

            auto type = makeNode<TypeContext>(root);
            auto scope = pushScope(type);

            bool queuedReference = false;
            foreachNodeChild(root, [&](TSNode node) {
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
                return std::nullopt;
            }

            return type;
        }

        // std::optional<Node<TemplateArgsContext>> parseTemplateArguments(TSNode root);
        // std::optional<Node<TemplateArgContext>> parseTemplateArgument(TSNode root);
        std::optional<Node<BinaryOperatorContext>> parseAdd(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Add))
                return std::nullopt;
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Add;

            auto tsLeft = getField(root, TSFieldName::Left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(*tsLeft);
            if(!leftNode)
                return std::nullopt;
            opr->left = *leftNode;

            auto tsRight = getField(root, TSFieldName::Left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(*tsRight);
            if(!rightNode)
                return std::nullopt;
            opr->right = *rightNode;

            return opr;
        }

        std::optional<Node<BinaryOperatorContext>> parseSub(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Sub))
                return std::nullopt;
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Sub;

            auto tsLeft = getField(root, TSFieldName::Left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(*tsLeft);
            if(!leftNode)
                return std::nullopt;
            opr->left = *leftNode;

            auto tsRight = getField(root, TSFieldName::Left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(*tsRight);
            if(!rightNode)
                return std::nullopt;
            opr->right = *rightNode;

            return opr;
        }

        std::optional<Node<BinaryOperatorContext>> parseMul(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Mul))
                return std::nullopt;
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Mul;

            auto tsLeft = getField(root, TSFieldName::Left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(*tsLeft);
            if(!leftNode)
                return std::nullopt;
            opr->left = *leftNode;

            auto tsRight = getField(root, TSFieldName::Left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(*tsRight);
            if(!rightNode)
                return std::nullopt;
            opr->right = *rightNode;

            return opr;
        }

        std::optional<Node<BinaryOperatorContext>> parseDiv(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Div))
                return std::nullopt;
            auto opr = makeNode<BinaryOperatorContext>(root);
            auto scope = pushScope(opr);

            opr->opType = BinaryOperator::Div;

            auto tsLeft = getField(root, TSFieldName::Left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(*tsLeft);
            if(!leftNode)
                return std::nullopt;
            opr->left = *leftNode;

            auto tsRight = getField(root, TSFieldName::Left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(*tsRight);
            if(!rightNode)
                return std::nullopt;
            opr->right = *rightNode;

            return opr;
        }

        std::optional<Node<VariableDefinitionContext>> parseVariableDefinition(TSNode root)
        {
            if(!expectNode(root, TSNodeType::VariableDefinition))
                return std::nullopt;
            auto def = makeNode<VariableDefinitionContext>(root);
            auto scope = pushScope(def);

            def->definedValue = makeNode<ValueContext>(root);

            auto tsMut = getField(root, TSFieldName::Mut);
            def->definedValue->isMut = tsMut.has_value();

            auto tsIdNode = getField(root, TSFieldName::Id);
            Expect(root, tsIdNode, "Expected Identifier!");
            auto idNode = parseIdentifier(*tsIdNode);
            if(!idNode)
                return std::nullopt;
            def->definedValue->label = idNode;

            auto tsTypeNode = getField(root, TSFieldName::Type);
            if(tsTypeNode)
                def->definedValue->type = parseType(*tsTypeNode);

            return def;
        }

        std::optional<Node<AssignmentContext>> parseAssign(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Assign))
                return std::nullopt;
            auto assign = makeNode<AssignmentContext>(root);
            auto scope = pushScope(assign);

            auto tsLeft = getField(root, TSFieldName::Left);
            Expect(root, tsLeft, "Expected lvalue");
            auto leftNode = parseExpression(*tsLeft);
            if(!leftNode)
                return std::nullopt;
            assign->lValue = *leftNode;

            auto tsRight = getField(root, TSFieldName::Left);
            Expect(root, tsRight, "Expected rvalue");
            auto rightNode = parseExpression(*tsRight);
            if(!rightNode)
                return std::nullopt;
            assign->rValue = *rightNode;

            return assign;
        }

        std::optional<Node<BlockContext>> parseBlock(TSNode root)
        {
            if(!expectNode(root, TSNodeType::AsyncOperation))
                return std::nullopt;
            auto block = makeNode<BlockContext>(root);
            auto scope = pushScope(block);

            foreachNamedNodeChild(root, [&](TSNode node) {
                auto expr = parseExpression(node);
                if(!expr)
                    return;
                block->expressions.push_back(*expr);
            });

            return block;
        }

        std::optional<Node<SinkDefContext>> parseSinkDef(TSNode root)
        {
            if(!expectNode(root, TSNodeType::SinkDef))
                return std::nullopt;

            auto def = makeNode<SinkDefContext>(root);
            auto scope = pushScope(def);

            auto tsIdNode = getField(root, TSFieldName::Id);
            Expect(root, tsIdNode, "Expected Identifier");
            auto idNode = parseIdentifier(*tsIdNode);
            if(!idNode)
                return std::nullopt;
            def->id = *idNode;

            auto tsValueNode = getField(root, TSFieldName::Value);
            Expect(root, tsValueNode, "Expected Expression");
            auto valueNode = parseExpression(*tsValueNode);
            if(!valueNode)
                return std::nullopt;
            def->expression = *valueNode;
            return def;
        }

        std::optional<Node<SinkListContext>> parseSinkList(TSNode root)
        {
            if(!expectNode(root, TSNodeType::SinkList))
                return std::nullopt;

            auto list = makeNode<SinkListContext>(root);
            auto scope = pushScope(list);

            foreachNamedNodeChild(root, [&](TSNode tsSinkDefNode) {
                auto sinkDefNode = parseSinkDef(tsSinkDefNode);
                if(!sinkDefNode)
                    return;
                list->values.push_back(*sinkDefNode);
            });

            return list;
        }

        std::optional<Node<ValueContext>> parseSourceDef(TSNode root)
        {
            if(!expectNode(root, TSNodeType::SourceDef))
                return std::nullopt;

            auto def = makeNode<ValueContext>(root);
            auto scope = pushScope(def);

            def->isLValue = true;

            auto mutField = getField(root, TSFieldName::Mut);
            def->isMut = mutField.has_value();

            auto tsIdNode = getField(root, TSFieldName::Id);
            Expect(root, tsIdNode, "Expected identifier");
            def->label = parseIdentifier(*tsIdNode);

            auto tsTypeNode = getField(root, TSFieldName::Type);
            Expect(root, tsTypeNode, "Expected type");
            auto typeNode = parseType(*tsTypeNode);
            if(!typeNode)
                return std::nullopt;
            def->type = *typeNode;

            return def;
        }

        std::optional<Node<SourceListContext>> parseSourceList(TSNode root)
        {
            if(!expectNode(root, TSNodeType::SourceList))
                return std::nullopt;

            auto list = makeNode<SourceListContext>(root);
            auto scope = pushScope(list);

            foreachNamedNodeChild(root, [&](TSNode tsSourceDefNode) {
                auto sourceDefNode = parseSourceDef(tsSourceDefNode);
                if(!sourceDefNode)
                    return;
                list->defs.push_back(*sourceDefNode);
            });

            return list;
        }

        std::optional<Node<CallContext>> parseCall(TSNode root)
        {
            auto range = nodeRange(root);
            warningMessage(root,
                           "Function calls not implemented yet! \n\"" +
                               std::string(source.substr(range.start_byte, range.end_byte - range.start_byte)) +
                               "\" will be ignored");
            return std::nullopt;
        }

        std::optional<Node<PipelineStageContext>> parsePipelineStage(TSNode root)
        {
            if(!expectNode(root, TSNodeType::PipelineStage))
                return std::nullopt;
            auto stage = makeNode<PipelineStageContext>(root);
            auto scope = pushScope(stage);

            foreachNamedNodeChild(root, [&](TSNode node) {
                auto expr = parseExpression(node);
                if(!expr)
                    return;
                stage->expressions.push_back(*expr);
            });

            return stage;
        }

        std::optional<Node<AsyncExpressionContext>> parseAsyncOperation(TSNode root)
        {
            if(!expectNode(root, TSNodeType::AsyncOperation))
                return std::nullopt;
            auto range = nodeRange(root);
            warningMessage(root,
                           "Async Operations not implemented yet! \n\"" +
                               std::string(source.substr(range.start_byte, range.end_byte - range.start_byte)) +
                               "\" will be ignored");
            return std::nullopt;
        }

        std::optional<Node<FunctionContext>> parseFunction(TSNode root)
        {
            warningMessage(root, "Functions not implemented yet!");
            return std::nullopt;
        }

        std::optional<Node<PipelineContext>> parsePipeline(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Pipeline))
                return std::nullopt;
            auto tsIdNode = getField(root, TSFieldName::Id);
            Expect(root, tsIdNode, "Identifier was not found");
            auto idNode = parseIdentifier(*tsIdNode);
            auto pipe = makeNode<PipelineContext>(root);
            idNode.value()->parent = pipe;
            pipe->identifier = *idNode;

            auto tsSourcesNode = getField(root, TSFieldName::Sources);
            Expect(root, tsSourcesNode, "Pipeline sources list not found");
            auto sourcesNode = parseSourceList(*tsSourcesNode);
            if(!sourcesNode)
                return std::nullopt;
            pipe->sources = *sourcesNode;

            auto tsSinksNode = getField(root, TSFieldName::Sinks);
            Expect(root, tsSinksNode, "Pipeline sinks not found");
            auto sinksNode = parseSinkList(*tsSinksNode);
            if(!sinksNode)
                return std::nullopt;
            pipe->sinks = *sinksNode;

            auto tsStagesNode = getField(root, TSFieldName::Stages);
            Expect(root, tsStagesNode, "Pipeline must have at least one stage");
            advanceWhileType<TSNodeType::PipelineStage, TSNodeType::AsyncOperation>(*tsStagesNode,
                                                                                    [&](TSNode tsStageNode) {
                auto stageNode = parse(tsStageNode);
                if(!stageNode)
                    return;
                std::visit(overloads{[&](Node<PipelineStageContext> stage) { pipe->stages.push_back(stage); },
                                     [&](Node<AsyncExpressionContext> asyncOp) {
                    pipe->stages.back()->asyncExpressions.push_back(asyncOp);
                },
                                     [&](auto& none) {
                    errorMessage(tsStageNode,
                                 std::string("Grammer was correct, but context created was wrong: ") +
                                     typeid(none).name());
                }},
                           *stageNode);
            });

            return pipe;
        }

        std::optional<Node<ModuleContext>> parseModule(TSNode root)
        {
            if(!expectNode(root, TSNodeType::Module))
                return std::nullopt;
            auto mod = makeNode<ModuleContext>(root);
            auto scope = pushScope(mod);

            auto identifier = getField(root, TSFieldName::Id);
            if(!identifier)
                return std::nullopt;

            auto idNode = parseIdentifier(*identifier);
            if(!idNode)
                return std::nullopt;
            idNode.value()->parent = mod;
            mod->identifier = *idNode;

            auto firstDef = getField(root, TSFieldName::Defs);
            if(!firstDef)
                return mod;
            for(auto currentDef = *firstDef; !ts_node_is_null(currentDef) && ts_node_is_named(currentDef);
                currentDef = ts_node_next_named_sibling(currentDef))
            {
                std::optional<TextContextNode> def = parse(currentDef);
                if(!def)
                    continue;
                std::visit(
                    overloads{[&](Node<PipelineContext>& pipeline) { mod->pipelines.push_back(std::move(pipeline)); },
                              [&](Node<FunctionContext>& function) { mod->functions.push_back(std::move(function)); },
                              [&](auto& none) {
                    errorMessage(currentDef,
                                 std::string("Grammer was correct, but context created was wrong: ") +
                                     typeid(none).name());
                }},
                    *def);
            }
            return mod;
        }

        ParserResult<DocumentContext> parseDocument()
        {
            tree = ts_parser_parse_string(parser->parser(), nullptr, source.data(), source.size());

            auto doc = std::make_shared<DocumentContext>();
            auto scope = pushScope(doc);

            doc->source = path;

            TSNode root = ts_tree_root_node(tree);

            foreachNodeChild(root, [&](TSNode node) {
                auto newMod = parseModule(node);
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
        ParserAPI ctx{_path, _source, _parser};
        _cachedResult = std::make_optional(ctx.parseDocument());
        return _cachedResult.value();
    }


} // namespace BraneScript
