//
// Created by wirewhiz on 1/23/23.
//

#include <memory>
#include <typeinfo>
#include "parser.h"
#include "constexprEvaluator.h"
#include "functionCallTree.h"

namespace BraneScript
{
#define NULL_EXPR                                                                                                      \
    (ExpressionContext*)new ExpressionErrorContext { "null expr", __LINE__ }
#define RETURN_EXPR(expr) return (ExpressionContext*)expr
#define RETURN_STMT(expr) return (StatementContext*)expr
#define ASSERT_EXISTS(ptr)                                                                                             \
    if(!(ptr))                                                                                                         \
        return {}
#define EXPR_ASSERT_EXISTS(ptr, message)                                                                               \
    if(!(ptr))                                                                                                         \
    {                                                                                                                  \
        recordError(ctx, message);                                                                                     \
        return (ExpressionContext*)new ExpressionErrorContext{message, __LINE__};                                      \
    }
#define STMT_ASSERT_EXISTS(ptr, message)                                                                               \
    if(!(ptr))                                                                                                         \
    {                                                                                                                  \
        recordError(ctx, message);                                                                                     \
        return (StatementContext*)new StatementErrorContext{message, __LINE__};                                        \
    }

    class AnalyzerCore : public braneBaseVisitor
    {

        class BraneErrorListener : public antlr4::BaseErrorListener
        {
            AnalyzerCore* _analyzer = nullptr;

          public:
            void syntaxError(antlr4::Recognizer* recognizer,
                             antlr4::Token* offendingSymbol,
                             size_t line,
                             size_t charPositionInLine,
                             const std::string& msg,
                             std::exception_ptr e) override
            {
                if(!_analyzer)
                {
                    fprintf(stderr, "lexer/parser error: %s\n", msg.c_str());
                    return;
                }

                if(offendingSymbol)
                    _analyzer->recordError(offendingSymbol,
                                           "Unexpected token: " + offendingSymbol->getText() + "\n" + msg);
                else
                    _analyzer->recordError({line, charPositionInLine}, msg);
            }

            void setAnalyzer(AnalyzerCore* analyzer) { _analyzer = analyzer; }
        };

        Parser& _analyzer;
        Parser::ParserContext& _result;
        std::stack<DocumentContext*> _documentContext;
        bool _extractOnlyIdentifiers;
        bool _allowUnsafe;

        bool _functionHasReturn = false;
        bool _enforceConstexpr = false;

        size_t _lambdaCounter = 0;
        robin_hood::unordered_map<std::string, ModuleContext*> _linkedLibraries;
        robin_hood::unordered_map<std::string, TypeContext> _nativeTypes = {
            {"bool", {"bool", ValueType::Bool}},
            {"char", {"char", ValueType::Char}},
            {"uint", {"uint", ValueType::UInt32}},
            {"uint64", {"uint64", ValueType::UInt64}},
            {"int", {"int", ValueType::Int32}},
            {"int64", {"int64", ValueType::Int64}},
            {"float", {"float", ValueType::Float32}},
            {"double", {"double", ValueType::Float64}},
            {"FuncRef", {"FuncRef", ValueType::FuncRef}}};

        ModuleContext* _currentModule = nullptr;

        using MappedTemplateArgs = robin_hood::unordered_map<std::string, std::unique_ptr<TemplateArgContext>>;

        std::list<MappedTemplateArgs> _templateArgs;
        const size_t maxTemplateDepth = 200;
        bool _instantiatingTemplate = false;
        bool _instantiatingTemplateMembers = false;

        robin_hood::unordered_map<std::string, std::vector<ValueContext>> _funcRefArgTypes;

        TemplateArgContext* getTemplateArg(const std::string& id)
        {
            for(auto i = _templateArgs.rbegin(); i != _templateArgs.rend(); i++)
            {
                auto arg = i->find(id);
                if(arg == i->end())
                    continue;
                return arg->second.get();
            }
            return nullptr;
        }

        std::unique_ptr<ExpressionContext> asExpr(std::any value)
        {
            if(!value.has_value())
                return std::make_unique<ExpressionErrorContext>("internal null value error", __LINE__);
            if(value.type() != typeid(ExpressionContext*))
                return std::make_unique<ExpressionErrorContext>("internal casting error, attempted to cast type " +
                                                                    std::string(value.type().name()) + " to expression",
                                                                __LINE__);
            return std::unique_ptr<ExpressionContext>(std::any_cast<ExpressionContext*>(value));
        }

        std::unique_ptr<StatementContext> asStmt(std::any value)
        {
            if(!value.has_value())
                return std::make_unique<StatementErrorContext>("internal null value error", __LINE__);
            if(value.type() == typeid(ExpressionContext*))
                return std::unique_ptr<StatementContext>(std::any_cast<ExpressionContext*>(value));
            if(value.type() == typeid(StatementContext*))
                return std::unique_ptr<StatementContext>(std::any_cast<StatementContext*>(value));
            return std::make_unique<StatementErrorContext>("internal casting error, attempted to cast type " +
                                                               std::string(value.type().name()) + " to statement",
                                                           __LINE__);
        }

        TextRange toRange(antlr4::Token* token)
        {
            if(!token)
                return _documentContext.top()->range;
            return TextRange{token->getLine() - 1,
                             token->getCharPositionInLine(),
                             token->getLine() - 1,
                             token->getCharPositionInLine() + (token->getStopIndex() - token->getStartIndex()) + 1};
        };

        TextRange toRange(antlr4::ParserRuleContext* ctx)
        {
            if(!ctx)
                return _documentContext.top()->range;
            return TextRange{ctx->getStart()->getLine() - 1,
                             ctx->getStart()->getCharPositionInLine(),
                             ctx->getStop()->getLine() - 1,
                             ctx->getStop()->getCharPositionInLine() +
                                 (ctx->getStop()->getStopIndex() - ctx->getStop()->getStartIndex()) + 1};
        };

        void initDoc(DocumentContext* doc, TextRange range)
        {
            doc->parent = lastNode();
            doc->range = range;
            doc->version = _result.version;
            _documentContext.push(doc);
        }

        void initDoc(DocumentContext* doc) { initDoc(doc, _documentContext.top()->range); }

        void initDoc(DocumentContext* doc, antlr4::Token* token) { initDoc(doc, toRange(token)); }

        void initDoc(DocumentContext* doc, antlr4::ParserRuleContext* ctx) { initDoc(doc, toRange(ctx)); }

        void popDoc(DocumentContext* doc)
        {
            assert(_documentContext.top() == doc);
            _documentContext.pop();
        }

        /**
         * @brief Checks node version numbers to determine if a node is still present in a document, and deletes it if
         * it does not
         * @param vector list to prune
         */
        template<typename T>
        void pruneNodes(LabeledNodeMap<T>& nodes)
        {
            auto itr = nodes.begin();
            while(itr != nodes.end())
            {
                if(itr->second->version != _result.version)
                    itr = nodes.erase(itr);
                else
                    ++itr;
            }
        }

        template<typename T>
        void pruneNodes(LabeledNodeList<T>& nodes)
        {
            nodes.erase(
                std::remove_if(nodes.begin(), nodes.end(), [this](auto& n) { return n->version != _result.version; }),
                nodes.end());
        }

        /**
         * @brief Get an existing node from the passed list, or create a new one if one does not exist
         * Node type must have an "identifier" member
         * @tparam T The type of node we need
         */
        template<typename T>
        T* getNode(LabeledNodeMap<T>& nodes, const std::string& identifier)
        {
            auto n = nodes.find(identifier);
            if(n != nodes.end())
                return n->second.get();
            auto newNode = new T{};
            newNode->identifier.text = identifier;
            nodes.insert({identifier, std::unique_ptr<T>(newNode)});
            return newNode;
        }

        template<typename T>
        T* getNode(LabeledNodeList<T>& nodes, const std::string& identifier)
        {
            for(auto& n : nodes)
            {
                if(n->identifier.text == identifier)
                    return n.get();
            }
            auto newNode = new T{};
            newNode->identifier.text = identifier;
            nodes.push_back(std::unique_ptr<T>(newNode));
            return newNode;
        }

        std::string safeGetText(antlr4::Token* token)
        {
            if(!token)
                return "";
            return token->getText();
        }

        std::string templateArgsToString(const TemplateArgs& args)
        {
            std::string str = "<";
            for(int i = 0; i < args.args.size(); ++i)
            {
                auto& arg = args.args[i];
                if(arg.type == TemplateArgs::Type::Typedef)
                    str += arg.typeDef.signature();
                else if(arg.type == TemplateArgs::Type::Value)
                    str += arg.value->toString();
                if(i != args.args.size() - 1)
                    str += ",";
            }
            str += ">";
            return str;
        }

        std::string templateDefToString(braneParser::TemplateDefContext* tempDef)
        {
            std::string str = "<";
            auto argDefs = std::any_cast<std::vector<TemplateDefArgumentContext*>>(visitTemplateDef(tempDef));
            size_t index = 0;
            for(auto& arg : argDefs)
            {
                auto* argInstance = getTemplateArg(arg->identifier);
                assert(argInstance || arg->type == TemplateDefArgumentContext::ArgType::TypedefPack);
                if(!argInstance)
                    continue;
                if(auto typeArg = dynamic_cast<ValueArgContext*>(argInstance))
                {
                    if(index > 0)
                        str += ",";
                    str += typeArg->value->toString();
                }
                if(auto typeArg = dynamic_cast<TypedefArgContext*>(argInstance))
                {
                    if(index > 0)
                        str += ",";
                    str += typeArg->value.signature();
                }
                if(auto typePackArg = dynamic_cast<TypedefPackArgContext*>(argInstance))
                {
                    bool isFirst = index == 0;
                    for(auto& value : typePackArg->values)
                    {
                        if(!isFirst)
                            str += ", ";
                        str += value.signature();
                        isFirst = false;
                    }
                }

                delete arg;
                index++;
            }
            str += ">";
            return str;
        }

        std::string functionSig(const std::string& identifier, const std::vector<ValueContext> args)
        {
            std::string sig = identifier + "(";
            for(int i = 0; i < args.size(); ++i)
            {
                sig += args[i].signature();
                if(i != args.size() - 1)
                    sig += ",";
            }
            sig += ")";
            return sig;
        }

        std::string functionSig(const std::string& identifier, const LabeledNodeList<LabeledValueContext>& args)
        {
            std::string sig = identifier + "(";
            for(int i = 0; i < args.size(); ++i)
            {
                sig += args[i]->signature();
                if(i != args.size() - 1)
                    sig += ",";
            }
            sig += ")";
            return sig;
        }

        /**
         * Is this DocumentContext a child of the current ScriptContext being constructed
         */
        bool isLocal(const DocumentContext* node) const
        {
            if(!node)
                return false;
            return node->getParent<const ModuleContext>() == _currentModule;
        }

        DocumentContext* getIdentifierContextFromLibs(const std::string& identifier)
        {
            // For a template, search its module links first
            if(_instantiatingTemplate)
            {
                auto currentMod = getLast<ModuleContext>();
                if(currentMod != _currentModule)
                {
                    for(auto& link : currentMod->links)
                    {
                        if(!link.second->alias.empty())
                            continue;
                        if(!link.second->module)
                        {
                            link.second->module = _analyzer.getModule(link.second->moduleName);
                            if(!link.second->module)
                                continue;
                        }
                        if(auto* ident = link.second->module->findIdentifier(identifier, IDSearchOptions_ChildrenOnly))
                            return ident;
                    }
                }
            }

            for(auto& link : _currentModule->links)
            {
                if(!link.second->alias.empty())
                    continue;
                if(!link.second->module)
                {
                    link.second->module = _analyzer.getModule(link.second->moduleName);
                    if(!link.second->module)
                        continue;
                }
                if(auto* ident = link.second->module->findIdentifier(identifier, IDSearchOptions_ChildrenOnly))
                    return ident;
            }
            return nullptr;
        }

        DocumentContext* getIdentifierContext(const std::string& identifier)
        {
            if(auto* localCtx = lastNode()->findIdentifier(identifier, IDSearchOptions_ParentsOnly))
                return localCtx;
            if(auto* libMemberCtx = getIdentifierContextFromLibs(identifier))
                return libMemberCtx;
            if(auto* mod = _analyzer.getModule(identifier))
                return mod;

            auto currentMod = getLast<ModuleContext>();
            assert(currentMod);
            for(auto& link : currentMod->links)
            {
                if(identifier == link.second->alias)
                    return link.second->module;
            }
            return nullptr;
        }

        DocumentContext* getIdentifierContext(braneParser::ScopedIDContext* ctx, DocumentContext* parent = nullptr)
        {
            ASSERT_EXISTS(ctx->id);

            DocumentContext* found = nullptr;
            std::string id = ctx->id->getText();

            if(ctx->template_)
            {
                if(!parent)
                    found = getIdentifierContext(id);
                else
                    found = parent->findIdentifier(id, IDSearchOptions_ChildrenOnly);
                if(found && found->is<TemplateHandle>())
                {
                    TemplateArgs args = std::any_cast<TemplateArgs>(visitTemplateArgs(ctx->template_));

                    found = getInstance(found->as<TemplateHandle>(), args);
                    if(!found)
                        return nullptr;

                    if(ctx->child)
                        return getIdentifierContext(ctx->child, found);
                    return found;
                }
            }

            if(!parent)
                found = getIdentifierContext(id);
            else
                found = parent->findIdentifier(id, IDSearchOptions_ChildrenOnly);

            if(!found && _instantiatingTemplate)
            {
                auto* tempArg = getTemplateArg(id);
                if(auto* tempValueArg = dynamic_cast<ValueArgContext*>(tempArg))
                    found = tempValueArg->value->deepCopy();
                if(auto* tempTypedefArg = dynamic_cast<TypedefArgContext*>(tempArg))
                    found = tempTypedefArg->value.type.structCtx;
            }

            if(found && ctx->child)
                return getIdentifierContext(ctx->child, found);

            if(found)
                return found;

            //If we have not found it by this point it must be a function or just not exist
            auto funcOverrides = getFunctionOverrides(id, parent, ctx->template_);
            if(!funcOverrides->overrides.empty() || !funcOverrides->templates.empty())
               return funcOverrides;
            delete funcOverrides;

            return nullptr;
        }

        TypeContext getTypeContext(braneParser::ScopedIDContext* ctx)
        {
            ASSERT_EXISTS(ctx->id);
            TypeContext output;
            output.identifier = ctx->id->getText();
            if(output.identifier == "void")
                return output;

            if(_nativeTypes.contains(output.identifier))
            {
                output.storageType = _nativeTypes.at(output.identifier).storageType;
                if(ctx->template_)
                {
                    if(output.storageType != ValueType::FuncRef)
                    {
                        recordError(ctx->child, "Native type " + output.identifier + " is not a template");
                        return output;
                    }
                    auto args = std::any_cast<TemplateArgs>(visitTemplateArgs(ctx->template_));
                    if(args.args.empty())
                    {
                        recordError(ctx->child, output.identifier + " expects at least 1 template argument");
                        return output;
                    }
                    output.identifier = output.identifier + templateArgsToString(args);

                    std::vector<ValueContext> argTypes;
                    for(auto& arg : args.args)
                    {
                        if(arg.type != TemplateArgs::Type::Typedef)
                        {
                            recordError(ctx, output.identifier + " expects only typedef arguments");
                            return output;
                        }
                        argTypes.push_back(arg.typeDef);
                    }
                    _funcRefArgTypes[output.identifier] = std::move(argTypes);
                }
                if(ctx->child)
                    recordError(ctx->child, "Native type " + output.identifier + " has no members");
                return output;
            }

            auto sCtx = dynamic_cast<StructContext*>(getIdentifierContext(ctx));
            if(!sCtx)
                sCtx = dynamic_cast<StructContext*>(getIdentifierContextFromLibs(output.identifier));
            if(sCtx)
            {
                output.identifier = sCtx->longId();
                output.structCtx = sCtx;
                output.storageType = ValueType::Struct;
                return output;
            }

            return output;
        }

        std::string idToString(braneParser::ScopedIDContext* ctx)
        {
            ASSERT_EXISTS(ctx->id);
            std::string id = ctx->id->getText();
            if(ctx->template_)
                id += templateArgsToString(std::any_cast<TemplateArgs>(visitTemplateArgs(ctx->template_)));
            if(ctx->child)
                return id + "::" + idToString(ctx->child);
            return id;
        }

        bool implicitCastViable(const ValueContext& current, const ValueContext& target)
        {
            if(current.isRef && target.isRef &&
               (current.type.storageType == ValueType::None || target.type.storageType == ValueType::None))
                return true;
            return current.type.isScalar() && target.type.isScalar();
        }

        bool
        castSame(std::unique_ptr<ExpressionContext>& left, std::unique_ptr<ExpressionContext>& right, std::string& err)
        {
            left.reset(asRValue(left.release()));
            right.reset(asRValue(right.release()));

            if(left->returnType.sameBaseType(right->returnType))
                return true;

            if(left->returnType.castCost(right->returnType) < right->returnType.castCost(left->returnType))
            {
                auto* oldLeft = left.release();
                auto* newLeft = resolveCast(oldLeft, right->returnType, err);
                if(!newLeft)
                {
                    left.reset(oldLeft);
                    return false;
                }
                left.reset(newLeft);
            }
            else
            {
                auto* oldRight = right.release();
                auto* newRight = resolveCast(oldRight, left->returnType, err);
                if(!newRight)
                {
                    right.reset(oldRight);
                    return false;
                }
                right.reset(newRight);
            }
            return true;
        }

        /**
         * @return Returns a pair, with the first member being the parent context of the last element in the scoped id
         * and the second is the last child. The first member is null if there is only one element in the scoped id.
         */
        std::pair<DocumentContext*, braneParser::ScopedIDContext*>
        getLastScopedID(braneParser::ScopedIDContext* idCtx, braneParser::ScopedIDContext* first = nullptr)
        {
            if(!first)
                first = idCtx;
            if(idCtx->child)
                return getLastScopedID(idCtx->child, first);

            std::pair<DocumentContext*, braneParser::ScopedIDContext*> output = {nullptr, idCtx};

            if(first != idCtx)
            {
                auto parent = dynamic_cast<braneParser::ScopedIDContext*>(idCtx->parent);
                parent->child = nullptr;
                output.first = getIdentifierContext(first);
                parent->child = idCtx;
            }
            return output;
        }

        void getFunctionOverridesInLinks(const std::string& name, FunctionOverridesContext* overrides, ModuleContext* mod, robin_hood::unordered_set<ModuleContext*>& searched)
        {
            searched.insert(mod);
            for(auto& link : mod->links)
            {
                if(!link.second->alias.empty())
                    continue;
                auto m = link.second->module;
                if(!m)
                    continue;
                if(searched.contains(m))
                    continue;
                m->getFunction(name, overrides, IDSearchOptions_ChildrenOnly);
                getFunctionOverridesInLinks(name, overrides, m, searched);
            }
        }

        FunctionOverridesContext* getFunctionOverrides(const std::string& name,
                                                       DocumentContext* scope = nullptr,
                                                       braneParser::TemplateArgsContext* tempArgsCtx = nullptr)
        {
            auto overrides = new FunctionOverridesContext{};
            initDoc(overrides);

            if(scope)
                scope->getFunction(name, overrides, IDSearchOptions_ChildrenOnly);
            else
            {
                robin_hood::unordered_set<ModuleContext*> searched;
                if(_instantiatingTemplate)
                {
                    auto tempScope = getLast<ModuleContext>();
                    if(tempScope != _currentModule)
                    {
                        tempScope->getFunction(name, overrides, IDSearchOptions_ChildrenOnly);
                        searched.insert(tempScope);
                        getFunctionOverridesInLinks(name, overrides, tempScope, searched);
                    }
                }
                _currentModule->getFunction(name, overrides, IDSearchOptions_ChildrenOnly);
                getFunctionOverridesInLinks(name, overrides, _currentModule, searched);
            }

            if(tempArgsCtx)
            {
                if(overrides->templates.empty())
                {
                    recordError(tempArgsCtx, "\"" + name + "\" is not a template!");
                    popDoc(overrides);
                    return overrides;
                }
                overrides->templateArgs = std::any_cast<TemplateArgs>(visitTemplateArgs(tempArgsCtx));
            }

            popDoc(overrides);
            return overrides;
        }

        FunctionContext* bestOverride(const std::vector<ValueContext>& args,
                                   const FunctionOverridesContext::CastCallback& canImplicitCast,
                                   const FunctionOverridesContext* overrides,
                                   FunctionOverridesContext::MatchFlags flags = FunctionOverridesContext::None)
        {
            if(overrides->overrides.empty() && overrides->templates.empty())
                return nullptr;
            /* We want to find the best match with the lowest amount of implicit casts. So we store the cast count for
         * each candidate function and try to find the closest one. Cast cost is a measure of the desirability of
         * casts the casts. For instance a cast from uint32 to uint64 is more desirable than a cast from uint32_t to
         * a float.
         * */
            uint16_t bestCastCount = -1;
            uint32_t bestCastCost = -1;
            FunctionContext* bestMatch = nullptr;
            if(overrides->templateArgs.args.empty())
            {
                for(auto f : overrides->overrides)
                {
                    if((flags & FunctionOverridesContext::Constexpr) && !f->isConstexpr)
                        continue;

                    uint8_t castCount = 0;
                    uint16_t castCost = 0;

                    if(f->arguments.size() != args.size())
                        continue;

                    bool argsMatch = true;
                    for(size_t i = 0; i < args.size(); ++i)
                    {
                        if(f->arguments[i]->type != args[i].type)
                        {
                            if(!canImplicitCast(*f->arguments[i], args[i]))
                            {
                                argsMatch = false;
                                break;
                            }
                            castCount++;
                            castCost += args[i].castCost(*f->arguments[i]);
                        }
                        // Honor constness
                        if(args[i].isConst && f->arguments[i]->isRef && !f->arguments[i]->isConst)
                        {
                            argsMatch = false;
                            break;
                        }
                    }
                    if(!argsMatch)
                        continue;
                    // This is the best match if it requires fewer casts or has the same amount and just costs lest
                    if(castCount < bestCastCount || (castCount == bestCastCount && bestCastCost < castCost))
                    {
                        bestMatch = f;
                        bestCastCount = castCount;
                        bestCastCost = castCost;
                    }
                }
            }

            if(!bestMatch && !overrides->templates.empty())
            {
                for(auto th : overrides->templates)
                {
                    auto* f = (FunctionContext*)getInstance(th, overrides->templateArgs);
                    if(!f)
                        continue;

                    if((flags & FunctionOverridesContext::Constexpr) && !f->isConstexpr)
                        continue;

                    uint8_t castCount = 0;
                    uint16_t castCost = 0;

                    if(f->arguments.size() != args.size())
                        continue;

                    bool argsMatch = true;
                    for(size_t i = 0; i < args.size(); ++i)
                    {
                        if(f->arguments[i]->type != args[i].type)
                        {
                            if(!canImplicitCast(*f->arguments[i], args[i]))
                            {
                                argsMatch = false;
                                break;
                            }
                            castCount++;
                            castCost += args[i].castCost(*f->arguments[i]);
                        }
                        // Honor constness
                        if(args[i].isConst && f->arguments[i]->isRef && !f->arguments[i]->isConst)
                        {
                            argsMatch = false;
                            break;
                        }
                    }
                    if(!argsMatch)
                        continue;
                    // This is the best match if it requires fewer casts or has the same amount and just costs lest
                    if(castCount < bestCastCount || (castCount == bestCastCount && bestCastCost < castCost))
                    {
                        bestMatch = f;
                        bestCastCount = castCount;
                        bestCastCost = castCost;
                    }
                }
            }

            return bestMatch;
        }

        /** Converts references to non-references */
        ExpressionContext* asRValue(ExpressionContext* value)
        {
            if(!value->returnType.isRef)
                return value;
            auto deref = new DereferenceContext(value);
            initDoc(deref);
            popDoc(deref);
            return deref;
        }

        ExpressionContext* resolveCast(ExpressionContext* from, const ValueContext& to, std::string& error)
        {
            /* We want to insert the error at the position that would make the most sense, which would be right after
             * the last one before a cast was attempted, but not after we get the error about the casting operator not
             * being found.
             */
            if(from->returnType.type == to.type)
            {
                if(!from->returnType.isRef && to.isRef && from->returnType.isLValue)
                {
                    auto createRef = new CreateReferenceContext(from);
                    initDoc(createRef);
                    popDoc(createRef);
                    return createRef;
                }
                if(from->returnType.isRef && !to.isRef)
                    return asRValue(from);
            }


            if(NativeCastContext::validCast(from->returnType, to))
            {
                auto castCtx = new NativeCastContext(to, from);
                initDoc(castCtx);
                popDoc(castCtx);
                return castCtx;
            }

            size_t errorPos = error.size();
            auto castCall = new FunctionCallContext{};
            castCall->arguments.emplace_back(from);

            auto castOverrides = getFunctionOverrides("opr " + to.signature(), nullptr, nullptr);
            castCall->function = bestOverride(
                {castCall->arguments[0]->returnType},
                [](auto from, auto to) { return false; },
                castOverrides,
                _enforceConstexpr ? FunctionOverridesContext::Constexpr : FunctionOverridesContext::None);
            if(!castCall->function)
            {
                std::string castError =
                    "No casting operator found for " + from->returnType.signature() + " to " + to.signature() + "!\n";
                error.insert(error.begin() + errorPos, castError.begin(), castError.end());
                castCall->arguments[0].release();
                delete castCall;
                return nullptr;
            }

            castCall->returnType = to;
            initDoc(castCall);
            popDoc(castCall);

            return castCall;
        }

        ExpressionContext* resolveOperator(const std::string opr,
                                           std::string& error,
                                           std::unique_ptr<ExpressionContext> lValue,
                                           std::unique_ptr<ExpressionContext> rValue = nullptr)
        {
            size_t errorPos = error.size();
            auto oprCall = new FunctionCallContext{};
            oprCall->arguments.emplace_back(std::move(lValue));
            if(rValue)
                oprCall->arguments.emplace_back(std::move(rValue));

            auto oprOverrides = getFunctionOverrides("opr " + opr, nullptr, nullptr);
            std::vector<ValueContext> argTypes;
            if(oprCall->arguments.size() == 2)
                argTypes = {oprCall->arguments[0]->returnType, oprCall->arguments[1]->returnType};
            else
                argTypes = {oprCall->arguments[0]->returnType};

            oprCall->function = bestOverride(
                argTypes,
                [this](auto from, auto to) { return implicitCastViable(from, to); },
                oprOverrides,
                _enforceConstexpr ? FunctionOverridesContext::Constexpr : FunctionOverridesContext::None);
            if(!oprCall->function)
            {
                if(oprCall->arguments.size() == 1)
                    error = "No operator \"" + opr + "\" found with argument " +
                            oprCall->arguments[0]->returnType.signature() + "!\n";
                else
                    error = "No operator \"" + opr + "\" found with arguments " +
                            oprCall->arguments[0]->returnType.signature() + " and " +
                            oprCall->arguments[1]->returnType.signature() + "!\n";
                delete oprCall;
                return nullptr;
            }
            initDoc(oprCall);
            popDoc(oprCall);
            oprCall->returnType = oprCall->function->returnType;

            return oprCall;
        }

      public:
        AnalyzerCore(Parser& analyzer,
                     Parser::ParserContext& result,
                 bool extractOnlyIdentifiers,
                 bool allowUnsafe)
            : _analyzer(analyzer), _result(result), _extractOnlyIdentifiers(extractOnlyIdentifiers),
              _allowUnsafe(allowUnsafe){};

        std::any visitExprStatement(braneParser::ExprStatementContext* ctx) override
        {
            return visit(ctx->expression());
        }

        std::any visitTags(braneParser::TagsContext* ctx) override
        {
            std::vector<std::string> tags;
            for(auto& tag : ctx->STRING())
            {
                std::string text = tag->getText();
                if(text.size() < 2)
                    continue;
                // Remove the quotes
                tags.push_back(text.substr(1, text.size() - 2));
            }
            return tags;
        }

        std::any visitModule(braneParser::ModuleContext* ctx) override
        {
            ASSERT_EXISTS(ctx->id);
            assert(lastNode()->is<ScriptContext>());
            std::string identifier = safeGetText(ctx->id);
            if(identifier.size() < 2)
                return {};

            identifier = identifier.substr(1, identifier.size() - 2);
            if(auto prexisting = _analyzer.getModule(identifier))
            {
                if(!isLocal(prexisting) || prexisting->version == _result.version)
                {
                    recordError(ctx->id, "Module with id: \"" + identifier + "\" has already been defined!");
                    return {};
                }
            }

            auto mod = getNode(_result.scriptContext->modules, identifier);
            mod->identifier.range = toRange(ctx->id);
            _currentModule = mod;

            if(ctx->tags())
                mod->tags = std::any_cast<std::vector<std::string>>(visitTags(ctx->tags()));

            initDoc(mod, ctx);
            for(auto link : ctx->linkList()->link())
                visitLink(link);
            for(auto component : ctx->moduleComponent())
                visit(component);
            popDoc(mod);

            pruneNodes(mod->globals);
            pruneNodes(mod->structs);
            pruneNodes(mod->functions);
            pruneNodes(mod->templates);
            pruneNodes(mod->links);

            if(!_extractOnlyIdentifiers)
            {
                LabeledNodeList<LabeledValueContext> args = {};
                // Make constructor
                {
                    auto func = getFunctionNode(mod->longId() + "::_construct", args);
                    initDoc(func, ctx->start);
                    popDoc(func);
                    func->body = std::make_unique<ScopeContext>();

                    size_t varIndex = 0;
                    for(auto& var : mod->globals)
                    {
                        if(var->type.storageType == ValueType::Struct)
                        {
                            auto globalRef = new LabeledValueReferenceContext{*var};

                            auto constructCall = new FunctionCallContext{};
                            constructCall->arguments.emplace_back(globalRef);
                            constructCall->function = var->type.structCtx->constructor;
                            if(!constructCall->function)
                                recordError(ctx->start,
                                            "Could not find constructor for struct \"" + var->type.structCtx->longId() +
                                                "\"");
                            func->body->expressions.emplace_back(constructCall);
                        }
                        varIndex++;
                    }
                }
                // Make destructor
                {
                    auto func = getFunctionNode(mod->longId() + "::_destruct", args);
                    initDoc(func, ctx->start);
                    popDoc(func);
                    func->body = std::make_unique<ScopeContext>();

                    size_t varIndex = 0;
                    for(auto& var : mod->globals)
                    {
                        if(var->type.storageType == ValueType::Struct)
                        {
                            auto globalRef = new LabeledValueReferenceContext{*var};

                            auto constructCall = new FunctionCallContext{};
                            constructCall->arguments.emplace_back(globalRef);
                            constructCall->function = var->type.structCtx->destructor;
                            if(!constructCall->function)
                                recordError(ctx->start,
                                            "Could not find destructor for struct \"" + var->type.structCtx->longId() +
                                                "\"");
                            func->body->expressions.emplace_back(constructCall);
                        }
                        varIndex++;
                    }
                }
            }

            if(_extractOnlyIdentifiers)
                _analyzer.registerModule(mod);

            _currentModule = nullptr;
            return {};
        }

        void linkModuleRecursive(ModuleContext* mod)
        {
            for(auto& link : mod->links)
            {
                if(_currentModule->links.contains(link.second->moduleName))
                    continue;
                if(!link.second->isPublic)
                    continue;
                _currentModule->links.insert(
                    {link.second->moduleName,
                     std::unique_ptr<LinkContext>((LinkContext*)(link.second->DocumentContext::deepCopy()))});
                linkModuleRecursive(link.second->module);
            }
            _analyzer.registerModule(mod);
        }

        std::any visitLink(braneParser::LinkContext* ctx) override
        {
            ASSERT_EXISTS(ctx->library);
            LinkContext link;
            if(ctx->alias)
            {
                link.alias = safeGetText(ctx->alias);
                if(link.alias.size() >= 2)
                    link.alias = link.alias.substr(1, link.alias.size() - 2);
            }
            link.isPublic = ctx->isPublic;
            link.moduleName = safeGetText(ctx->library);
            if(link.moduleName.size() >= 2)
                link.moduleName = link.moduleName.substr(1, link.moduleName.size() - 2);
            initDoc(&link, ctx);
            popDoc(&link);
            link.module = _analyzer.getModule(link.moduleName);
            if(!link.module && !_extractOnlyIdentifiers)
                recordError(ctx->library, "Module \"" + link.moduleName + "\" not found!");

            if(!_allowUnsafe && link.moduleName == "unsafe")
                recordError(ctx->library, "Unsafe code is not allowed for this script");

            lastNode()->as<ModuleContext>()->links[link.moduleName] = std::make_unique<LinkContext>(link);
            return {};
        }

        std::any visitGlobal(braneParser::GlobalContext* ctx) override
        {
            ASSERT_EXISTS(ctx->id);
            ASSERT_EXISTS(ctx->type());

            auto globVar = getNode(_currentModule->globals, safeGetText(ctx->id));
            globVar->identifier.range = toRange(ctx->id);
            initDoc(globVar, ctx);
            auto value = std::any_cast<ValueContext>(visit(ctx->type()));
            globVar->type = value.type;
            globVar->isConst = value.isConst;
            globVar->isRef = value.isRef;
            globVar->isLValue = true;
            popDoc(globVar);
            return {};
        }

        std::any visitTemplateDefArgument(braneParser::TemplateDefArgumentContext* ctx) override
        {
            auto arg = new TemplateDefArgumentContext{};
            if(ctx->isTypedef)
                arg->type = ctx->isPack ? TemplateDefArgumentContext::TypedefPack : TemplateDefArgumentContext::Typedef;
            else if(ctx->exprType)
            {
                if(ctx->isPack)
                    recordError(ctx->isPack, "Expression arguments can not have variable length");
                arg->type = TemplateDefArgumentContext::Value;
                arg->valueType = std::any_cast<ValueContext>(visitType(ctx->exprType)).type;
            }
            arg->identifier = ctx->id->getText();
            return arg;
        }

        std::any visitTemplateDef(braneParser::TemplateDefContext* ctx) override
        {
            std::vector<TemplateDefArgumentContext*> args;
            auto argsCtx = ctx->templateDefArgument();
            for(auto arg : argsCtx)
            {
                auto newArg = std::any_cast<TemplateDefArgumentContext*>(visitTemplateDefArgument(arg));
                args.push_back(newArg);
                if(arg->isPack && args.size() != argsCtx.size())
                    recordError(arg->isPack, "Only the last template argument may be a pack!");
            }
            return std::move(args);
        }

        std::any visitTemplateArgs(braneParser::TemplateArgsContext* ctx) override
        {
            TemplateArgs args;
            for(auto* arg : ctx->templateArg())
            {
                if(auto* exprArg = dynamic_cast<braneParser::TemplateExprArgContext*>(arg))
                {
                    auto expr = asExpr(visit(exprArg->expr));
                    if(!expr->isConstexpr())
                    {
                        recordError(exprArg, "Template arguments must be constant expressions");
                        continue;
                    }
                    if(!_analyzer.constexprEvaluator())
                        throw std::runtime_error("Static analyzer must have an attached constexpr evaluator to use "
                                                 "expressions as template arguments");
                    auto exprResult = _analyzer.constexprEvaluator()->evaluateConstexpr(expr.get());
                    if(!exprResult)
                    {
                        recordError(exprArg, "Could not evaluate expression");
                        continue;
                    }
                    args.addArg(exprResult);

                    continue;
                }
                if(auto* typeArg = dynamic_cast<braneParser::TemplateTypeArgContext*>(arg))
                {
                    args.addArg(std::any_cast<ValueContext>(visit(typeArg)));
                    continue;
                }
                if(auto* packArg = dynamic_cast<braneParser::PackExpansionArgContext*>(arg))
                {
                    std::string packID = safeGetText(packArg->packID);
                    auto* argInstance = getTemplateArg(packID);
                    if(!argInstance)
                    {
                        recordError(arg, packID + " does not refer to a template argument!");
                        continue;
                    }
                    auto* pack = dynamic_cast<TypedefPackArgContext*>(argInstance);
                    if(!pack)
                    {
                        recordError(arg, packID + " is not a pack argument!");
                        continue;
                    }
                    for(auto& value : pack->values)
                        args.addArg(value);
                    break;
                }
            }
            return std::move(args);
        }

        std::any visitScopedID(braneParser::ScopedIDContext* ctx) override
        {
            assert(false && "Allways call getIdentifier instead visit");
            return {};
        }

        std::any visitType(braneParser::TypeContext* ctx) override
        {
            ValueContext output;
            output.type = getTypeContext(ctx->name);
            output.isConst = ctx->isConst;
            output.isRef = !!ctx->isRef | !!ctx->isArrayRef;
            if(ctx->isArrayRef)
            {
                output.arraySize = size_t(-1);
                if(ctx->size)
                    output.arraySize = std::stoul(ctx->size->getText());
            }

            if(output.type.storageType == ValueType::None && output.type.identifier != "void")
            {
                auto* arg = getTemplateArg(output.type.identifier);
                if(_instantiatingTemplate && arg)
                {
                    if(auto* tempType = dynamic_cast<TypedefArgContext*>(arg))
                    {
                        output.type = tempType->value.type;
                        output.isConst = output.isConst || tempType->value.isConst;
                        output.isRef = output.isRef || tempType->value.isRef;
                    }
                    else if(auto* tempTypePack = dynamic_cast<TypedefPackArgContext*>(arg))
                        recordError(ctx->name, "Cannot use an argument pack as a type!");
                }
                else
                    recordError(ctx->name, "Type " + output.signature() + " could not be found!");
            }

            if(_enforceConstexpr && ctx->isRef)
                recordError(ctx, "References not supported in constexpr scopes!");

            if(_enforceConstexpr && output.type.storageType == ValueType::Struct)
                recordError(ctx, "Types of struct not supported in constexpr scopes");

            assert(output.type.storageType != ValueType::Struct || output.type.structCtx);
            return output;
        }

        std::any visitDeclaration(braneParser::DeclarationContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->type(), "expecting a type!");
            EXPR_ASSERT_EXISTS(ctx->id, "expecting an identifier!");

            std::string id = ctx->id->getText();
            if(auto prexisting = getIdentifierContext(id))
            {
                if(prexisting->version == _result.version && !prexisting->is<ModuleContext>())
                {
                    recordError(ctx->id, "Identifier \"" + id + "\" already exists as \"" + prexisting->longId() + "\"!");
                    return LabeledValueContext{};
                }
            }

            LabeledValueContext valueContext{};

            initDoc(&valueContext, ctx);
            auto type = std::any_cast<ValueContext>(visitType(ctx->type()));
            valueContext.type = std::move(type.type);
            valueContext.isConst = type.isConst;
            valueContext.isRef = type.isRef;
            valueContext.arraySize = type.arraySize;

            valueContext.identifier.text = safeGetText(ctx->id);
            valueContext.identifier.range = toRange(ctx->id);

            popDoc(&valueContext);
            return valueContext;
        }

        std::any visitArgumentList(braneParser::ArgumentListContext* ctx) override
        {
            assert(false);
            return {};
        }

        LabeledNodeList<LabeledValueContext> constructArgumentList(braneParser::ArgumentListContext* ctx,
                                                                   ArgPackInstanceContext** argPackInstance = nullptr)
        {
            LabeledNodeList<LabeledValueContext> args;
            for(auto& item : ctx->argumentListItem())
            {
                if(auto* declaration = item->declaration())
                    args.emplace_back(
                        new LabeledValueContext{std::any_cast<LabeledValueContext>(visitDeclaration(declaration))});
                else
                {
                    if(!_instantiatingTemplate)
                    {
                        recordError(item, "argument packs may only be used in templates!");
                        continue;
                    }
                    std::string packID = safeGetText(item->pack);
                    auto* argInstance = getTemplateArg(packID);
                    if(!argInstance)
                    {
                        recordError(item->id, packID + " is not a template argument!");
                        continue;
                    }
                    auto* pack = dynamic_cast<TypedefPackArgContext*>(argInstance);
                    if(!pack)
                    {
                        recordError(item->id, packID + " is not an argument pack!");
                        continue;
                    }

                    std::string prefix = safeGetText(item->id);
                    size_t labelIndex = 0;
                    if(argPackInstance)
                    {
                        if(*argPackInstance)
                        {
                            recordError(item, "Only one argument pack is in an argument list is allowed!");
                            continue;
                        }
                        *argPackInstance = new ArgPackInstanceContext{};
                        (*argPackInstance)->identifier = prefix;
                    }
                    for(auto& value : pack->values)
                    {
                        auto* valueInstance =
                            new LabeledValueContext{prefix + " " + std::to_string(labelIndex++), value};
                        if(argPackInstance)
                            (*argPackInstance)->values.emplace_back(valueInstance);
                        args.emplace_back(valueInstance);
                    }
                }
            }
            return std::move(args);
        }

        bool populateTemplateArgs(TemplateHandle* temp, const TemplateArgs& args)
        {
            if(_templateArgs.size() == maxTemplateDepth)
            {
                recordError(temp->root, "Maximum template depth of " + std::to_string(maxTemplateDepth) + " reached!");
                return false;
            }

            if(args.args.size() < temp->args.size() - 1)
                return false;
            size_t arg = 0;
            MappedTemplateArgs argsToPush;
            for(size_t argDef = 0; argDef < temp->args.size(); ++argDef)
            {
                std::string identifier = temp->args[argDef]->identifier;
                if(temp->args[argDef]->type == TemplateDefArgumentContext::Value)
                {
                    if(arg == args.args.size())
                        return false;
                    if(args.args[arg].type != TemplateArgs::Type::Value)
                        return false;
                    if(temp->args[argDef]->valueType != args.args[arg].value->returnType.type)
                        return false;
                    auto valueArg = new ValueArgContext{};
                    valueArg->identifier = identifier;
                    valueArg->value.reset((ConstValueContext*)args.args[arg++].value->deepCopy());
                    argsToPush.emplace(identifier, valueArg);
                }
                else if(temp->args[argDef]->type == TemplateDefArgumentContext::Typedef)
                {
                    if(arg == args.args.size())
                        return false;
                    if(args.args[arg].type != TemplateArgs::Type::Typedef)
                        return false;
                    argsToPush.emplace(identifier, new TypedefArgContext{identifier, args.args[arg++].typeDef});
                }
                else if(temp->args[argDef]->type == TemplateDefArgumentContext::TypedefPack)
                {
                    if(argDef != temp->args.size() - 1)
                        return false;
                    auto pack = new TypedefPackArgContext{};
                    pack->identifier = identifier;
                    while(arg != args.args.size())
                    {
                        if(args.args[arg].type != TemplateArgs::Type::Typedef)
                            return false;
                        pack->values.push_back(args.args[arg++].typeDef);
                    }
                    argsToPush.emplace(std::move(identifier), pack);
                }
            }
            if(arg != args.args.size())
                return false;
            _templateArgs.push_back(std::move(argsToPush));
            return true;
        }

        DocumentContext* getInstance(TemplateHandle* handle, const TemplateArgs& args)
        {
            assert(handle);
            if(!populateTemplateArgs(handle, args))
                return nullptr;

            _instantiatingTemplate = true;
            DocumentContext* generated = nullptr;

            _documentContext.push(handle->parent);
            if(auto* structDef = dynamic_cast<braneParser::StructDefContext*>(handle->root))
                generated = std::any_cast<StructContext*>(visitStructDef(structDef));
            if(auto* functionDef = dynamic_cast<braneParser::FunctionContext*>(handle->root))
                generated = std::any_cast<FunctionContext*>(visitFunction(functionDef));
            _documentContext.pop();

            _templateArgs.pop_back();
            _instantiatingTemplate = !_templateArgs.empty();
            return generated;
        }

        FunctionContext* getFunctionNode(const std::string& identifier,
                                         const LabeledNodeList<LabeledValueContext>& args)
        {
            LabeledNodeList<FunctionContext>* functions = nullptr;
            if(auto* s = getLast<StructContext>())
                functions = &s->functions;
            else
                functions = &_currentModule->functions;
            assert(functions);

            // See if a function with this overload already exists
            for(auto& f : *functions)
            {
                if(f->identifier.text != identifier)
                    continue;
                if(f->arguments.size() != args.size())
                    continue;
                bool argumentsMatch = true;
                for(size_t a = 0; a < args.size(); ++a)
                {
                    if(*f->arguments[a] != *args[a])
                    {
                        argumentsMatch = false;
                        break;
                    }
                }
                if(argumentsMatch)
                    return f.get();
            }
            // If the function was not found, add it
            auto func = new FunctionContext{};
            func->identifier.text = identifier;
            func->version = -1;
            functions->emplace_back(func);
            return func;
        }

        // I despise std::any
        struct FunctionSig
        {
            std::string id;
            ValueContext returnType;
            std::vector<std::string> tags;
        };

        virtual std::any visitFunctionSig(braneParser::FunctionSigContext* ctx) override
        {
            ASSERT_EXISTS(ctx->type() || ctx->castType);
            ASSERT_EXISTS(ctx->id || ctx->oprID || ctx->castType || ctx->bracketOprID);

            FunctionSig o;
            o.returnType = std::any_cast<ValueContext>((ctx->castType) ? visit(ctx->castType) : visit(ctx->type()));
            if(ctx->id)
                o.id = safeGetText(ctx->id);
            else if(ctx->oprID)
                o.id = "opr " + ctx->oprID->getText();
            else if(ctx->castType)
                o.id = "opr " + o.returnType.signature();
            else if(ctx->bracketOprID)
                o.id = "opr " + ctx->bracketOprID->getText();

            // Modify id if we are instantiating a template
            if(_instantiatingTemplate && ctx->template_)
                o.id += templateDefToString(ctx->template_);

            // Add tags
            if(ctx->tags())
                o.tags = std::any_cast<std::vector<std::string>>(visitTags(ctx->tags()));

            if(_enforceConstexpr && o.returnType.type.storageType == ValueType::Struct)
                recordError((ctx->castType) ? ctx->castType : ctx->type(),
                            "Return types of struct are not yet supported in constexpr functions");

            return o;
        }

        std::any visitFunctionStub(braneParser::FunctionStubContext* ctx) override
        {
            ASSERT_EXISTS(ctx->sig);
            auto sigAny = visitFunctionSig(ctx->functionSig());
            if(!sigAny.has_value())
                return (FunctionContext*)nullptr;
            if(ctx->sig->template_)
                recordError(ctx->sig->template_, "External functions may not be templates!");

            FunctionSig sig = std::move(std::any_cast<FunctionSig>(sigAny));
            auto arguments = constructArgumentList(ctx->arguments);

            auto parentStruct = lastNode()->getLast<StructContext>();
            if(parentStruct)
            {
                auto* thisRef = new LabeledValueContext{};
                thisRef->type = TypeContext{parentStruct->longId(), ValueType::Struct, parentStruct};
                thisRef->identifier.text = "this";
                thisRef->isLValue = true;
                thisRef->isRef = true;
                thisRef->isConst = ctx->isConst;
                arguments.emplace(arguments.begin(), thisRef);
            }

            FunctionContext* func = getFunctionNode(sig.id, arguments);
            func->identifier.range = toRange(ctx->sig->id);
            func->tags = std::move(sig.tags);
            func->returnType = sig.returnType;
            for(auto& arg : arguments)
                assert(arg->type.storageType != ValueType::Struct || arg->type.structCtx);
            func->arguments = std::move(arguments);
            func->isConstexpr = ctx->functionSig()->isConstexpr;

            for(auto& a : func->arguments)
            {
                a->parent = func;
                a->isLValue = true;
            }

            if(func->version == _result.version)
                recordError(ctx->sig->id, "Redefinition of already existing function!");

            initDoc(func, ctx);
            popDoc(func);
            return (FunctionContext*)func;
        }

        std::any visitFunction(braneParser::FunctionContext* ctx) override
        {
            ASSERT_EXISTS(ctx->sig);

            if(ctx->sig->template_ && (_instantiatingTemplateMembers || !_instantiatingTemplate))
            {
                std::string id = safeGetText(ctx->sig->id);
                auto args = std::any_cast<std::vector<TemplateDefArgumentContext*>>(visit(ctx->sig->template_));

                LabeledNodeMap<TemplateHandle>* templates = nullptr;
                if(auto* s = getLast<StructContext>())
                    templates = &s->templates;
                else
                    templates = &_currentModule->templates;

                if(templates->contains(id) && templates->at(id)->version == _result.version)
                    recordError(ctx->sig->id, "Redefinition of already existing template!");

                (*templates)[id] = std::make_unique<TemplateHandle>(TemplateHandle::Function, ctx, lastNode(), args);
                (*templates)[id]->version = _result.version;
                return (FunctionContext*)nullptr;
            }

            auto sigAny = visitFunctionSig(ctx->sig);
            if(!sigAny.has_value())
                return (FunctionContext*)nullptr;

            FunctionSig sig = std::move(std::any_cast<FunctionSig>(sigAny));

            ArgPackInstanceContext* argPackInstanceContext = nullptr;
            auto arguments = std::move(constructArgumentList(ctx->arguments, &argPackInstanceContext));

            auto parentStruct = lastNode()->getLast<StructContext>();
            auto constructorType = ConstructorType::None;
            if(parentStruct)
            {
                auto* thisRef = new LabeledValueContext{};
                thisRef->type = TypeContext{parentStruct->longId(), ValueType::Struct, parentStruct};
                thisRef->identifier.text = "this";
                thisRef->isLValue = true;
                thisRef->isRef = true;
                thisRef->isConst = ctx->isConst;
                arguments.emplace(arguments.begin(), thisRef);

                if(sig.id == "_construct")
                {
                    if(arguments.size() != 1)
                        recordError(ctx->arguments, "Constructor may have no arguments, it is intended for initializing default values only!");
                    constructorType = ConstructorType::Default;
                }
                else if(sig.id == "_copy")
                {
                    if(arguments.size() != 2 || arguments[1]->type.structCtx != parentStruct || !arguments[1]->isConst)
                        recordError(ctx->sig->id, "Copy constructor must have exactly one argument of type \"const ref " + parentStruct->longId() + "\".");
                }
                else if(sig.id == "_move")
                {
                    if(arguments.size() != 2 || arguments[1]->type.structCtx != parentStruct || arguments[1]->isConst)
                        recordError(ctx->sig->id, "Copy constructor must have exactly one argument of type \"ref " + parentStruct->longId() + "\".");
                }
                else if(sig.id == "_destruct")
                {
                    if(arguments.size() != 1)
                        recordError(ctx->sig->id, "Destructor may have no arguments!");
                    constructorType = ConstructorType::Destructor;
                }
            }

            FunctionContext* func = getFunctionNode(sig.id, arguments);

            func->identifier.range = toRange(ctx->sig->id);
            func->tags = std::move(sig.tags);
            func->returnType = sig.returnType;
            func->arguments = std::move(arguments);
            func->isConstexpr = ctx->functionSig()->isConstexpr;
            func->isTemplateInstance = _instantiatingTemplate;
            if(argPackInstanceContext)
            {
                func->argPackInstances.insert({argPackInstanceContext->identifier, std::move(*argPackInstanceContext)});
                delete argPackInstanceContext;
            }

            for(size_t i = 0; i < func->arguments.size(); i++)
            {
                auto& a = func->arguments[i];
                a->parent = func;
                a->isLValue = true;
                if(a->type.storageType == ValueType::Struct && !a->isRef)
                    recordError(
                        a->identifier.range,
                        R"(Structs may only be passed by reference, consider adding the "ref" and "const" descriptors instead)");
            }

            if(func->version == _result.version)
            {
                // If we are attempting to instantiate a template, but it turns out we already have, just return the
                // existing one
                if(_instantiatingTemplate)
                    return func;
                recordError(ctx->functionSig()->id, "Redefinition of already existing function!");
            }

            bool cachedFHR = _functionHasReturn;
            bool cachedConstexpr = _enforceConstexpr;
            _functionHasReturn = false;
            _enforceConstexpr = func->isConstexpr;

            initDoc(func, ctx);

            if(!_extractOnlyIdentifiers)
            {
                func->body = std::make_unique<ScopeContext>();
                initDoc(func->body.get(), ctx);
                if(constructorType != ConstructorType::None)
                    appendDefaultConstructorOperations(func, constructorType);

                for(auto stmt : ctx->statement())
                {
                    auto statement = asStmt(visit(stmt));
                    if(statement)
                        func->body->expressions.push_back(std::move(statement));
                    if(_documentContext.top() != func->body.get())
                        assert(false);
                }
                popDoc(func->body.get());

                if(!_functionHasReturn && func->returnType.type.storageType != ValueType::None)
                    recordError(ctx->sig->id, "Function never returns " + func->returnType.type.identifier);
            }
            popDoc(func);


            _functionHasReturn = cachedFHR;
            _enforceConstexpr = cachedConstexpr;

            return func;
        }

        std::any visitLambda(braneParser::LambdaContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->arguments, "Expected lambda arguments");
            EXPR_ASSERT_EXISTS(ctx->returnType, "Expected lambda return type");

            auto returnType = std::any_cast<ValueContext>(visitType(ctx->returnType));
            auto arguments = constructArgumentList(ctx->arguments);
            std::vector<std::unique_ptr<ExpressionContext>> captures;

            TemplateArgs lambdaTemplateArgs;
            lambdaTemplateArgs.addArg(returnType);
            for(auto& arg : arguments)
                lambdaTemplateArgs.addArg(*arg);

            std::string lambdaID = "-Lambda" + std::to_string(_lambdaCounter++);
            StructContext* lambdaCaptureStruct = nullptr;
            FunctionContext* copyData = nullptr;

            if(ctx->capture)
            {
                lambdaCaptureStruct = getNode(_currentModule->structs, lambdaID + "Capture");
                initDoc(lambdaCaptureStruct, ctx);

                for(auto& var : ctx->capture->capturedVar())
                {
                    auto node = getIdentifierContext(var->id);
                    if(!node)
                    {
                        recordError(var->id, "Unknown variable " + var->id->getText() + " was not found");
                        continue;
                    }
                    auto capVar = node->as<LabeledValueContext>();
                    if(!capVar)
                    {
                        recordError(var->id, "Identifier " + var->id->getText() + " is not a variable!");
                        continue;
                    }
                    LabeledValueContext member = *capVar;
                    member.isLValue = true;
                    member.isRef = var->isRef;
                    member.parent = lambdaCaptureStruct;
                    lambdaCaptureStruct->variables.push_back(std::make_unique<LabeledValueContext>(std::move(member)));
                    captures.emplace_back(new LabeledValueReferenceContext{*capVar});
                }

                auto* parentFunc = getLast<FunctionContext>();
                buildDefaultConstructor(lambdaCaptureStruct, ConstructorType::Default);
                buildDefaultConstructor(lambdaCaptureStruct, ConstructorType::Copy);
                buildDefaultConstructor(lambdaCaptureStruct, ConstructorType::Move);
                buildDefaultConstructor(lambdaCaptureStruct, ConstructorType::Destructor);

                std::vector<std::unique_ptr<LabeledValueContext>> captureArgs;
                captureArgs.emplace_back(new LabeledValueContext{"this", ValueContext{{lambdaCaptureStruct->longId(), ValueType::Struct, lambdaCaptureStruct}, true, false, true}});
                captureArgs.emplace_back(new LabeledValueContext{"other", ValueContext{{lambdaCaptureStruct->longId(), ValueType::Struct, lambdaCaptureStruct}, true, true, true}});
                copyData = getFunctionNode(lambdaID + "CopyData", arguments);
                initDoc(copyData, ctx);
                copyData->arguments = std::move(captureArgs);
                copyData->body = std::make_unique<ScopeContext>();
                LabeledValueContext thisRef = *copyData->arguments[0];
                thisRef.isRef = false;
                copyData->body->expressions.emplace_back(new LabeledValueConstructionContext{thisRef});
                copyData->body->expressions.emplace_back(new AssignmentContext{
                    new LabeledValueReferenceContext(*copyData->arguments[0]),
                    new LabeledValueReferenceContext{*copyData->arguments[1]}
                });

                popDoc(copyData);

                arguments.insert(
                    arguments.begin(),
                    std::make_unique<LabeledValueContext>(
                        "this",
                        ValueContext{
                            {lambdaCaptureStruct->longId(), ValueType::Struct, lambdaCaptureStruct}, false, false, true}));
                popDoc(lambdaCaptureStruct);
            }



            auto lambdaLib = getIdentifierContext("lambda");
            if(!lambdaLib)
            {
                recordError(ctx->label, "Lambda type could not be constructed, make sure you have \"lambda\" linked!");
                return NULL_EXPR;
            }
            auto lambdaTemplate = getIdentifierContext("lambda")->findIdentifier("Lambda");
            if(!lambdaTemplate || !lambdaTemplate->is<TemplateHandle>())
            {
                recordError(ctx->label,
                            "Lambda type could not be constructed, but the library is linked, the standard module "
                            "files may be damaged!");
                return NULL_EXPR;
            }

            auto* lambdaCtx = new LambdaInstanceContext{};
            initDoc(lambdaCtx, ctx);

            auto* lambdaType =
                getInstance(lambdaTemplate->as<TemplateHandle>(), lambdaTemplateArgs)->as<StructContext>();
            if(!lambdaType)
            {
                recordError(ctx->label,
                            "Lambda type could not be constructed, but the library is linked, native libraries may be "
                            "damaged!");
                popDoc(lambdaCtx);
                RETURN_EXPR(lambdaCtx);
            }

            if(lambdaCaptureStruct)
            {
                TemplateArgs captureAllocArg;
                captureAllocArg.addArg(
                    {{lambdaCaptureStruct->longId(), ValueType::Struct, lambdaCaptureStruct}, false, false, false});

                auto allocFuncTemplate = lambdaType->templates.find("_allocCaptureData");
                if(allocFuncTemplate == lambdaType->templates.end() || !allocFuncTemplate->second->is<TemplateHandle>())
                {
                    recordError(ctx->label,
                                "Lambda alloc function could not be constructed, native libraries may be damaged!");
                    popDoc(lambdaCtx);
                    RETURN_EXPR(lambdaCtx);
                }
                lambdaCtx->allocFunc =
                    getInstance(allocFuncTemplate->second->as<TemplateHandle>(), captureAllocArg)->as<FunctionContext>();
                assert(lambdaCtx->allocFunc);
            }

            lambdaCtx->returnType = {{lambdaType->longId(), ValueType::Struct, lambdaType}, true, false, false};
            lambdaCtx->lambdaType = lambdaType;
            lambdaCtx->captureType = lambdaCaptureStruct;
            lambdaCtx->captures = std::move(captures);
            lambdaCtx->copyFunc = copyData;

            FunctionContext* func = getFunctionNode(lambdaID + "Body", arguments);
            initDoc(func, ctx);
            func->arguments = std::move(arguments);
            func->returnType = std::any_cast<ValueContext>(visitType(ctx->returnType));

            bool cachedFHR = _functionHasReturn;
            bool cachedConstexpr = _enforceConstexpr;
            _functionHasReturn = false;
            _enforceConstexpr = func->isConstexpr;
            func->body = std::make_unique<ScopeContext>();
            initDoc(func->body.get(), ctx);
            for(auto stmt : ctx->statement())
            {
                auto statement = asStmt(visit(stmt));
                if(statement)
                    func->body->expressions.push_back(std::move(statement));
            }
            popDoc(func->body.get());

            if(!_functionHasReturn && func->returnType.type.storageType != ValueType::None)
                recordError(ctx->label, "Lambda never returns " + func->returnType.type.identifier);
            popDoc(func);

            _functionHasReturn = cachedFHR;
            _enforceConstexpr = cachedConstexpr;


            lambdaCtx->func = func;

            popDoc(lambdaCtx);
            RETURN_EXPR(lambdaCtx);
        }

        std::any visitMemberVariable(braneParser::MemberVariableContext* ctx) override
        {
            ASSERT_EXISTS(ctx->var);
            ASSERT_EXISTS(ctx->var->id);
            auto var = getNode(lastNode()->as<StructContext>()->variables, safeGetText(ctx->var->id));
            *var = std::any_cast<LabeledValueContext>(visit(ctx->var));
            var->isLValue = true;
            initDoc(var, ctx);
            // TODO default value
            popDoc(var);
            return {};
        }

        std::any visitMemberFunction(braneParser::MemberFunctionContext* ctx) override
        {
            return visitFunction(ctx->func);
        }

        enum class ConstructorType
        {
            None,
            Default,
            Copy,
            Move,
            Destructor
        };

        FunctionContext* buildDefaultConstructor(StructContext* structCtx, ConstructorType type)
        {
            std::string operationName;
            switch(type)
            {
                case ConstructorType::Default:
                    operationName = "_construct";
                    break;
                case ConstructorType::Copy:
                    operationName = "_copy";
                    break;
                case ConstructorType::Move:
                    operationName = "_move";
                    break;
                case ConstructorType::Destructor:
                    operationName = "_destruct";
                    break;
            }

            TypeContext structType = {structCtx->longId(), ValueType::Struct, structCtx};
            LabeledNodeList<LabeledValueContext> args;
            args.emplace_back(new LabeledValueContext{"this", ValueContext{structType, false, false, true}});

            if(type == ConstructorType::Copy)
                args.emplace_back(new LabeledValueContext{"other", ValueContext{structType, true, true, true}});
            else if(type == ConstructorType::Move)
                args.emplace_back(new LabeledValueContext{"other", ValueContext{structType, true, false, true}});

            _documentContext.push(structCtx);
            auto func = getFunctionNode(operationName, args);
            initDoc(func, structCtx->identifier.range);
            popDoc(func);
            _documentContext.pop();
            func->arguments = std::move(args);
            func->body = std::make_unique<ScopeContext>();

            appendDefaultConstructorOperations(func, type);

            switch(type)
            {
                case ConstructorType::Default:
                    structCtx->constructor = func;
                    break;
                case ConstructorType::Copy:
                    structCtx->copyConstructor = func;
                    break;
                case ConstructorType::Move:
                    structCtx->moveConstructor = func;
                    break;
                case ConstructorType::Destructor:
                    structCtx->destructor = func;
                    break;
            }
            return func;
        }

        void appendDefaultConstructorOperations(FunctionContext* func, ConstructorType type)
        {
            size_t varIndex = 0;
            assert(func->arguments.size() >= 1);
            assert(func->arguments.size() <= 2);
            auto structCtx = func->arguments[0]->type.structCtx;
            for(auto& var : structCtx->variables)
            {
                if(var->type.storageType == ValueType::Struct && !var->isRef)
                {
                    auto memberAccess = new MemberAccessContext{};
                    memberAccess->member = varIndex;
                    memberAccess->returnType = *var;
                    memberAccess->returnType.isRef = true;
                    memberAccess->baseExpression = std::make_unique<LabeledValueReferenceContext>(*func->arguments[0]);

                    MemberAccessContext* otherAccess = nullptr;
                    if(type == ConstructorType::Copy || type == ConstructorType::Move)
                    {
                        otherAccess = new MemberAccessContext{};
                        otherAccess->member = varIndex;
                        otherAccess->returnType = *var;
                        otherAccess->returnType.isRef = true;
                        otherAccess->baseExpression =
                            std::make_unique<LabeledValueReferenceContext>(*func->arguments[1]);
                    }

                    auto constructCall = new FunctionCallContext{};
                    constructCall->arguments.emplace_back(memberAccess);
                    if(otherAccess)
                        constructCall->arguments.emplace_back(otherAccess);
                    switch(type)
                    {
                        case ConstructorType::Default:
                            constructCall->function = var->type.structCtx->constructor;
                            break;
                        case ConstructorType::Copy:
                            constructCall->function = var->type.structCtx->copyConstructor;
                            break;
                        case ConstructorType::Move:
                            constructCall->function = var->type.structCtx->moveConstructor;
                            break;
                        case ConstructorType::Destructor:
                            constructCall->function = var->type.structCtx->destructor;
                            break;
                    }
                    if(!constructCall->function)
                        recordError(structCtx->identifier.range,
                                    "Could not find " + func->identifier.text + " for struct \"" +
                                        var->type.structCtx->longId() + "\"");

                    func->body->expressions.emplace_back(constructCall);
                }
                else if(type == ConstructorType::Copy || type == ConstructorType::Move)
                {
                    auto memberAccess = new MemberAccessContext{};
                    memberAccess->member = varIndex;
                    memberAccess->returnType = *var;
                    memberAccess->returnType.isRef = true;
                    memberAccess->baseExpression = std::make_unique<LabeledValueReferenceContext>(*func->arguments[0]);

                    MemberAccessContext* otherAccess = new MemberAccessContext{};
                    otherAccess->member = varIndex;
                    otherAccess->returnType = *var;
                    otherAccess->returnType.isRef = true;
                    otherAccess->baseExpression = std::make_unique<LabeledValueReferenceContext>(*func->arguments[1]);

                    auto assign = new AssignmentContext{};
                    assign->setArgs(memberAccess, otherAccess);
                    func->body->expressions.emplace_back(assign);
                }
                varIndex++;
            }
        }

        std::any visitStructDef(braneParser::StructDefContext* ctx) override
        {
            ASSERT_EXISTS(ctx->id);

            std::string id = safeGetText(ctx->id);

            // Register template
            if(ctx->template_ && !_instantiatingTemplate)
            {
                std::string prefix = lastNode()->longId();
                auto args = std::any_cast<std::vector<TemplateDefArgumentContext*>>(visitTemplateDef(ctx->template_));

                if(_currentModule->templates.contains(id) &&
                   _currentModule->templates.at(id)->version == _result.version)
                    recordError(ctx->id, "Redefinition of already existing template!");

                _currentModule->templates[id] =
                    std::make_unique<TemplateHandle>(TemplateHandle::Struct, ctx, lastNode(), args);
                _currentModule->templates[id]->version = _result.version;
                return (StructContext*)nullptr;
            }

            // Modify id if we are instantiating a template
            if(ctx->template_)
                id += templateDefToString(ctx->template_);

            auto structCtx = getNode(_currentModule->structs, id);
            structCtx->identifier.range = toRange(ctx->id);
            if(ctx->tags())
                structCtx->tags = std::any_cast<std::vector<std::string>>(visitTags(ctx->tags()));
            if(structCtx->version == _result.version)
            {
                if(_instantiatingTemplate)
                    return structCtx;
                recordError(ctx->id, "Structure with this identifier has already been defined!");
            }

            initDoc(structCtx, ctx);
            _instantiatingTemplateMembers = true;
            for(auto& m : ctx->structMember())
                visit(m);
            _instantiatingTemplateMembers = false;

            pruneNodes(structCtx->variables);
            pruneNodes(structCtx->functions);

            structCtx->constructor = nullptr;
            structCtx->copyConstructor = nullptr;
            structCtx->moveConstructor = nullptr;
            structCtx->destructor = nullptr;
            for(auto& f : structCtx->functions)
            {
                if(f->identifier.text == "_construct")
                    structCtx->constructor = f.get();
                if(f->identifier.text == "_copy")
                    structCtx->copyConstructor = f.get();
                if(f->identifier.text == "_move")
                    structCtx->moveConstructor = f.get();
                if(f->identifier.text == "_destruct")
                    structCtx->destructor = f.get();
            }
            // Make constructor
            if(!structCtx->constructor)
                buildDefaultConstructor(structCtx, ConstructorType::Default);
            // Make copy constructor
            if(!structCtx->copyConstructor)
                buildDefaultConstructor(structCtx, ConstructorType::Copy);
            // Make move constructor
            if(!structCtx->moveConstructor)
                buildDefaultConstructor(structCtx, ConstructorType::Move);
            // Make destructor
            if(!structCtx->destructor)
                buildDefaultConstructor(structCtx, ConstructorType::Destructor);
            structCtx->packed = ctx->packed;
            popDoc(structCtx);

            return structCtx;
        }

        std::any visitScope(braneParser::ScopeContext* ctx) override
        {
            auto scope = new ScopeContext{};
            initDoc(scope, ctx);
            for(auto& s : ctx->statement())
            {
                auto stmt = asStmt(visit(s));
                if(stmt)
                    scope->expressions.push_back(std::move(stmt));
            }

            for(auto& var : scope->localVariables)
            {
                if(!var->type.structCtx)
                    continue;
                auto destructor = new LabeledValueDestructionContext(*var);
                scope->expressions.emplace_back(destructor);
            }

            popDoc(scope);
            RETURN_STMT(scope);
        }

        std::any visitReturn(braneParser::ReturnContext* ctx) override
        {
            auto retCtx = new ReturnContext{};
            initDoc(retCtx, ctx);
            if(ctx->expression())
                retCtx->value = asExpr(visit(ctx->expression()));

            auto& functionType = lastNode()->getParent<FunctionContext>()->returnType;
            if(retCtx->value && !retCtx->value->returnType.sameBaseType(functionType))
            {
                std::string castError;
                if(auto castCtx = resolveCast(retCtx->value.release(), functionType, castError))
                    retCtx->value.reset(castCtx);
                else
                    recordError(ctx, castError);
            }
            if(retCtx->value && retCtx->value->returnType.type.storageType == ValueType::Struct)
            {
                auto* sCtx = retCtx->value->returnType.type.structCtx;
                retCtx->value->returnType.type.identifier = sCtx->longId();
                auto* parentFunc = lastNode()->getParent<FunctionContext>();
            }

            popDoc(retCtx);
            _functionHasReturn = true;
            RETURN_STMT(retCtx);
        }

        std::any visitIf(braneParser::IfContext* ctx) override
        {
            STMT_ASSERT_EXISTS(ctx->cond, "If missing condition");
            STMT_ASSERT_EXISTS(ctx->operation, "If statement missing body");
            auto ifCtx = new IfContext{};
            initDoc(ifCtx, ctx);
            ifCtx->condition = asExpr(visit(ctx->cond));
            if(ifCtx->condition->returnType.type.storageType != ValueType::Bool)
                recordError(ctx->cond, "Condition must be a Boolean value");

            if(_instantiatingTemplate && _analyzer.constexprEvaluator() && ifCtx->condition->isConstexpr() &&
               _result.errors.empty())
            {
                auto condRes = std::unique_ptr<ConstValueContext>(
                    _analyzer.constexprEvaluator()->evaluateConstexpr(ifCtx->condition.get()));
                popDoc(ifCtx);
                delete ifCtx;
                if(dynamic_cast<ConstBoolContext*>(condRes.get())->value)
                    RETURN_STMT(asStmt(visit(ctx->operation)).release());
                if(ctx->elseOp)
                    RETURN_STMT(asStmt(visit(ctx->elseOp)).release());
                RETURN_STMT(new ScopeContext{});
            }


            ifCtx->body = asStmt(visit(ctx->operation));
            if(ctx->elseOp)
                ifCtx->elseBody = asStmt(visit(ctx->elseOp));
            popDoc(ifCtx);

            RETURN_STMT(ifCtx);
        }

        std::any visitWhile(braneParser::WhileContext* ctx) override
        {
            STMT_ASSERT_EXISTS(ctx->cond, "expected while loop condition");
            STMT_ASSERT_EXISTS(ctx->operation, "expected while loop body");
            auto whileCtx = new WhileContext{};
            initDoc(whileCtx, ctx);
            whileCtx->condition = asExpr(visit(ctx->cond));
            whileCtx->body = asStmt(visit(ctx->operation));
            popDoc(whileCtx);
            RETURN_STMT(whileCtx);
        }

        std::any visitFor(braneParser::ForContext* ctx) override
        {
            STMT_ASSERT_EXISTS(ctx->cond, "expected for loop condition");
            STMT_ASSERT_EXISTS(ctx->operation, "while loop body missing");
            auto forCtx = new ForContext{};
            initDoc(forCtx, ctx);
            forCtx->loopScope = std::make_unique<ScopeContext>();
            initDoc(forCtx->loopScope.get(), ctx);

            if(ctx->init)
                forCtx->init = asStmt(visit(ctx->init));
            forCtx->condition = asExpr(visit(ctx->cond));
            if(ctx->step)
                forCtx->step = asStmt(visit(ctx->step));
            forCtx->body = asStmt(visit(ctx->operation));

            popDoc(forCtx->loopScope.get());
            popDoc(forCtx);
            RETURN_STMT(forCtx);
        }

        std::any visitDecl(braneParser::DeclContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->declaration(), "declaration expected");
            auto* scope = lastNode()->as<ScopeContext>();
            if(!scope)
                scope = lastNode()->getParent<ScopeContext>();
            assert(scope);

            auto result = new LabeledValueConstructionContext{};
            initDoc(result, ctx);
            auto decl = std::make_unique<LabeledValueContext>(
                std::any_cast<LabeledValueContext>(visitDeclaration(ctx->declaration())));

            decl->isLValue = true;

            scope->localVariables.push_back(std::move(decl));
            popDoc(result);
            auto value = (--scope->localVariables.end())->get();
            result->identifier = value->identifier;
            result->returnType = *value;

            RETURN_EXPR(result);
        }

        std::any visitAssignment(braneParser::AssignmentContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->lValue, "lValue expected");
            EXPR_ASSERT_EXISTS(ctx->rValue, "rValue expected");
            auto assignmentCtx = new AssignmentContext{};
            initDoc(assignmentCtx, ctx);
            auto lValue = asExpr(visit(ctx->lValue));
            assert(lValue);
            if(!lValue->returnType.isLValue && !lValue->returnType.isRef)
                recordError(ctx->lValue, "Can not assign to a temporary value!");
            if(lValue->returnType.isConst)
                recordError(ctx->lValue, "Can not modify a constant value!");

            auto rValue = asExpr(visit(ctx->rValue));
            assert(rValue);
            if(auto funcOverrides = rValue->as<FunctionOverridesContext>())
            {
                if(lValue->returnType.type.storageType != ValueType::FuncRef)
                    recordError(ctx->rValue, "Can not reference a value of different type!");
                const auto& argTypes = _funcRefArgTypes.at(lValue->returnType.type.identifier);
                auto func = bestOverride(
                    {argTypes.begin() + 1, argTypes.end()},
                    [](auto from, auto to) { return false; },
                    funcOverrides,
                    _enforceConstexpr ? FunctionOverridesContext::Constexpr : FunctionOverridesContext::None);
                if(!func)
                {
                    std::string error = "Could not find override of function \"" + funcOverrides->longId();
                    error += "\" with arguments: ";
                    for(auto& arg : argTypes)
                        error += arg.signature() + ", ";
                    if(!funcOverrides->overrides.empty())
                    {
                        error += "\nDid you mean to use one of these overrides?\n-----\n";
                        for(auto o : funcOverrides->overrides)
                            error += functionSig(o->longId(), o->arguments) + "\n";
                        error += "-----";
                        if(_enforceConstexpr)
                            error += "\nVerify the functions you reference in a constexpr scope are also constexpr.";
                    }
                    recordError(ctx->rValue, error);
                    popDoc(assignmentCtx);
                    RETURN_EXPR(assignmentCtx);
                }
                rValue.reset(new FunctionReferenceContext(func));
            }

            if(lValue->returnType.type != rValue->returnType.type)
            {
                std::string error;
                if(auto castCtx = resolveCast(rValue.release(), lValue->returnType, error))
                    rValue.reset(castCtx);
                else
                    recordError(ctx->rValue, error);
            }

            assignmentCtx->setArgs(lValue.release(), rValue.release());
            popDoc(assignmentCtx);
            RETURN_EXPR(assignmentCtx);
        }

        std::any visitRefAssignment(braneParser::RefAssignmentContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->lValue, "lValue expected");
            EXPR_ASSERT_EXISTS(ctx->rValue, "rValue expected");
            auto assignmentCtx = new RefAssignmentContext{};
            initDoc(assignmentCtx, ctx);
            auto lValue = asExpr(visit(ctx->lValue));
            assert(lValue);
            if(!lValue->returnType.isRef)
                recordError(ctx->lValue, "Expression is not a reference!");
            if(!lValue->returnType.isLValue)
                recordError(ctx->lValue, "Can not assign to a temporary value!");
            if(lValue->returnType.isConst)
                recordError(ctx->lValue, "Can not modify a constant value!");

            auto rValue = asExpr(visit(ctx->rValue));
            assert(rValue);

            if(!(rValue->returnType.isRef || rValue->returnType.isLValue))
            {
                recordError(ctx->rValue, "Can not reference a temporary value!");
                popDoc(assignmentCtx);
                RETURN_EXPR(assignmentCtx);
            }
            if(lValue->returnType.type != rValue->returnType.type)
            {
                recordError(ctx, "Can not reference a value of different type!");
                popDoc(assignmentCtx);
                RETURN_EXPR(assignmentCtx);
            }

            assignmentCtx->setArgs(lValue.release(), rValue.release());
            popDoc(assignmentCtx);
            RETURN_EXPR(assignmentCtx);
        }

        std::any visitConstBool(braneParser::ConstBoolContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->BOOL(), "constant missing");
            auto constCtx = new ConstBoolContext{};
            initDoc(constCtx, ctx);
            constCtx->value = (ctx->getText() == "true");
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        }

        std::any visitConstInt(braneParser::ConstIntContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->INT(), "constant missing");
            auto constCtx = new ConstIntContext{};
            initDoc(constCtx, ctx);
            constCtx->value = std::stoi(ctx->getText());
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        }

        std::any visitConstFloat(braneParser::ConstFloatContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->FLOAT(), "constant missing");
            auto constCtx = new ConstFloatContext{};
            initDoc(constCtx, ctx);
            constCtx->value = std::stof(ctx->getText());
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        }

        std::any visitConstChar(braneParser::ConstCharContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->CHAR(), "constant missing");
            auto constCtx = new ConstCharContext{};
            initDoc(constCtx, ctx);
            constCtx->value = ctx->getText()[1];
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        }

        std::any visitConstString(braneParser::ConstStringContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->STRING(), "constant missing");
            auto constCtx = new ConstStringContext{};
            initDoc(constCtx, ctx);
            auto stringLib = _currentModule->links.find("string");
            if(stringLib == _currentModule->links.end())
            {
                recordError(ctx, "To use the string type, the \"string\" module must be linked.");
                popDoc(constCtx);
                return NULL_EXPR;
            }
            constCtx->returnType.type.structCtx =  dynamic_cast<StructContext*>(stringLib->second->module->findIdentifier("string", IDSearchOptions_ChildrenOnly));
            if(!constCtx->returnType.type.structCtx)
            {
                recordError(ctx, "Native string type not found! The \"string\" module may be damaged.");
                popDoc(constCtx);
                return NULL_EXPR;
            }
            constCtx->value = ctx->getText();
            constCtx->value = {constCtx->value.data() + 1, constCtx->value.size() - 2};
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        }

        std::any visitSizeOfType(braneParser::SizeOfTypeContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->t, "type expected");
            auto value = std::any_cast<ValueContext>(visitType(ctx->t));
            auto constCtx = new TypeSizeContext{value};
            initDoc(constCtx, ctx);
            popDoc(constCtx);
            RETURN_EXPR(constCtx);
        };

        std::any visitSizeOfPack(braneParser::SizeOfPackContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->id, "expression missing");
            auto constCtx = new ConstIntContext{};
            initDoc(constCtx, ctx);
            constCtx->value = 1;
            popDoc(constCtx);
            if(!_instantiatingTemplate)
            {
                recordError(ctx, "sizeof... can only be used inside templates");
                RETURN_EXPR(constCtx);
            }
            std::string id = safeGetText(ctx->id);
            auto* argInstance = getTemplateArg(id);
            if(!argInstance)
            {
                recordError(ctx, id + " is not a template parameter!");
                RETURN_EXPR(constCtx);
            }
            auto* pack = dynamic_cast<TypedefPackArgContext*>(argInstance);
            if(!pack)
            {
                recordError(ctx, id + " is not a type pack!");
                RETURN_EXPR(constCtx);
            }
            constCtx->value = pack->values.size();
            RETURN_EXPR(constCtx);
        };

        std::any visitParen(braneParser::ParenContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->expression(), "expression expected");
            return visit(ctx->expression());
        }

        std::any visitCast(braneParser::CastContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->type(), "type expected");
            EXPR_ASSERT_EXISTS(ctx->expression(), "expression expected");

            auto returnType = std::any_cast<ValueContext>(visitType(ctx->type()));
            std::string error;
            if(auto* castCtx = resolveCast(asExpr(visit(ctx->expression())).release(), returnType, error))
            {
                initDoc(castCtx, ctx);
                popDoc(castCtx);
                RETURN_EXPR(castCtx);
            }
            else
                recordError(ctx, error);
            return NULL_EXPR;
        }

        std::any visitAddsub(braneParser::AddsubContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->left, "expression expected");
            EXPR_ASSERT_EXISTS(ctx->opr, "operator expected");
            EXPR_ASSERT_EXISTS(ctx->right, "expression expected");

            auto left = asExpr(visit(ctx->left));
            auto right = asExpr(visit(ctx->right));
            std::string op = ctx->opr->getText();

            if(NativeArithmeticContext::validArithmetic(left->returnType, right->returnType))
            {
                std::string err;
                if(castSame(left, right, err))
                    RETURN_EXPR(new NativeArithmeticContext((op == "+") ? NativeArithmeticContext::ADD
                                                                        : NativeArithmeticContext::SUB,
                                                            left.release(),
                                                            right.release()));
            }

            std::string error;
            if(auto callCtx = resolveOperator(op, error, std::move(left), std::move(right)))
                RETURN_EXPR(callCtx);
            recordError(ctx->opr, error);
            return NULL_EXPR;
        }

        std::any visitMuldiv(braneParser::MuldivContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->left, "expression expected");
            EXPR_ASSERT_EXISTS(ctx->opr, "operator expected");
            EXPR_ASSERT_EXISTS(ctx->right, "expression expected");

            auto left = asExpr(visit(ctx->left));
            auto right = asExpr(visit(ctx->right));
            std::string op = ctx->opr->getText();

            if(NativeArithmeticContext::validArithmetic(left->returnType, right->returnType))
            {
                std::string err;
                if(castSame(left, right, err))
                    RETURN_EXPR(new NativeArithmeticContext((op == "*") ? NativeArithmeticContext::MUL
                                                                        : NativeArithmeticContext::DIV,
                                                            left.release(),
                                                            right.release()));
            }

            std::string error;
            if(auto callCtx = resolveOperator(op, error, std::move(left), std::move(right)))
                RETURN_EXPR(callCtx);
            recordError(ctx->opr, error);
            return NULL_EXPR;
        }

        std::any visitPreInc(braneParser::PreIncContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->value, "expression expected");
            auto value = asExpr(visit(ctx->value));
            if(!isValueTypeScalar(value->returnType.type.storageType))
            {
                recordError(ctx, "pre increment operator may only be used on scalar types!");
                return NULL_EXPR;
            }
            auto incCtx = new NativeIncrementContext(value.release(), true);
            initDoc(incCtx, ctx);
            popDoc(incCtx);
            RETURN_EXPR(incCtx);
        }

        std::any visitPostInc(braneParser::PostIncContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->value, "expression expected");
            auto value = asExpr(visit(ctx->value));
            if(!isValueTypeScalar(value->returnType.type.storageType))
            {
                recordError(ctx, "post increment operator may only be used on scalar types!");
                return NULL_EXPR;
            }
            auto incCtx = new NativeIncrementContext(value.release(), false);
            initDoc(incCtx, ctx);
            popDoc(incCtx);
            RETURN_EXPR(incCtx);
        }

        std::any visitPreDec(braneParser::PreDecContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->value, "expression expected");
            auto value = asExpr(visit(ctx->value));
            if(!isValueTypeScalar(value->returnType.type.storageType))
            {
                recordError(ctx, "pre decrement operator may only be used on scalar types!");
                return NULL_EXPR;
            }
            auto decCtx = new NativeDecrementContext(value.release(), true);
            initDoc(decCtx, ctx);
            popDoc(decCtx);
            RETURN_EXPR(decCtx);
        }

        std::any visitPostDec(braneParser::PostDecContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->value, "expression expected");
            auto value = asExpr(visit(ctx->value));
            if(!isValueTypeScalar(value->returnType.type.storageType))
            {
                recordError(ctx, "post decrement operator may only be used on scalar types!");
                return NULL_EXPR;
            }
            auto decCtx = new NativeDecrementContext(value.release(), false);
            initDoc(decCtx, ctx);
            popDoc(decCtx);
            RETURN_EXPR(decCtx);
        }

        std::any visitNot(braneParser::NotContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->value, "expression expected");
            auto value = asExpr(visit(ctx->value));
            if(value->returnType.type.storageType == ValueType::Bool)
                    RETURN_EXPR(new NativeNotContext(value.release()));
            std::string error;
            if(auto callCtx = resolveOperator("!", error, std::move(value)))
                RETURN_EXPR(callCtx);
            recordError(ctx, error);
            return NULL_EXPR;
        }

        std::any visitLogic(braneParser::LogicContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->left, "expression expected");
            EXPR_ASSERT_EXISTS(ctx->opr, "operator expected");
            EXPR_ASSERT_EXISTS(ctx->right, "expression expected");

            auto left = asExpr(visit(ctx->left));
            auto right = asExpr(visit(ctx->right));
            if(left->returnType.type.storageType == ValueType::Bool && right->returnType.type.storageType == ValueType::Bool)
            {
                    std::string op = ctx->opr->getText();
                    RETURN_EXPR(new NativeLogicContext((op == "||") ? NativeLogicContext::OR : NativeLogicContext::AND,
                                                       left.release(),
                                                       right.release()));
            }

            std::string error;
            if(auto callCtx = resolveOperator("&&", error, std::move(left), std::move(right)))
                RETURN_EXPR(callCtx);
            recordError(ctx->opr, error);
            return NULL_EXPR;
        }

        std::any visitComparison(braneParser::ComparisonContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->left, "expression expected");
            EXPR_ASSERT_EXISTS(ctx->opr, "operator expected");
            EXPR_ASSERT_EXISTS(ctx->right, "expression expected");
            std::is_same<int, int>::value;

            auto left = asExpr(visit(ctx->left));
            auto right = asExpr(visit(ctx->right));
            std::string opText = ctx->opr->getText();

            if(NativeCompareContext::validCompare(left->returnType, right->returnType))
            {
                NativeCompareContext::Operation op;
                if(opText == "==")
                    op = NativeCompareContext::EQ;
                else if(opText == "!=")
                    op = NativeCompareContext::NEQ;
                else if(opText == ">")
                    op = NativeCompareContext::GT;
                else if(opText == ">=")
                    op = NativeCompareContext::GE;
                else if(opText == "<")
                    op = NativeCompareContext::LT;
                else if(opText == "<=")
                    op = NativeCompareContext::LE;
                else
                    assert(false);

                std::string err;
                if(castSame(left, right, err))
                    RETURN_EXPR(new NativeCompareContext(op, left.release(), right.release()));
            }

            std::string error;
            if(auto callCtx = resolveOperator(opText, error, std::move(left), std::move(right)))
                RETURN_EXPR(callCtx);
            recordError(ctx->opr, error);
            return NULL_EXPR;
        }

        std::any visitIndexAccess(braneParser::IndexAccessContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->base, "expression expected");
            EXPR_ASSERT_EXISTS(ctx->arg, "expression expected");
            auto left = asExpr(visit(ctx->base));
            auto right = asExpr(visit(ctx->arg));

            if(left->returnType.arraySize > 0)
            {
                if(!isValueTypeInt(right->returnType.type.storageType))
                {
                    recordError(ctx, "Array index must be an integer!");
                    return NULL_EXPR;
                }
                RETURN_EXPR(new NativeIndexOperator(left.release(), right.release()));
            }

            std::string error;
            if(auto callCtx = resolveOperator("[]", error, std::move(left), std::move(right)))
                RETURN_EXPR(callCtx);

            recordError(ctx, error);
            return NULL_EXPR;


        }

        std::any visitId(braneParser::IdContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->scopedID(), "identifier expected");
            auto node = getIdentifierContext(ctx->scopedID());

            if(!node)
            {
                std::string id = idToString(ctx->scopedID());
                recordError(ctx, "\"" + id + "\" is not an identifier!");
                return NULL_EXPR;
            }

            if(auto constCtx = node->as<ConstValueContext>())
                RETURN_EXPR(constCtx);

            if(auto valueContext = node->as<LabeledValueContext>())
            {
                // Do we implicitly need to reference this?
                if(auto parentStruct= node->parent->as<StructContext>())
                {
                    bool parentContainsMember = false;
                    for(auto& m : parentStruct->variables)
                    {
                        if(m->identifier.text == valueContext->identifier.text)
                        {
                            parentContainsMember = true;
                            break;
                        }
                    }

                    if(parentContainsMember)
                    {
                        auto memberAccess = new MemberAccessContext{};
                        initDoc(memberAccess, ctx);
                        auto thisRef =
                            lastNode()->findIdentifier("this", IDSearchOptions_ParentsOnly)->as<LabeledValueContext>();
                        assert(thisRef);
                        auto valueAccess = new LabeledValueReferenceContext{*thisRef};
                        initDoc(valueAccess, ctx);

                        memberAccess->baseExpression.reset(valueAccess);
                        memberAccess->member = 0;
                        for(auto& m : parentStruct->variables)
                        {
                            if(m->identifier.text == valueContext->identifier.text)
                                break;
                            memberAccess->member++;
                        }

                        assert(memberAccess->member < parentStruct->variables.size());
                        memberAccess->returnType = *parentStruct->variables[memberAccess->member];

                        popDoc(valueAccess);
                        popDoc(memberAccess);
                        RETURN_EXPR(memberAccess);
                    }
                }

                auto valueAccess = new LabeledValueReferenceContext{*node->as<LabeledValueContext>()};
                initDoc(valueAccess, ctx);
                popDoc(valueAccess);
                RETURN_EXPR(valueAccess);
            }

            if(auto overrides = dynamic_cast<FunctionOverridesContext*>(node))
                RETURN_EXPR(overrides);

            recordError(ctx, ctx->getText() + " is not an identifier!");
            return NULL_EXPR;
        }

        std::any visitMemberAccess(braneParser::MemberAccessContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->base, "expected base expression");
            EXPR_ASSERT_EXISTS(ctx->member, "expected member identifier");

            auto base = asExpr(visit(ctx->base));
            if(base->returnType.type.storageType != ValueType::Struct)
            {
                recordError(ctx, "Cannot access members of non-struct objects");
                return NULL_EXPR;
            }

            auto& typeName = base->returnType.type.identifier;
            auto type = base->returnType.type.structCtx;
            assert(type);

            std::string memberName = safeGetText(ctx->member);
            auto memberOverrides = getFunctionOverrides(memberName, type, ctx->template_);
            if(!memberOverrides->overrides.empty())
            {
                memberOverrides->thisRef = std::move(base);
                RETURN_EXPR(memberOverrides);
            }
            delete memberOverrides;

            auto accessCtx = new MemberAccessContext{};
            accessCtx->baseExpression = std::move(base);
            initDoc(accessCtx, ctx);

            accessCtx->member = 0;
            for(auto& m : type->variables)
            {
                if(m->identifier.text == memberName)
                    break;
                accessCtx->member++;
            }
            if(accessCtx->member == type->variables.size())
                recordError(ctx->member, "Struct " + typeName + " does not have a member " + memberName);
            else
            {
                accessCtx->returnType = *type->variables[accessCtx->member];
                accessCtx->returnType.isRef |= accessCtx->returnType.type.storageType == ValueType::Struct;
            }

            popDoc(accessCtx);
            RETURN_EXPR(accessCtx);
        }

        std::vector<std::unique_ptr<ExpressionContext>> populateFunctionArgs(braneParser::ArgumentPackContext* ctx)
        {
            std::vector<std::unique_ptr<ExpressionContext>> args;
            for(auto& item : ctx->argumentPackItem())
            {
                if(item->expr)
                {
                    args.push_back(asExpr(visit(item->expr)));
                    continue;
                }

                if(!_instantiatingTemplate)
                {
                    recordError(item, "argument packs may only be used in templates!");
                    continue;
                }

                std::string packID = safeGetText(item->packID);
                auto* argPack = dynamic_cast<ArgPackInstanceContext*>(lastNode()->findIdentifier(packID));
                if(!argPack)
                {
                    recordError(item->packID, packID + " is not an argument pack!");
                    continue;
                }

                for(auto& value : argPack->values)
                    args.emplace_back(new LabeledValueReferenceContext{*value});
            }
            return args;
        }

        std::any visitFunctionCall(braneParser::FunctionCallContext* ctx) override
        {
            EXPR_ASSERT_EXISTS(ctx->expression(), "expected expression");
            auto callCtx = new FunctionCallContext{};
            initDoc(callCtx, ctx);
            callCtx->arguments = populateFunctionArgs(ctx->argumentPack());

            auto source = asExpr(visit(ctx->expression()));
            auto overridesCtx = source->as<FunctionOverridesContext>();
            std::string id;

            if(source->returnType.type.structCtx && source->returnType.type.storageType == ValueType::Struct)
            {
                overridesCtx = getFunctionOverrides("opr ()", source->returnType.type.structCtx, nullptr);
                if(!overridesCtx)
                {
                    recordError(ctx->expression(), "type " + source->returnType.type.identifier + " is not callable");
                    popDoc(callCtx);
                    RETURN_EXPR(callCtx);
                }
                id = source->longId();
                callCtx->arguments.emplace(callCtx->arguments.begin(), source.release());
            }

            if(overridesCtx)
            {
                bool isMember = false;
                if(overridesCtx->thisRef)
                {
                    callCtx->arguments.insert(callCtx->arguments.begin(), std::move(overridesCtx->thisRef));
                    isMember = true;
                }
                std::vector<ValueContext> argTypes;
                for(auto& arg : callCtx->arguments)
                    argTypes.push_back(arg->returnType);
                callCtx->function = bestOverride(
                    argTypes,
                    [this](const ValueContext& a, const ValueContext& b) { return implicitCastViable(a, b); },
                    overridesCtx,
                    _enforceConstexpr ? FunctionOverridesContext::Constexpr : FunctionOverridesContext::None);

                if(!callCtx->function && !isMember && getLast<StructContext>())
                {
                    auto& structCtx = *getLast<StructContext>();
                    auto* thisRef =
                        lastNode()->findIdentifier("this", IDSearchOptions_ParentsOnly)->as<LabeledValueContext>();
                    auto valueAccess = new LabeledValueReferenceContext{*thisRef};
                    initDoc(valueAccess, ctx);
                    popDoc(valueAccess);

                    callCtx->arguments.emplace(callCtx->arguments.begin(), valueAccess);
                    argTypes.emplace(argTypes.begin(), valueAccess->returnType);

                    callCtx->function = bestOverride(
                        argTypes,
                        [this](const ValueContext& a, const ValueContext& b) { return implicitCastViable(a, b); },
                        overridesCtx,
                        _enforceConstexpr ? FunctionOverridesContext::Constexpr : FunctionOverridesContext::None);
                }

                if(!callCtx->function)
                {
                    if(source)
                        id = source->longId();
                    std::string error = "Could not find override of function \"" + id;
                    error += "\" with arguments: ";
                    for(auto& arg : argTypes)
                        error += arg.signature() + ", ";
                    if(!overridesCtx->overrides.empty())
                    {
                        error += "\nDid you mean to use one of these overrides?\n-----\n";
                        for(auto o : overridesCtx->overrides)
                            error += functionSig(o->longId(), o->arguments) + "\n";
                        error += "-----";
                        if(_enforceConstexpr)
                            error += "\nVerify the functions you call in a constexpr scope are also constexpr.";
                    }
                    recordError(ctx->expression(), error);
                    popDoc(callCtx);
                    RETURN_EXPR(callCtx);
                }

                callCtx->returnType = callCtx->function->returnType;

                for(int i = 0; i < callCtx->arguments.size(); ++i)
                {
                    if(callCtx->arguments[i]->returnType.sameBaseType(*callCtx->function->arguments[i]))
                        continue;

                    // Find the cast operator, should never return false ideally, but I can see configuration bugs
                    // triggering this in the future.
                    std::string error;
                    auto* arg = callCtx->arguments[i].release();
                    if(auto* castCtx = resolveCast(arg, *callCtx->function->arguments[i], error))
                        callCtx->arguments[i].reset(castCtx);
                    else
                    {
                        callCtx->arguments[i].reset(arg);
                        recordError(callCtx->arguments[i]->range, error);
                        popDoc(callCtx);
                        RETURN_EXPR(callCtx);
                    }
                }

                popDoc(callCtx);
                RETURN_EXPR(callCtx);
            }

            if(source->returnType.type.storageType != ValueType::FuncRef)
            {
                recordError(ctx->expression(), "expected function or function reference");
                popDoc(callCtx);
                RETURN_EXPR(callCtx);
            }

            const auto& ptrArgs = _funcRefArgTypes.at(source->returnType.type.identifier);
            if(ptrArgs.size() != callCtx->arguments.size() + 1)
            {
                recordError(ctx->argumentPack(), "expected " + std::to_string(ptrArgs.size() - 1) + " arguments");
                popDoc(callCtx);
                RETURN_EXPR(callCtx);
            }
            for(int i = 0; i < callCtx->arguments.size(); ++i)
            {
                if(callCtx->arguments[i]->returnType.sameBaseType(ptrArgs[i + 1]))
                    continue;
                std::string error;
                auto castCtx = resolveCast(callCtx->arguments[i].get(), ptrArgs[i + 1], error);
                if(!castCtx)
                {
                    recordError(callCtx->arguments[i]->range, error);
                    popDoc(callCtx);
                    RETURN_EXPR(callCtx);
                }
                callCtx->arguments[i].release();
                callCtx->arguments[i].reset(castCtx);
            }

            callCtx->returnType = ptrArgs[0];
            callCtx->functionRef = std::move(source);

            popDoc(callCtx);
            RETURN_EXPR(callCtx);
        }

        std::any visitModules(braneParser::ModulesContext* ctx) override
        {
            for(auto* module : ctx->module())
                visit(module);
            return {};
        }

        void recordError(TextRange range, const std::string& message) { _result.errors.push_back({range, message}); }

        void recordError(antlr4::Token* token, const std::string& message) { recordError(toRange(token), message); }

        void recordError(antlr4::ParserRuleContext* ctx, const std::string& message)
        {
            recordError(toRange(ctx), message);
        }

        DocumentContext* lastNode() { return _documentContext.top(); }

        template<typename T>
        T* getLast()
        {
            if(lastNode()->is<T>())
                return lastNode()->as<T>();
            return lastNode()->getParent<T>();
        }

        bool analyze()
        {
            _result.scriptContext->range = {0, _result.document.size()};
            _result.version++;
            _result.errors.clear();
            _result.warnings.clear();

            auto& script = *_result.scriptContext;

            _result.parseCtx.inputStream = std::make_unique<antlr4::ANTLRInputStream>(_result.document);

            _result.parseCtx.lexer = std::make_unique<braneLexer>(_result.parseCtx.inputStream.get());
            _result.parseCtx.lexer->removeErrorListeners();

            _result.parseCtx.lexErrorListener = std::make_unique<BraneErrorListener>();
            auto* lexErrorListener = dynamic_cast<BraneErrorListener*>(_result.parseCtx.lexErrorListener.get());
            lexErrorListener->setAnalyzer(this);
            _result.parseCtx.lexer->addErrorListener(_result.parseCtx.lexErrorListener.get());


            _result.parseCtx.tokenStream = std::make_unique<antlr4::CommonTokenStream>(_result.parseCtx.lexer.get());
            _result.parseCtx.parser = std::make_unique<braneParser>(_result.parseCtx.tokenStream.get());
            _result.parseCtx.parser->removeErrorListeners();

            _result.parseCtx.parseErrorListener = std::make_unique<BraneErrorListener>();
            auto* parseErrorListener = dynamic_cast<BraneErrorListener*>(_result.parseCtx.parseErrorListener.get());
            parseErrorListener->setAnalyzer(this);
            _result.parseCtx.parser->addErrorListener(_result.parseCtx.parseErrorListener.get());

            _documentContext.push(_result.scriptContext.get());
            auto* modules = _result.parseCtx.parser->modules();
            visit(modules);
            _documentContext.pop();

            // Trim modules that were no longer defined this pass
            for(auto mod = _result.scriptContext->modules.begin(); mod != _result.scriptContext->modules.end();)
            {
                if(mod->second->version != _result.version)
                {
                    _analyzer.deregisterModule(mod->second.get());
                    mod = _result.scriptContext->modules.erase(mod);
                }
                else
                    mod++;
            }

            lexErrorListener->setAnalyzer(nullptr);
            parseErrorListener->setAnalyzer(nullptr);

            return _result.errors.empty();
        }
    };

    Parser::Parser() { addWorkspace("include", true); }

    bool Parser::isLoaded(const std::string& path) { return _analyzationContexts.contains(path); }

    std::string readDocument(const std::string& path)
    {
        std::ifstream f(path, std::ios::ate | std::ios::binary);
        if(!f.is_open())
            throw std::runtime_error("Could not load " + path + "!");
        std::string document;
        document.resize(f.tellg());
        f.seekg(0);
        f.read(document.data(), document.size());
        f.close();
        return std::move(document);
    }

    void Parser::load(const std::string& path, bool cacheDocument, bool allowUnsafe)
    {
        load(path, readDocument(path), cacheDocument, allowUnsafe);
    }

    void Parser::load(const std::string& path, std::string document, bool cacheDocument, bool allowUnsafe)
    {
        if(!_analyzationContexts.contains(path))
        {
            _analyzationContexts.insert({path, std::make_unique<ParserContext>()});
            _analyzationContexts.at(path)->source = path;
        }
        auto& context = _analyzationContexts.at(path);
        std::scoped_lock lock(context->lock);
        context->document = std::move(document);
        if(!context->scriptContext)
            context->scriptContext = std::make_unique<ScriptContext>();
        context->complete = false;
        context->scriptContext->source = path;

        AnalyzerCore(*this, *context, true, allowUnsafe).analyze();

        if(!cacheDocument)
            context->document.clear();
    }

    void Parser::reload(const std::string& path)
    {
        assert(_analyzationContexts.contains(path));
        auto& context = _analyzationContexts.at(path);
        std::scoped_lock lock(context->lock);

        bool cacheDocument = !context->document.empty();
        if(!cacheDocument)
            context->document = readDocument(path);

        AnalyzerCore(*this, *context, true, false).analyze();
        context->complete = false;

        if(!cacheDocument)
            context->document.clear();
    }

    bool Parser::validate(const std::string& path, bool allowUnsafe)
    {
        assert(_analyzationContexts.contains(path));
        auto& context = _analyzationContexts.at(path);
        std::scoped_lock lock(context->lock);
        if(!context->errors.empty())
            return false;
        AnalyzerCore(*this, *context, false, allowUnsafe).analyze();
        context->complete = true;
        return context->errors.empty();
    }

    Parser::ParserContext* Parser::getCtx(const std::string& path)
    {
        return _analyzationContexts.at(path).get();
    }

    void Parser::addWorkspace(const std::string& path, bool allowUnsafe)
    {
        std::cout << "Loading workspace " << path << std::endl;
        assert(std::filesystem::exists(path));
        for(auto& entry : std::filesystem::recursive_directory_iterator(
                path, std::filesystem::directory_options::skip_permission_denied))
        {
            assert(std::filesystem::exists(entry));
            if(!entry.is_regular_file())
                continue;
            if(entry.path().extension() != ".bs")
                continue;
            load(entry.path().generic_string(), false, allowUnsafe);
            auto& context = _analyzationContexts.at(entry.path().generic_string());
            if(!context->errors.empty())
            {
                std::cout << "Errors in loaded file " << entry.path().generic_string() << std::endl;
                for(auto& err : context->errors)
                    std::cout << "(" << err.range.start.line << ", " << err.range.start.charPos << ") " << err.message
                              << std::endl;
            }
        }
        _workspaceRoots.push_back(path);
    }

    bool Parser::registerModule(ModuleContext* lib)
    {
        if(_modules.contains(lib->identifier.text))
            return false;
        _modules[lib->identifier.text] = lib;
        return true;
    }

    void Parser::deregisterModule(ModuleContext* lib)
    {
        auto module = _modules.find(lib->identifier.text);
        assert(module != _modules.end());
        _modules.erase(module);
    }

    ModuleContext* Parser::getModule(const std::string& id) const
    {
        auto modCtx = _modules.find(id);
        if(modCtx == _modules.end())
            return nullptr;
        return modCtx->second;
    }

    void Parser::setConstexprEvaluator(ConstexprEvaluator* evaluator) { _evaluator = evaluator; }

    ConstexprEvaluator* Parser::constexprEvaluator() const { return _evaluator; }

    bool Parser::isValid(const std::string& path)
    {
        if(!_analyzationContexts.contains(path))
            return false;
        return _analyzationContexts.at(path)->errors.empty();
    }

    IRScript Parser::compile(const std::string& path, uint8_t flags)
    {
        assert(isValid(path));
        assert(_analyzationContexts.at(path)->complete); // validate() must be called before compile()
        llvm::LLVMContext llvmContext;
        return _analyzationContexts.at(path)->scriptContext->compile(&llvmContext, flags);
    }

    Parser::~Parser() = default;


} // namespace BraneScript