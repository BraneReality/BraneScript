#ifndef BRANESCRIPT_DOCUMENTCONTEXT_H
#define BRANESCRIPT_DOCUMENTCONTEXT_H

#include <filesystem>
#include <memory>
#include <string>
#include <variant>
#include <vector>
#include "../types/valueType.h"
#include "enums/option.h"
#include <tree_sitter/api.h>
#include <unordered_map>

namespace BraneScript
{
    template<class T>
    using Node = std::shared_ptr<T>;
    template<typename T>
    using LabeledNodeMap = std::unordered_map<std::string, Node<T>>;
    template<typename T>
    using NodeList = std::vector<Node<T>>;


    struct TextContext;
    struct ValueContext;
    struct ConstValueContext;
    struct Identifier;
    struct ScopedIdentifier;
    struct RefTypeContext;
    struct StructContext;
    struct BaseTypeContext;
    struct TypeContext;
    struct BinaryOperatorContext;
    struct VariableDefinitionContext;
    struct AssignmentContext;
    struct AnonStructContext;
    struct AnonStructTypeContext;
    struct CallContext;
    struct CallSigContext;
    struct ScopeContext;
    struct PipelineStageContext;
    struct MemberInitContext;
    struct FunctionContext;
    struct PipelineContext;
    struct ModuleContext;
    struct DocumentContext;
    using TextContextNode = std::variant<Node<ValueContext>,
                                         Node<ConstValueContext>,
                                         Node<Identifier>,
                                         Node<ScopedIdentifier>,
                                         Node<TypeContext>,
                                         Node<BinaryOperatorContext>,
                                         Node<VariableDefinitionContext>,
                                         Node<AssignmentContext>,
                                         Node<AnonStructContext>,
                                         Node<AnonStructTypeContext>,
                                         Node<CallContext>,
                                         Node<CallSigContext>,
                                         Node<ScopeContext>,
                                         Node<PipelineStageContext>,
                                         Node<MemberInitContext>,
                                         Node<FunctionContext>,
                                         Node<PipelineContext>,
                                         Node<StructContext>,
                                         Node<ModuleContext>,
                                         Node<DocumentContext>>;

    struct TextSource
    {
        std::string uri;
    };

    struct TextContext : public std::enable_shared_from_this<TextContext>
    {
        TSRange range;
        Node<TextSource> source;
        Option<std::weak_ptr<TextContext>> parent;

        virtual ~TextContext() = default;
        virtual Option<TextContextNode> getNodeAtChar(TSPoint pos);

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier);
        /// Searches up the context tree until we find an identifier that matches the first scope of the identifier,
        /// Then attempts to search downwards through matching nodes until the full path matches.
        virtual Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope);
        virtual std::string longId() const;

        template<typename T>
        Option<Node<T>> as()
        {
            static_assert(std::is_base_of<TextContext, T>::value, "T must be a subclass of DocumentContext");
            auto r = std::dynamic_pointer_cast<T>(shared_from_this());
            if(!r)
                return None();
            return Some(r);
        }

        template<typename T>
        Option<Node<T>> as() const
        {
            static_assert(std::is_base_of<TextContext, T>::value, "T must be a subclass of DocumentContext");
            auto r = std::dynamic_pointer_cast<T>(shared_from_this());
            if(!r)
                return None();
            return Some(r);
        }

        template<typename T>
        bool is()
        {
            return std::dynamic_pointer_cast<T>(shared_from_this());
        }

        template<typename T>
        Option<Node<T>> getParent() const
        {
            if(!parent)
                return None();
            if(auto p = parent.value().lock())
            {
                auto pt = p->as<T>();
                if(pt)
                    return pt;
                return p->getParent<T>();
            }
            return None();
        }

        template<typename T>
        Node<T> getLast()
        {
            auto o = as<T>();
            if(o)
                return o;
            return getParent<T>();
        }

        template<typename T>
        Node<T> getLast() const
        {
            auto o = as<T>();
            if(o)
                return o;
            return getParent<T>();
        }
    };

    struct Identifier : public TextContext
    {
        std::string text;
        operator std::string&();

        inline bool operator==(const Identifier& o) const { return o.text == text; }

        inline bool operator!=(const Identifier& o) const { return o.text != text; }
    };

    enum class TypeModifiers
    {
        MutRef,
        ConstRef,
    };

    struct TypeContext : public TextContext
    {
        Node<ScopedIdentifier> baseType;
        std::vector<TypeModifiers> modifiers;
    };

    struct ValueContext : public TextContext
    {
        // What data does this value store
        Option<Node<Identifier>> label;
        Option<Node<TypeContext>> type;

        // Is stored on the heap or the stack, instead of being a temporary value holder
        bool isLValue = false;
        bool isMut = false;

        /*bool operator==(const ValueContext& o) const;*/
        /*bool operator!=(const ValueContext& o) const;*/
        /**/
        /*// SameBaseType compares everything but isConst and isLValue*/
        /*bool sameBaseType(const ValueContext& o) const;*/
        /**/
        /*uint32_t castCost(const ValueContext& target) const;*/

        ValueContext() = default;
        /*ValueContext(TypeContext type, bool isLValue, bool isConst, bool isRef);*/
        /*ValueContext(std::string label, TypeContext type, bool isLValue, bool isConst, bool isRef);*/
        /**/
        std::string longId() const override;
    };

    struct TemplateDefArgumentContext : public TextContext
    {
        std::string identifier;

        enum ArgType
        {
            Typedef,
            TypedefPack,
            Value
        } type = Typedef;

        Node<TypeContext> valueType;
    };

    struct ConstValueContext;

    struct TemplateArgContext : public TextContext
    {
        std::string identifier;
        std::variant<ValueContext, std::vector<ValueContext>, Node<ConstValueContext>> value;
    };

    using ScopeSegment = std::variant<Node<Identifier>>;

    struct ScopedIdentifier : public TextContext
    {
        std::vector<ScopeSegment> scopes;
        std::string longId() const override;
    };

    struct ErrorContext
    {
        std::string message;
    };

    struct ExpressionContext : public TextContext
    {
        ValueContext returnType;
        // Is the result of this expression a constant?
    };

    struct ExpressionErrorContext;
    struct ScopeContext;
    struct IfContext;
    struct WhileContext;
    struct ForContext;
    struct AssignmentContext;
    struct ConstValueContext;
    struct LabeledValueReferenceContext;
    struct MemberAccessContext;
    struct CreateReferenceContext;
    struct DereferenceContext;
    struct UnaryOperatorContext;
    struct BinaryOperatorContext;
    struct AnonStructContext;
    struct CallContext;

    using ExpressionContextNode = std::variant<Node<ExpressionErrorContext>,
                                               Node<ScopeContext>,
                                               Node<IfContext>,
                                               Node<WhileContext>,
                                               Node<ForContext>,
                                               Node<AssignmentContext>,
                                               Node<VariableDefinitionContext>,
                                               Node<ConstValueContext>,
                                               Node<ScopedIdentifier>,
                                               Node<MemberAccessContext>,
                                               Node<CreateReferenceContext>,
                                               Node<DereferenceContext>,
                                               Node<UnaryOperatorContext>,
                                               Node<BinaryOperatorContext>,
                                               Node<AnonStructContext>,
                                               Node<CallContext>>;

    struct ExpressionErrorContext : public ExpressionContext, ErrorContext
    {
        inline ExpressionErrorContext(std::string message, TSRange range)
        {
            this->message = std::move(message);
            this->range = range;
        }
    };

    struct VariableDefinitionContext : public ExpressionContext
    {
        Node<ValueContext> definedValue;
    };

    struct ScopeContext : public ExpressionContext
    {
        // This must be a list so that we can keep construction/destruction order consistent
        std::vector<Node<ValueContext>> localVariables;
        std::vector<ExpressionContextNode> expressions;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
    };

    struct CallSigContext : public TextContext
    {
        Node<AnonStructTypeContext> input;
        Node<AnonStructTypeContext> output;
        std::string longId() const override;
    };

    struct PipelineStageContext : public TextContext
    {
        Option<Node<Identifier>> identifier;
        Node<CallSigContext> callSig;
        Node<ScopeContext> body;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
    };

    struct IfContext : public ExpressionContext
    {
        Node<ScopeContext> branchScope;
        ExpressionContextNode condition;
        ExpressionContextNode body;
        ExpressionContextNode elseBody;
    };

    struct WhileContext : public ExpressionContext
    {
        Node<ScopeContext> loopScope;
        ExpressionContextNode condition;
        ExpressionContextNode body;
    };

    struct ForContext : public ExpressionContext
    {
        Node<ScopeContext> loopScope;
        ExpressionContextNode init;
        ExpressionContextNode condition;
        ExpressionContextNode step;
        ExpressionContextNode body;
    };

    struct AssignmentContext : public ExpressionContext
    {
        ExpressionContextNode lValue;
        ExpressionContextNode rValue;

        AssignmentContext() = default;
        AssignmentContext(ExpressionContext* lValue, ExpressionContext* rValue);
        void setArgs(ExpressionContext* lValue, ExpressionContext* rValue);
    };

    struct ConstValueContext : public ExpressionContext
    {
        std::variant<bool, char, int64_t, uint64_t, double, std::string> value;
    };

    struct LabeledValueReferenceContext : public ExpressionContext
    {
        std::string identifier;
        LabeledValueReferenceContext(const ValueContext& value);
    };

    struct MemberAccessContext : public ExpressionContext
    {
        ExpressionContextNode baseExpression;
        size_t member = -1;
        MemberAccessContext() = default;
        MemberAccessContext(ExpressionContext* base, StructContext* baseType, size_t member);
    };

    struct CreateReferenceContext : public ExpressionContext
    {
        ExpressionContextNode _source;
        CreateReferenceContext(ExpressionContext* source);
    };

    struct DereferenceContext : public ExpressionContext
    {
        ExpressionContextNode _source;
        DereferenceContext(ExpressionContext* source);
    };

    enum class UnaryOperator : uint8_t
    {
        Deref = 0,
        Ref,
        Negate,
        LogicNot,
        BitwiseNot,
    };

    struct UnaryOperatorContext : public ExpressionContext
    {
        UnaryOperator opType;
        ExpressionContextNode arg;
    };

    enum class BinaryOperator : uint8_t
    {
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
    };

    struct BinaryOperatorContext : public ExpressionContext
    {
        BinaryOperator opType;
        ExpressionContextNode left;
        ExpressionContextNode right;
    };

    struct AnonStructTypeContext : public TextContext
    {
        NodeList<ValueContext> members;
    };

    struct MemberInitContext : public TextContext
    {
        Node<Identifier> id;
        ExpressionContextNode expression;
    };

    struct AnonStructContext : public ExpressionContext
    {
        NodeList<MemberInitContext> members;
    };

    struct CallContext : public ExpressionContext
    {
        ExpressionContextNode callable;
        Node<AnonStructContext> args;
    };

    struct FunctionContext : public TextContext
    {
        Node<Identifier> identifier;
        Node<CallSigContext> callSig;

        Node<ScopeContext> body;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
        std::string longId() const override;
    };

    struct TraitContext : public TextContext
    {
        Node<Identifier> identifier;
        LabeledNodeMap<CallSigContext> methods;
        std::string longId() const override;
    };

    struct ImplContext : public TextContext
    {
        Option<Node<Identifier>> trait;
        Node<TypeContext> type;
        LabeledNodeMap<FunctionContext> methods;
        std::string longId() const override;
    };

    struct PipelineContext : public TextContext
    {
        Node<Identifier> identifier;
        // Arguments
        Node<CallSigContext> callSig;
        NodeList<PipelineStageContext> stages;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
        std::string longId() const override;
        std::string argSig() const;
        std::string signature() const;
    };

    struct StructContext : public TextContext
    {
        Node<Identifier> identifier;

        LabeledNodeMap<ValueContext> members;
        bool packed = false;

        Option<TextContextNode> getNodeAtChar(TSPoint pos) override;
        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
        std::string longId() const override;
    };

    struct ModuleContext : public TextContext
    {
        Node<Identifier> identifier;
        LabeledNodeMap<StructContext> structs;
        LabeledNodeMap<FunctionContext> functions;
        LabeledNodeMap<PipelineContext> pipelines;

        Option<TextContextNode> getNodeAtChar(TSPoint pos) override;

        Option<TextContextNode> searchFor(Node<ScopedIdentifier> identifier, size_t scope) override;
        std::string longId() const override;
    };

    struct DocumentContext : public TextContext
    {
        std::filesystem::path source;
        LabeledNodeMap<ModuleContext> modules;

        Option<TextContextNode> getNodeAtChar(TSPoint pos) override;
    };
} // namespace BraneScript

#endif
