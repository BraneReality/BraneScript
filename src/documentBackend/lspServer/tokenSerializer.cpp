//
// Created by wirewhiz on 1/31/23.
//

#include "tokenSerializer.h"
#include "antlr4/braneBaseVisitor.h"
#include "antlr4/braneLexer.h"

namespace lsp
{
    class BraneTokenSerializer : braneBaseVisitor
    {
        enum class TokenType : uint32_t
        {
            Type = 0,
            Variable = 1,
            Function = 2,
            Number = 3,
            String = 4,
        };
        enum TokenModifiers : uint32_t
        {
            TokenModifiers_None = 0,
            TokenModifiers_Definition = 1,
            TokenModifiers_Declaration = 1 << 1,
            TokenModifiers_Deprecated = 1 << 2
        };
        uint32_t _lastLine = 0;
        uint32_t _lastCharacter = 0;
        void appendToken(uint32_t line,
                         uint32_t character,
                         uint32_t length,
                         TokenType type,
                         TokenModifiers modifiers = TokenModifiers_None)
        {
            assert(line >= _lastLine);
            tokens.reserve(tokens.size() + 5);
            tokens.push_back(line - _lastLine);
            if(line != _lastLine)
                _lastCharacter = 0;
            assert(character >= _lastCharacter);
            tokens.push_back(character - _lastCharacter);
            tokens.push_back(length);
            tokens.push_back((uint32_t)type);
            tokens.push_back((uint32_t)modifiers);

            _lastLine = line;
            _lastCharacter = character;
        }

        void appendToken(antlr4::Token* token, TokenType type, TokenModifiers modifiers = TokenModifiers_None)
        {
            if(!token)
                return;
            appendToken(token->getLine() - 1,
                        token->getCharPositionInLine(),
                        token->getStopIndex() - token->getStartIndex() + 1,
                        type,
                        modifiers);
        }
        std::vector<uint32_t> tokens;

      public:

        std::any visitScopedID(braneParser::ScopedIDContext* ctx) override
        {
            if(ctx->child)
            {
                appendToken(ctx->id, TokenType::Type);
                if(ctx->template_)
                    visit(ctx->template_);
                return visitScopedID(ctx->child);
            }
            return ctx;
        }

        std::any visitType(braneParser::TypeContext* ctx) override
        {
            appendToken(ctx->name->id, TokenType::Type);
            if(ctx->name->template_)
                visit(ctx->name->template_);
            visitScopedID(ctx->name);
            return {};
        }

        std::any visitDeclaration(braneParser::DeclarationContext* ctx) override
        {
            visitChildren(ctx);
            appendToken(ctx->id, TokenType::Variable, TokenModifiers_Declaration);
            return {};
        }

        std::any visitGlobal(braneParser::GlobalContext *ctx) override {
            visitChildren(ctx);
            appendToken(ctx->id, TokenType::Variable, TokenModifiers_Declaration);
            return {};
        }

        std::any visitStructDef(braneParser::StructDefContext* ctx) override
        {
            if(ctx->template_)
                visit(ctx->template_);
            appendToken(ctx->id, TokenType::Type);
            for(auto m : ctx->structMember())
                visit(m);
            return {};
        }

        std::any visitConstString(braneParser::ConstStringContext* ctx) override
        {
            appendToken(ctx->getStart(), TokenType::String);
            return visitChildren(ctx);
        }

        std::any visitMemberAccess(braneParser::MemberAccessContext *ctx) override {
            visit(ctx->base);
            appendToken(ctx->member, TokenType::Variable);
            return {};
        }

        std::any visitFunctionCall(braneParser::FunctionCallContext* ctx) override
        {
            visit(ctx->overrides);
            visit(ctx->argumentPack());
            return {};
        }

        std::any visitConstFloat(braneParser::ConstFloatContext* ctx) override
        {
            appendToken(ctx->getStart(), TokenType::Number);
            return visitChildren(ctx);
        }

        std::any visitId(braneParser::IdContext* ctx) override
        {
            appendToken(ctx->getStart(), TokenType::Variable);
            return visitChildren(ctx);
        }

        std::any visitConstChar(braneParser::ConstCharContext* ctx) override
        {
            appendToken(ctx->getStart(), TokenType::String);
            return visitChildren(ctx);
        }

        std::any visitFunction(braneParser::FunctionContext* ctx) override
        {
            visit(ctx->sig);
            appendToken(ctx->sig->id, TokenType::Function, TokenModifiers_Definition);
            visit(ctx->arguments);
            for(auto& stmt : ctx->statement())
                visit(stmt);
            return {};
        }

        std::any visitTemplateDefArgument(braneParser::TemplateDefArgumentContext* ctx) override
        {
            if(ctx->isTypedef)
                appendToken(ctx->id, TokenType::Type);
            else
                visitType(ctx->exprType);
            return {};
        }

        std::vector<uint32_t> extractTokens(const std::string& document)
        {
            antlr4::ANTLRInputStream input(document);
            braneLexer lexer(&input);
            lexer.removeErrorListeners();
            antlr4::CommonTokenStream tokenStream(&lexer);

            braneParser parser(&tokenStream);
            parser.removeErrorListeners();
            visit(parser.modules());

            return std::move(tokens);
        }
    };

    std::vector<uint32_t> extractSemanticTokens(const std::string& document)
    {
        BraneTokenSerializer serializer;
        return std::move(serializer.extractTokens(document));
    }

    Json::Value semanticTokensOptions()
    {
        Json::Value options;
        Json::Value tokenTypes;
        tokenTypes.append("type");
        tokenTypes.append("variable");
        tokenTypes.append("function");
        tokenTypes.append("number");
        tokenTypes.append("string");
        Json::Value tokenModifiers;
        tokenModifiers.append("definition");
        tokenModifiers.append("declaration");
        tokenModifiers.append("deprecated");
        options["legend"]["tokenTypes"] = std::move(tokenTypes);
        options["legend"]["tokenModifiers"] = std::move(tokenModifiers);
        options["full"] = true;

        return options;
    }
} // namespace lsp