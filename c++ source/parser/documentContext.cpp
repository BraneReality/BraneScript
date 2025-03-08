#include "documentContext.h"
#include <cassert>
#include <format>
#include "enums/matchv.h"

namespace BraneScript
{
    Option<TextContextNode> TextContext::getNodeAtChar(TSPoint pos) { return None(); }

    Option<TextContextNode> TextContext::searchFor(Node<ScopedIdentifier> identifier)
    {
        return searchFor(std::move(identifier), 0);
    }

    Option<TextContextNode> TextContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(scope > 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(std::move(identifier), scope);
        return None();
    }

    std::string TextContext::longId() const { return ""; }

    std::string scopeSegementId(const ScopeSegment& segment)
    {
        return MATCHV(segment, [&](Node<Identifier> id) { return id->text; });
    }

    std::string ScopedIdentifier::longId() const
    {
        if(scopes.empty())
            return "";
        std::string id = scopeSegementId(scopes[0]);
        for(size_t i = 1; i < scopes.size(); ++i)
            id += "::" + scopeSegementId(scopes[i]);
        return id;
    }

    std::string ValueContext::longId() const
    {
        std::string id;
        if(parent)
        {
            if(auto p = parent.value().lock())
                id += p->longId() + "::";
        }
        if(label)
            id += label.value()->text;
        else
            id += "unnamed_value";
        return id;
    }

    std::string CallSigContext::longId() const
    {
        auto lid = std::format("{} -> {}", input->longId(), output->longId());
        if(auto p = getParent<TextContext>())
            lid = std::format("{}::{}", p.value()->longId(), lid);
        return lid;
    }

    Option<TextContextNode> FunctionContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
        {
            for(auto& var : callSig->input->members)
            {
                if(!var->label)
                    continue;
                if(**id == *var->label.value())
                    return Some<TextContextNode>(var);
            }
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    std::string FunctionContext::longId() const
    {
        std::string idText;
        if(parent)
        {
            if(auto p = parent.value().lock())
                idText += p->longId() + "::";
        }
        idText += identifier->text;
        return idText;
    }

    Option<TextContextNode> ScopeContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
        {
            for(auto& var : localVariables)
            {
                if(!var->label)
                    continue;
                if(**id == *var->label.value())
                    return Some<TextContextNode>(var);
            }
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    Option<TextContextNode> PipelineStageContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
        {
            for(auto& var : callSig->input->members)
            {
                if(!var->label)
                    continue;
                if(**id == *var->label.value())
                    return Some<TextContextNode>(var);
            }
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    Option<TextContextNode> PipelineContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
        {
            if(**id == *this->identifier)
                return Some<TextContextNode>(std::static_pointer_cast<PipelineContext>(shared_from_this()));
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    std::string PipelineContext::longId() const
    {
        std::string idText;
        if(parent)
        {
            if(auto p = parent.value().lock())
                idText += p->longId() + "::";
        }
        idText += identifier->text;
        return idText;
    }

    Option<TextContextNode> StructContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        if(auto id = std::get_if<Node<Identifier>>(&identifier->scopes[scope]))
        {
            if(**id == *this->identifier)
                return Some<TextContextNode>(std::static_pointer_cast<StructContext>(shared_from_this()));
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    Option<TextContextNode> ModuleContext::getNodeAtChar(TSPoint pos) { return None(); }

    Option<TextContextNode> ModuleContext::searchFor(Node<ScopedIdentifier> identifier, size_t scope)
    {
        // Eventually this will search for generics and traits as well
        if(auto res = MATCHV(identifier->scopes[scope],
                             [&](Node<Identifier>& sid) -> Option<TextContextNode>
        {
            if(*sid == *this->identifier)
            {
                if(identifier->scopes.size() == scope + 1)
                    return Some<TextContextNode>(std::static_pointer_cast<ModuleContext>(shared_from_this()));
                // If we match the current scope, but there's more, try to match the rest of the path
                scope += 1;
            }

            auto s = structs.find(sid->text);
            if(s != structs.end())
                return Some<TextContextNode>(s->second);

            auto f = functions.find(sid->text);
            if(f != functions.end())
                return Some<TextContextNode>(f->second);

            auto p = pipelines.find(sid->text);
            if(p != pipelines.end())
                return Some<TextContextNode>(f->second);

            return None();
        }))
        {
            return res;
        }

        if(scope != 0)
            return None();
        if(auto p = getParent<TextContext>())
            return p.value()->searchFor(identifier, scope);
        return None();
    }

    Option<TextContextNode> DocumentContext::getNodeAtChar(TSPoint pos) { return None(); }

    std::string ModuleContext::longId() const
    {
        std::string id;
        if(parent)
        {
            if(auto p = parent.value().lock())
                id += p->longId() + "::";
        }
        return id + identifier->text;
    }
} // namespace BraneScript
