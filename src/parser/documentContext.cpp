
#include "documentContext.h"
#include <cassert>

namespace BraneScript
{
    Option<TextContextNode> TextContext::getNodeAtChar(TSPoint pos) { return None(); }

    Option<TextContextNode> TextContext::findIdentifier(std::string_view identifier)
    {
        return findIdentifier(identifier, 0);
    }

    Option<TextContextNode> TextContext::findIdentifier(std::string_view identifier, uint8_t searchOptions)
    {
        return None();
    }

    std::string TextContext::longId() const { return ""; }

    std::string ValueContext::signature() const
    {
        assert(false && "Unimplemented");
        std::string sig;
        return sig;
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

    std::string CallSigContext::longId() const { return "callSigToBeImplemented"; }

    Option<TextContextNode> PipelineContext::findIdentifier(std::string_view identifier, uint8_t searchOptions)
    {
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

    Option<TextContextNode> ModuleContext::getNodeAtChar(TSPoint pos) { return None(); }

    Option<TextContextNode> ModuleContext::findIdentifier(std::string_view identifier, uint8_t searchOptions)
    {
        return None();
    }

    Option<TextContextNode> DocumentContext::getNodeAtChar(TSPoint pos) { return None(); }

    Option<TextContextNode> DocumentContext::findIdentifier(std::string_view identifier, uint8_t searchOptions)
    {
        return None();
    }

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
