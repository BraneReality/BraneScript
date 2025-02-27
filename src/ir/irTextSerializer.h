#pragma once
#include "enums/result.h"
#include "ir.h"

namespace BraneScript
{
    namespace IRSerializer
    {
        Result<std::string> irToText(const IRModule& module);
        Result<IRModule> textToIR(const& module);
    } // namespace IRSerializer
} // namespace BraneScript
