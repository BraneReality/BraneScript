#pragma once
#include "enums/result.h"
#include "ir.h"

namespace BraneScript::IRSerializer
{
    Result<std::string> irToText(const IRModule& module);
    Result<IRModule> textToIR(std::string_view text);
} // namespace BraneScript::IRSerializer
