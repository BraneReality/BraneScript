#pragma once
#include "backends/jitBackend.h"

namespace BraneScript
{
    class Runtime
    {
        std::shared_ptr<JitBackend> _jitBackend;

      public:
    };
} // namespace BraneScript
