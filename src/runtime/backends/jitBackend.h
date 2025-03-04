#pragma once

#include "enums/result.h"
#include "ir/ir.h"

namespace BraneScript
{
    struct JitPtr
    {
        uint16_t binding;
        uint16_t index;
    };

    using JitFuncHandle = void(__cdecl*)(void*, JitPtr, JitPtr);

    struct JitStructType;
    using JitType = std::variant<IRNativeType, std::shared_ptr<JitStructType>>;

    struct JitStructMember
    {
        Option<std::string> label;
        JitType type;
        uint16_t offset;
    };

    struct JitStructType
    {
        std::vector<JitStructMember> members;
    };

    struct JitFunction
    {
        std::shared_ptr<JitStructType> input;
        std::shared_ptr<JitStructType> output;
        JitFuncHandle handle;
    };

    struct JitPipeline
    {
        std::shared_ptr<JitStructType> input;
        std::shared_ptr<JitStructType> output;
        std::vector<std::shared_ptr<JitFunction>> stages;
    };

    struct JitModule
    {
        std::unordered_map<std::string, std::shared_ptr<JitStructType>> structTypes;
        std::unordered_map<std::string, std::shared_ptr<JitFunction>> functions;
        std::unordered_map<std::string, std::shared_ptr<JitPipeline>> pipelines;
    };

    class JitBackend
    {
      public:
        virtual void stageModule(std::shared_ptr<IRModule> module) = 0;
        /// Consume all staged modules
        virtual Result<void> processModules() = 0;
        virtual Option<std::shared_ptr<JitPipeline>> getPipeline(std::string_view moduleName,
                                                                 std::string_view pipelineName) = 0;
    };
} // namespace BraneScript
