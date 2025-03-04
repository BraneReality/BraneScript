#pragma once

#include "../jitBackend.h"

namespace llvm
{
    class DataLayout;
    class Type;
    class JITEvaluatedSymbol;
    class Module;
    class LLVMContext;

    namespace orc
    {
        class LLJIT;
        class ExecutionSession;
        class IRCompileLayer;
        class MangleAndInterner;
        class RTDyldObjectLinkingLayer;
        class IRTransformLayer;
        class JITDylib;
        class ThreadSafeModule;
    } // namespace orc
} // namespace llvm

namespace BraneScript
{
    class LLVMJitBackend : public JitBackend
    {
        struct StageContext
        {
            std::shared_ptr<IRModule> ir;
            std::unique_ptr<llvm::orc::ThreadSafeModule> llvm;
        };

        std::vector<StageContext> _stagedModules;

        std::unique_ptr<llvm::orc::LLJIT> _llJit;

        std::unordered_map<std::string, llvm::JITEvaluatedSymbol> _nativeSymbols;

        std::unordered_map<std::string, JitFuncHandle> _functions;

      public:
        LLVMJitBackend();
        ~LLVMJitBackend();
        void stageModule(std::shared_ptr<IRModule> module) override;
        /// Consume all staged modules
        Result<void> processModules() override;
        Option<std::shared_ptr<JitPipeline>> getPipeline(std::string_view moduleName,
                                                         std::string_view pipelineName) override;

        std::unordered_map<std::string, JitFuncHandle>& functions();
    };
} // namespace BraneScript
