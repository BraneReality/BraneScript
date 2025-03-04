#include "llvmJitBackend.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"

#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <format>
#include <iostream>
#include <ostream>

namespace BraneScript
{
    struct LLVMModuleBuilderCtx
    {
        llvm::LLVMContext* llvmCtx;
        // IR builder
        std::shared_ptr<llvm::IRBuilder<>> builder;
        std::unique_ptr<llvm::Module> llvmMod;


        // Debug info builer
        std::unique_ptr<llvm::DIBuilder> dBuilder;
        llvm::DICompileUnit* diCompileUnit = nullptr;
        llvm::DIFile* diFile = nullptr;

        // walker state
        llvm::Function* currentFunc = nullptr;
        llvm::BasicBlock* currentBlock = nullptr;
        ;

        llvm::DISubprogram* diFunction = nullptr;

        const IRModule* currentMod;

        struct StructTypeCtx
        {
            llvm::StructType* llvmType;
            const IRStruct* bsType;
        };

        struct FunctionCtx
        {
            llvm::Function* llvmFunc;
            const IRFunction* bsFunc;
        };

        struct NativeTypeCtx
        {
            llvm::Type* llvmType = nullptr;
            std::variant<IRNativeType, IRStruct*> type;
        };

        using TypeContext = std::variant<NativeTypeCtx, StructTypeCtx>;

        struct ValueCtx
        {
            llvm::Value* value = nullptr;
            TypeContext type;
        };

        std::vector<StructTypeCtx> structs;
        std::vector<llvm::Function*> functions;
        std::vector<ValueCtx> values;

        // Cache values
        llvm::FunctionType* functionType = nullptr;
        llvm::Value* memTable = nullptr;
        llvm::Type* branePtrType = nullptr;
        ValueCtx retPtr;

        Result<NativeTypeCtx> getNativeType(IRNativeType type) const
        {
            llvm::Type* llvmType = nullptr;
            switch(type)
            {
                case IRNativeType::U8:
                case IRNativeType::I8:
                    llvmType = llvm::Type::getInt8Ty(*llvmCtx);
                case IRNativeType::U16:
                case IRNativeType::I16:
                    llvmType = llvm::Type::getInt16Ty(*llvmCtx);
                case IRNativeType::U32:
                case IRNativeType::I32:
                    llvmType = llvm::Type::getInt32Ty(*llvmCtx);
                case IRNativeType::F32:
                    llvmType = llvm::Type::getFloatTy(*llvmCtx);
                case IRNativeType::U64:
                case IRNativeType::I64:
                    llvmType = llvm::Type::getInt64Ty(*llvmCtx);
                case IRNativeType::F64:
                    llvmType = llvm::Type::getDoubleTy(*llvmCtx);
                case IRNativeType::U128:
                case IRNativeType::I128:
                    llvmType = llvm::Type::getInt128Ty(*llvmCtx);
            }
            if(!llvmType)
                return Err("Invalid IRNativeType value");
            return Ok(NativeTypeCtx{.llvmType = llvmType, .type = type});
        };

        Result<StructTypeCtx> getStructType(const IDRef& id)
        {
            return MATCHV(id,
                          [&](const std::string& idStr) -> Result<StructTypeCtx>
            {
                for(auto& s : structs)
                {
                    if(s.bsType->id.isNone())
                        continue;
                    if(s.bsType->id.value() == idStr)
                        return Ok(s);
                }
                return Err(std::format("No struct defined with id {}", idStr));
            },
                          [&](int32_t idIndex) -> Result<StructTypeCtx>
            {
                if(idIndex >= structs.size())
                    return Err(std::format("Struct index out of range"));
                return Ok(structs[idIndex]);
            });
        }

        static llvm::Type* getLLVMType(const TypeContext& ctx)
        {
            return MATCHV(ctx, [](auto& ctx) -> llvm::Type* { return ctx.llvmType; });
        }

        Result<TypeContext> getType(const IRType& type)
        {
            return MATCHV(type,
                          [&](const IRNativeType& type) -> Result<TypeContext>
            {
                auto res = getNativeType(type);
                if(!res)
                    return Err(res.err());
                return Ok<TypeContext>(res.ok());
            },
                          [&](const IDRef& type) -> Result<TypeContext>
            {
                auto res = getStructType(type);
                if(!res)
                    return Err(res.err());
                return Ok(res.ok());
            });
        };

        Result<ValueCtx> getValue(const IRValue& value)
        {
            if(value.id >= values.size())
                return Err(std::format("Value index {} out of range", value.id));
            return Ok(values[value.id]);
        }

        // Evalutate a int32 brane script pointer against the current memory table to get the full pointer
        llvm::Value* evaluatePtr(llvm::Type* type, llvm::Value* intPtr)
        {
            auto* shiftedHigh = builder->CreateLShr(intPtr, builder->getInt32(16), "");
            auto* maskedLow = builder->CreateAnd(intPtr, builder->getInt32(0xFFFF), "");

            auto* memIndex = builder->CreateTrunc(shiftedHigh, builder->getInt16Ty(), "");
            auto* bindingIndex = builder->CreateTrunc(maskedLow, builder->getInt16Ty(), "l");

            auto* basePtr = builder->CreateGEP(memTable->getType(), memTable, {bindingIndex});
            auto* offsetPtr = builder->CreatePtrAdd(basePtr, memIndex);
            return builder->CreateBitOrPointerCast(offsetPtr, llvm::PointerType::get(type, 0));
        }

        Result<ValueCtx> getStructMember(ValueCtx structValue, size_t member)
        {
            auto* structTypeCtx = std::get_if<StructTypeCtx>(&structValue.type);
            if(!structTypeCtx)
                return Err("Tried to get member of type that was not a struct");

            if(member > structTypeCtx->bsType->members.size())
                return Err(std::format("Tried to get member {} but struct only has {} members",
                                       member,
                                       structTypeCtx->bsType->members.size()));


            auto memberValue = builder->CreateStructGEP(structTypeCtx->llvmType, structValue.value, member);
            auto memberTypeRes = getType(structTypeCtx->bsType->members[member].type);
            if(!memberTypeRes)
                return Err("Could not resove member type: " + memberTypeRes.err());

            return Ok(ValueCtx{
                .value = memberValue,
                .type = memberTypeRes.ok(),
            });
        }

        Result<> buildMov(const MovOp& mov)
        {
            auto srcRes = getValue(mov.src);
            CHECK_RESULT(srcRes);
            auto destRes = getValue(mov.dest);
            CHECK_RESULT(destRes);

            auto srcPtr = srcRes.ok();
            auto destPtr = destRes.ok();

            auto srcValue = builder->CreateLoad(getLLVMType(srcPtr.type), srcPtr.value);
            builder->CreateStore(srcValue, destPtr.value);
            return Ok<>();
        }

        Result<> buildLoad(const LoadOp& load)
        {
            auto branePtrRes = getValue(load.ptr);
            CHECK_RESULT(branePtrRes);
            auto destRes = getValue(load.dest);

            auto branePtr = branePtrRes.ok();
            auto branePtrValue = builder->CreateLoad(getLLVMType(branePtr.type), branePtr.value);
            auto destValue = destRes.ok();


            auto loadedPtr = evaluatePtr(getLLVMType(destValue.type), branePtr.value);
            builder->CreateStore(loadedPtr, destValue.value);
            return Ok<>();
        }

        Result<> buildStore(const StoreOp& store)
        {

            auto branePtrRes = getValue(store.ptr);
            CHECK_RESULT(branePtrRes);
            auto srcRes = getValue(store.src);

            auto branePtr = branePtrRes.ok();
            auto branePtrValue = builder->CreateLoad(getLLVMType(branePtr.type), branePtr.value);
            auto srcValuePtr = srcRes.ok();

            auto srcValue = builder->CreateLoad(getLLVMType(srcValuePtr.type), srcValuePtr.value);


            auto loadedPtr = evaluatePtr(getLLVMType(srcValuePtr.type), branePtr.value);
            builder->CreateStore(srcValue, loadedPtr);
            return Ok<>();
        }

        Result<> buildMemAcc(const MemAcc& memAcc)
        {
            auto srcRes = getValue(memAcc.ptr);
            CHECK_RESULT(srcRes);
            auto destRes = getValue(memAcc.dest);
            CHECK_RESULT(destRes);

            auto srcPtr = srcRes.ok();
            auto destPtr = destRes.ok();

            auto memberValue = getStructMember(srcPtr, memAcc.index);
            CHECK_RESULT(memberValue);
            values[memAcc.dest.id] = memberValue.ok();

            return Ok<>();
        }

        Result<> buildAdd(const AddOp& op)
        {
            auto leftRes = getValue(op.left);
            CHECK_RESULT(leftRes);
            auto rightRes = getValue(op.right);
            CHECK_RESULT(leftRes);
            auto destRes = getValue(op.out);
            CHECK_RESULT(destRes);

            auto leftPtr = leftRes.ok();
            auto rightPtr = leftRes.ok();
            auto destPtr = destRes.ok();

            auto leftValue = builder->CreateLoad(getLLVMType(leftPtr.type), leftPtr.value);
            auto rightValue = builder->CreateLoad(getLLVMType(rightPtr.type), rightPtr.value);

            auto res = builder->CreateAdd(leftValue, rightValue);
            builder->CreateStore(res, destPtr.value);

            return Ok<>();
        }

        Result<> buildNextStage(const NextStageOp& call)
        {
            auto inputRes = getValue(call.input);
            CHECK_RESULT(inputRes);
            auto inputValue = inputRes.ok();
            auto inputStructType = std::get_if<StructTypeCtx>(&inputValue.type);
            if(!inputStructType)
                return Err("Tried to return non struct type");

            auto memberCount = inputStructType->bsType->members.size();
            retPtr.type = inputValue.type; // "Temporary workaround"
            for(size_t i = 0; i < memberCount; ++i)
            {
                auto inputMemberRes = getStructMember(inputValue, i);

                auto outputMemberRes = getStructMember(retPtr, i);
                CHECK_RESULT(inputMemberRes);
                CHECK_RESULT(outputMemberRes);
                auto inputMemberValue = inputMemberRes.ok();
                builder->CreateStore(builder->CreateLoad(getLLVMType(inputMemberValue.type), inputMemberValue.value),
                                     outputMemberRes.ok().value);
            }
            builder->CreateRetVoid();
            return Ok<>();
        }

        Result<> buildOp(const IROperation& op)
        {
            return MATCHV(op,
                          [&](const MovOp& mov) { return buildMov(mov); },
                          [&](const LoadOp& load) -> Result<> { return buildLoad(load); },
                          [&](const StoreOp& store) -> Result<> { return buildStore(store); },
                          [&](const MemAcc& memAcc) -> Result<> { return buildMemAcc(memAcc); },
                          [&](const ConstI32& c) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const ConstU32& c) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const ConstF32& c) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const AddOp& op) -> Result<> { return buildAdd(op); },
                          [&](const SubOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const MulOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const DivOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const ModOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const EqOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const GeOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const LogicNotOp& op) -> Result<>
            { // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const LogicAndOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const LogicOrOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const BitNotOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const BitAndOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const BitOrOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const BitXorOp& op) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const CallOp& call) -> Result<>
            {
                // TODO: Implement
                return Err("Operand not implmeneted");
            },
                          [&](const NextStageOp& call) -> Result<> { return buildNextStage(call); }

            );
        }

        Result<std::unique_ptr<llvm::Module>> buildModule(const IRModule& module,
                                                          const llvm::DataLayout& layout,
                                                          std::string_view triplet,
                                                          llvm::LLVMContext* ctxPtr)
        {
            llvmCtx = ctxPtr;
            builder = std::make_shared<llvm::IRBuilder<>>(*llvmCtx);

            currentMod = &module;
            llvmMod = std::make_unique<llvm::Module>(module.id, *llvmCtx);

            llvmMod->setDataLayout(layout);    // *_layout);
            llvmMod->setTargetTriple(triplet); //_session->getExecutorProcessControl().getTargetTriple().str());


            branePtrType = llvm::ArrayType::get(llvm::Type::getInt16Ty(*llvmCtx), 2);

            structs.reserve(module.structs.size());
            for(size_t i = 0; i < module.structs.size(); ++i)
            {
                auto& s = module.structs[i];

                std::string sid = s.id ? s.id.value() : std::format("-s{}", i);

                std::vector<llvm::Type*> memberTypes;
                for(size_t m = 0; m < s.members.size(); ++m)
                {
                    std::string mid = s.members[m].id ? s.members[m].id.value() : std::format("-{}", m);
                    auto llvmType = getType(s.members[m].type);
                    if(!llvmType)
                        return Err(llvmType.err());

                    memberTypes.push_back(getLLVMType(llvmType.ok()));
                }

                llvm::StructType* st;
                if(memberTypes.empty())
                    st = llvm::StructType::create(*llvmCtx, sid);
                else
                    st = llvm::StructType::create(memberTypes, sid, false);

                structs.push_back(StructTypeCtx{.llvmType = st, .bsType = &s});
            }

            auto memTableType = llvm::ArrayType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(*llvmCtx), 0), 65535);
            llvm::Type* funcArgs[3] = {llvm::PointerType::get(memTableType,
                                                              0),         // Pointer to array of addresses
                                       llvm::Type::getInt32Ty(*llvmCtx),  // Brane pointer to args struct
                                       llvm::Type::getInt32Ty(*llvmCtx)}; // Brane pointer to output struct
            functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(*llvmCtx), funcArgs, false);


            functions.reserve(module.functions.size());
            for(size_t i = 0; i < module.functions.size(); ++i)
            {
                auto& f = module.functions[i];

                auto inputRes = getStructType(f.input);
                if(!inputRes)
                    return Err(std::format("Couldn't compile input type for {}: {}", f.id, inputRes.err()));
                auto inputType = inputRes.ok();

                auto outputRes = getStructType(f.output);
                if(!outputRes)
                    return Err(std::format("Couldn't compile output type for {}: {}", f.id, inputRes.err()));
                auto outputType = outputRes.ok();

                auto linkage = llvm::Function::ExternalLinkage;
                currentFunc = llvm::Function::Create(functionType, linkage, f.id, llvmMod.get());
                currentFunc->setCallingConv(llvm::CallingConv::C);
                currentBlock = llvm::BasicBlock::Create(*llvmCtx, "entry", currentFunc);
                builder->SetInsertPoint(currentBlock);

                memTable = currentFunc->getArg(0);
                auto inputStruct =
                    ValueCtx{.value = evaluatePtr(inputType.llvmType, currentFunc->getArg(1)), .type = inputType};

                // Really need a better return mechanism
                retPtr =
                    ValueCtx{.value = evaluatePtr(outputType.llvmType, currentFunc->getArg(2)), .type = outputType};

                auto inputCount = inputType.bsType->members.size();
                for(size_t i = 0; i < f.localVars.size(); ++i)
                {
                    auto& var = f.localVars[i];
                    if(i < inputCount)
                    {
                        auto argRes = getStructMember(inputStruct, i);
                        if(!argRes)
                            return Err(argRes.err());
                        values.push_back(argRes.ok());
                        std::cout << i << " Re-route function arg member" << std::endl;
                    }
                    else
                    {
                        auto typeRes = getType(f.localVars[i]);
                        if(!typeRes)
                            return Err(typeRes.err());
                        MATCHV(typeRes.ok(),
                               [&](NativeTypeCtx valueType)
                        {
                            auto* value = builder->CreateAlloca(valueType.llvmType);
                            values.push_back(ValueCtx{.value = value, .type = valueType});
                            std::cout << i << " Allocated new native value" << std::endl;
                        },
                               [&](StructTypeCtx valueType)
                        {
                            auto* value = builder->CreateAlloca(valueType.llvmType);
                            values.push_back(ValueCtx{.value = value, .type = valueType});
                            std::cout << i << " Allocated new struct value" << std::endl;
                        });
                    }
                }

                for(auto& op : f.operations)
                {
                    auto res = buildOp(op);
                    if(!res)
                        return Err(res.err());
                }

                values.clear();
                std::string funcError;
                llvm::raw_string_ostream funcErrorStream(funcError);
                if(llvm::verifyFunction(*currentFunc, &funcErrorStream))
                    return Err(funcError);
            }

            std::string modError;
            llvm::raw_string_ostream modErrorStream(modError);
            if(llvm::verifyModule(*llvmMod, &modErrorStream))
                return Err(modError);

            std::string moduleContent;
            llvm::raw_string_ostream moduleStream(moduleContent);
            llvmMod->print(moduleStream, nullptr);
            std::cout << "Staging module: \n" << moduleContent << std::endl;

            return Ok(std::move(llvmMod));
        }

        LLVMModuleBuilderCtx() {}
    }; // namespace BraneScript

    bool llvmInitialized = false;

    LLVMJitBackend::LLVMJitBackend()
    {
        if(!llvmInitialized)
        {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();
            llvmInitialized = true;
        }

        llvm::ObjectCache* OC = nullptr; // Implement later?


        auto builderRes =
            llvm::orc::LLJITBuilder()
                .setCompileFunctionCreator(
                    [&](llvm::orc::JITTargetMachineBuilder JTMB)
                        -> llvm::Expected<std::unique_ptr<llvm::orc::IRCompileLayer::IRCompiler>>
        {
            auto TM = JTMB.createTargetMachine();
            if(!TM)
                return TM.takeError();
            return std::make_unique<llvm::orc::TMOwningSimpleCompiler>(std::move(*TM), OC);
        })
                .setObjectLinkingLayerCreator([&](llvm::orc::ExecutionSession& ES,
                                                  const llvm::Triple& TT) -> std::unique_ptr<llvm::orc::ObjectLayer>
        {
            // Except for the GDBListener registration, the rest of
            // the code is taken from LLJIT.cpp.
            auto GetMemMgr = []() { return std::make_unique<llvm::SectionMemoryManager>(); };
            auto ObjLinkingLayer = std::make_unique<llvm::orc::RTDyldObjectLinkingLayer>(ES, std::move(GetMemMgr));
            if(TT.isOSBinFormatCOFF())
            {
                ObjLinkingLayer->setOverrideObjectFlagsWithResponsibilityFlags(true);
                ObjLinkingLayer->setAutoClaimResponsibilityForObjectSymbols(true);
            }
            ObjLinkingLayer->registerJITEventListener(*llvm::JITEventListener::createGDBRegistrationListener());
            return ObjLinkingLayer;
        }).create();
        if(!builderRes)
            throw std::runtime_error("Failed to create llvm jit");
        _llJit = std::move(*builderRes);
    }

    LLVMJitBackend::~LLVMJitBackend() {};

    void LLVMJitBackend::stageModule(std::shared_ptr<IRModule> module)
    {
        auto llvmCtx = std::make_unique<llvm::LLVMContext>();
        LLVMModuleBuilderCtx ctx;
        auto newMod = ctx.buildModule(*module, _llJit->getDataLayout(), _llJit->getTargetTriple().str(), llvmCtx.get());
        if(!newMod)
        {
            std::cout << "Failed to stage module! " << newMod.err() << std::endl;
            return;
        }
        _stagedModules.push_back(StageContext{
            .ir = module,
            .llvm = std::make_unique<llvm::orc::ThreadSafeModule>(std::move(newMod.ok()), std::move(llvmCtx)),
        });
    }

    Result<void> LLVMJitBackend::processModules()
    {
        for(auto& mod : _stagedModules)
        {

            auto libRes = _llJit->createJITDylib(mod.ir->id);
            if(!libRes)
            {
                std::string resMessage;
                llvm::raw_string_ostream resStream(resMessage);
                resStream << libRes.takeError();
                std::cout << "Failed to create jit dylib: " << resMessage << std::endl;
                continue;
            }
            auto& lib = *libRes;

            auto addRes = _llJit->addIRModule(lib, std::move(*mod.llvm));
            if(addRes)
            {
                std::string resMessage;
                llvm::raw_string_ostream resStream(resMessage);
                resStream << addRes;
                std::cout << "Failed to add jit module: " << resMessage << std::endl;
                continue;
            }

            for(auto& func : mod.ir->functions)
            {
                auto functionSym = _llJit->lookup(lib, func.id);
                if(!functionSym)
                    throw std::runtime_error("Unable to find exported function: " + toString(functionSym.takeError()));
                _functions.insert({func.id, functionSym->toPtr<JitFuncHandle>()});
            }


            /*std::unordered_map<std::string, std::unique_ptr<StructDef>> newStructs;
            // Extract all structs before populating members, so we can resolve out of order dependencies.

            for(auto& s : irModule.structs)
            {
                auto sConstructor = module->getFunction<void, void*>(s.constructorSig);
                auto sDestructor = module->getFunction<void, void*>(s.destructorSig);
                auto sCopyConstructor = module->getFunction<void, void*, const void*>(s.copyConstructorSig);
                auto sMoveConstructor = module->getFunction<void, void*, void*>(s.moveConstructorSig);
                if(!sConstructor || !sDestructor || !sCopyConstructor || !sMoveConstructor)
                    throw std::runtime_error("Module load failed, missing constructors for: " + s.name);
                auto sDef = new StructDef(s.name, sConstructor, sCopyConstructor, sMoveConstructor, sDestructor);
                for(auto& tag : s.tags)
                    sDef->tags.insert(tag);
                newStructs.emplace(s.name, sDef);
            }

            for(auto& ns : newStructs)
            {
                if(_types.contains(ns.first))
                    throw std::runtime_error("Module load failed, cannot load a type twice: " + ns.first);
                _types.insert({ns.first, ns.second.get()});
            }

            size_t structIndex = 0;
            for(auto& ns : newStructs)
            {
                auto structLayout = exportedStructLayouts[structIndex];
                auto& srcStruct = irModule.structs[structIndex];
                ns.second->size = structLayout->getSizeInBytes();
                for(size_t i = 0; i < srcStruct.members.size(); i++)
                {
                    auto& src = srcStruct.members[i];
                    auto memberOffset = structLayout->getElementOffset(i);
                    auto memberType = _types.find(src.type);
                    if(memberType == _types.end())
                        throw std::runtime_error("Module load failed, unable to find type metadata: " + src.type);
                    ns.second->memberVars.push_back(
                        StructVar{src.name, VarType{memberType->second, src.isRef}, memberOffset});
                }

                printf("Adding struct %s\n", ns.first.c_str());
                printf("Size: %zu\n", ns.second->size);
                printf("Members:\n");
                for(auto& m : ns.second->memberVars)
                    printf("  %s %s, offset: %zu\n", m.type.def->name.c_str(), m.name.c_str(), m.offset);

                module->structDefinitions.insert({ns.first, std::move(ns.second)});
                ++structIndex;
            }*/

            /*_modules.insert(irModule.id, module);
            for(auto& dep : irModule.links)
                _modules.addDependency(irModule.id, dep);*/
        }
        return Ok<>();
    }

    Option<std::shared_ptr<JitPipeline>> LLVMJitBackend::getPipeline(std::string_view moduleName,
                                                                     std::string_view pipelineName)
    {
        return None();
    }

    std::unordered_map<std::string, JitFuncHandle>& LLVMJitBackend::functions() { return _functions; }
} // namespace BraneScript
