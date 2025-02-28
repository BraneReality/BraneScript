#ifndef BRANESCRIPT_IR_H
#define BRANESCRIPT_IR_H

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "enums/option.h"

namespace BraneScript
{

    template<typename T>
    using IRNode = std::shared_ptr<T>;


    /// References can be stored as either full paths, or as context dependent ids, with positive ids representing local
    /// symbols, and negative representing external symbols, and 0 being a null value
    using IDRef = std::variant<std::string, int32_t>;

    enum class IRNativeType
    {
        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        F32,
        U64,
        I64,
        F64,
        U128,
        I128
    };

    using IRType = std::variant<IRNativeType, IDRef>;

    struct IRValue
    {
        uint32_t id;
    };

    struct ConstI32
    {
        IRValue dest;
        int32_t value;
    };

    struct ConstU32
    {
        IRValue dest;
        uint32_t value;
    };

    struct ConstF32
    {
        IRValue dest;
        float value;
    };

    // Move (and cast if needed) one values data to another
    struct MovOp
    {
        IRValue src;
        IRValue dest;
    };

    struct LoadOp
    {
        IRValue ptr;
        IRValue dest;
    };

    struct StoreOp
    {
        IRValue ptr;
        IRValue src;
    };

    struct MemAcc
    {
        uint8_t index;
        IRValue ptr;
        IRValue dest;
    };

    struct UnaryOp
    {
        IRValue in;
        IRValue out;
    };

    struct BinaryOp
    {
        IRValue left;
        IRValue right;
        IRValue out;
    };

    struct AddOp : public BinaryOp
    {
    };

    struct SubOp : public BinaryOp
    {
    };

    struct MulOp : public BinaryOp
    {
    };

    struct DivOp : public BinaryOp
    {
    };

    struct ModOp : public BinaryOp
    {
    };

    struct EqOp : public BinaryOp
    {
    };

    struct NeOp : public BinaryOp
    {
    };

    struct GtOp : public BinaryOp
    {
    };

    struct GeOp : public BinaryOp
    {
    };

    struct LogicNotOp : public UnaryOp
    {
    };

    struct LogicAndOp : public BinaryOp
    {
    };

    struct LogicOrOp : public BinaryOp
    {
    };

    struct BitNotOp : public BinaryOp
    {
    };

    struct BitAndOp : public BinaryOp
    {
    };

    struct BitOrOp : public BinaryOp
    {
    };

    struct BitXorOp : public BinaryOp
    {
    };

    struct CallOp
    {
        IDRef function;
        IRValue input;
        IRValue output;
    };

    struct NextStageOp
    {
        IRValue input;
    };

    using IROperation = std::variant<MovOp,
                                     LoadOp,
                                     StoreOp,
                                     MemAcc,

                                     AddOp,
                                     SubOp,
                                     MulOp,
                                     DivOp,
                                     ModOp,

                                     EqOp,
                                     GeOp,

                                     LogicNotOp,
                                     LogicAndOp,
                                     LogicOrOp,

                                     BitNotOp,
                                     BitAndOp,
                                     BitOrOp,
                                     BitXorOp,

                                     ConstI32,
                                     ConstU32,
                                     ConstF32,

                                     CallOp,
                                     NextStageOp>;

    struct IRStructMember
    {
        Option<std::string> id;
        IRType type;
    };

    struct IRStruct
    {
        Option<std::string> id;
        std::vector<IRStructMember> members;
    };

    struct IRPipeline
    {
        Option<std::string> id;
        IRType input;
        IRType output;
        std::vector<IDRef> stages;
    };

    struct IRFunction
    {
        Option<std::string> id;
        std::vector<IRType> localVars;
        IRType input;
        IRType output;
        std::vector<IROperation> operations;
    };

    struct IRModule
    {
        std::string id;
        std::vector<IRStruct> structs;
        std::vector<IRFunction> functions;
        std::vector<IRPipeline> pipelines;
    };


} // namespace BraneScript

#endif
