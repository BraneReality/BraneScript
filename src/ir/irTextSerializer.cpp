#include "irTextSerializer.h"
#include <format>

namespace BraneScript::IRSerializer
{
    std::string serializeNativeType(IRNativeType type)
    {
        switch(type)
        {
            case IRNativeType::U8:
                return "u8";
            case IRNativeType::I8:
                return "i8";
            case IRNativeType::U16:
                return "u16";
            case IRNativeType::I16:
                return "i16";
            case IRNativeType::U32:
                return "u32";
            case IRNativeType::I32:
                return "i32";
            case IRNativeType::F32:
                return "f32";
            case IRNativeType::U64:
                return "u64";
            case IRNativeType::I64:
                return "i64";
            case IRNativeType::F64:
                return "f64";
            case IRNativeType::U128:
                return "u128";
            case IRNativeType::I128:
                return "i128";
        }
        return "\"Parse write error\"";
    };

    std::string serializeIDRef(const IDRef& id)
    {
        return MATCHV(id,
                      [](const std::string& idStr) { return std::format("\"{}\"", idStr); },
                      [](int32_t idIndex) { return std::format("#{}", idIndex); });
    }

    std::string serializeType(const IRType& type)
    {
        return MATCHV(type,
                      [](const IRNativeType& type) { return serializeNativeType(type); },
                      [](const IDRef& type) { return serializeIDRef(type); });
    };

    std::string serializeIRValue(const IRValue& value) { return std::format("${}", value.id); }

    std::string serializeStoreVariant(const std::variant<IRValue, ConstU32>& store)
    {
        return MATCHV(store,
                      [](const IRValue& val) { return serializeIRValue(val); },
                      [](const ConstU32& val) { return std::format("{}", val.value); });
    }

    std::string serializeOp(const IROperation& op)
    {
        return MATCHV(
            op,
            [](const MovOp& mov)
        { return std::format("(mov {} {})", serializeIRValue(mov.src), serializeIRValue(mov.dest)); },
            [](const LoadOp& load)
        { return std::format("(load {} {})", serializeIRValue(load.ptr), serializeIRValue(load.dest)); },
            [](const StoreOp& store)
        { return std::format("(store {} {})", serializeIRValue(store.ptr), serializeIRValue(store.src)); },
            [](const MemAcc& memAcc)
        {
            return std::format("(ma {} #{} {} {})",
                               serializeType(memAcc.structType),
                               memAcc.index,
                               serializeIRValue(memAcc.ptr),
                               serializeIRValue(memAcc.dest));
        },
            [](const ConstI32& c) { return std::format("(const.i32 {} {})", serializeIRValue(c.dest), c.value); },
            [](const ConstU32& c) { return std::format("(const.u32 {} {})", serializeIRValue(c.dest), c.value); },
            [](const ConstF32& c) { return std::format("(const.f32 {} {})", serializeIRValue(c.dest), c.value); },
            [](const AddOp& op)
        {
            return std::format(
                "(add {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const SubOp& op)
        {
            return std::format(
                "(sub {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const MulOp& op)
        {
            return std::format(
                "(mul {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const DivOp& op)
        {
            return std::format(
                "(div {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const ModOp& op)
        {
            return std::format(
                "(mod {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const EqOp& op)
        {
            return std::format(
                "(eq {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const GeOp& op)
        {
            return std::format(
                "(ge {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const LogicNotOp& op)
        { return std::format("(logic.not {} {})", serializeIRValue(op.in), serializeIRValue(op.out)); },
            [](const LogicAndOp& op)
        {
            return std::format("(logic.and {} {} {})",
                               serializeIRValue(op.left),
                               serializeIRValue(op.right),
                               serializeIRValue(op.out));
        },
            [](const LogicOrOp& op)
        {
            return std::format(
                "(logic.or {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const BitNotOp& op)
        {
            return std::format(
                "(bit.not {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const BitAndOp& op)
        {
            return std::format(
                "(bit.and {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const BitOrOp& op)
        {
            return std::format(
                "(bit.or {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const BitXorOp& op)
        {
            return std::format(
                "(bit.xor {} {} {})", serializeIRValue(op.left), serializeIRValue(op.right), serializeIRValue(op.out));
        },
            [](const CallOp& call)
        {
            std::string inputs;
            for(const auto& input : call.inputs)
            {
                inputs += " " + serializeIRValue(input);
            }
            return std::format("(call {}{} {})", serializeIDRef(call.function), inputs, serializeIRValue(call.output));
        });
    }

    Result<std::string> irToText(const IRModule& module)
    {
        std::string structs;
        for(size_t i = 0; i < module.structs.size(); ++i)
        {
            auto& s = module.structs[i];
            std::string id = s.id ? s.id.value() : std::format("-{}", i);

            std::string members;
            for(size_t m = 0; m < s.members.size(); ++m)
            {
                std::string mid = s.members[i].id ? s.members[i].id.value() : std::format("-{}", i);

                members += std::format("(\"{}\" {})", mid, serializeType(s.members[i].type));
            }

            structs += std::format("\n(struct \"{}\" {})", id, members);
        }

        std::string functions;
        for(size_t i = 0; i < module.functions.size(); ++i)
        {
            auto& f = module.functions[i];
            std::string id = f.id ? f.id.value() : std::format("-{}", i);

            std::string input = serializeType(f.input);
            std::string output = serializeType(f.output);

            std::string vars;
            for(auto& var : f.localVars)
            {
                vars += " " + serializeType(var);
            }

            std::string ops;
            for(auto& op : f.operations)
            {
                ops += "\n\t" + serializeOp(op);
            }

            functions += std::format("\n(func \"{}\" {} {} (vars{}) (ops{}))", id, input, output, vars, ops);
        }

        std::string pipelines;
        for(size_t i = 0; i < module.pipelines.size(); ++i)
        {
            auto& p = module.pipelines[i];
            std::string id = p.id ? p.id.value() : std::format("-{}", i);

            std::string input = serializeType(p.input);
            std::string output = serializeType(p.output);

            std::string stages;
            for(auto& stage : p.stages)
                stages += " " + serializeIDRef(stage);

            pipelines += std::format("\n(pipe \"{}\" {} {} (stages{}))", id, input, output, stages);
        }
        return Ok(std::format("(module \"{}\" {} {} {})", module.id, structs, functions, pipelines));
    }
} // namespace BraneScript::IRSerializer
