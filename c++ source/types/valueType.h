//
// Created by wirewhiz on 17/10/22.
//

#ifndef BRANESCRIPT_VALUEINDEX_H
#define BRANESCRIPT_VALUEINDEX_H

#include <cstdint>
namespace BraneScript
{
    enum class ValueType : uint8_t
    {
        Void           = 0,
        Bool           = 1,
        Char           = 2,
        Scalar_Begin   = 3,
        Int_Begin      = 3,
        Unsigned_Begin = 3,
        U32            = 3,
        U64            = 4,
        Unsigned_End   = 4,
        I32            = 5,
        I64            = 6,
        Int_End        = 6,
        Float_Begin    = 7,
        F32            = 7,
        F64            = 8,
        Float_End      = 8,
        Scalar_End     = 8,
        Struct         = 9,
    };
    bool isValueTypeScalar(ValueType type);
    bool isValueTypeInt(ValueType type);
    bool isValueTypeUnsigned(ValueType type);
    bool isValueTypeFloat(ValueType type);

    enum ValueStorageType : uint8_t
    {
        ValueStorageType_Null = 0,
        ValueStorageType_Reg = 1,
        ValueStorageType_Const = 2,
        ValueStorageType_Global = 3,
        ValueStorageType_GlobalPtr = 4,
        ValueStorageType_Ptr = 5,
        ValueStorageType_DerefPtr = 6
    };

    struct alignas(2) SerializedValue
    {
        uint16_t index = 0;
        uint16_t offset = 0;
        ValueType valueType;
        ValueStorageType storageType;
    };
}

#endif //BRANESCRIPT_VALUEINDEX_H
