#include "typechecker.h"

#define Calculate(_operation_) \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_u64 _operation_ rhs.as_u64) : \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_u64 _operation_ rhs.as_s64) : \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_u64 _operation_ rhs.as_f64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_s64 _operation_ rhs.as_u64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_s64 _operation_ rhs.as_s64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_s64 _operation_ rhs.as_f64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_f64 _operation_ rhs.as_u64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_f64 _operation_ rhs.as_s64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_f64 _operation_ rhs.as_f64) : \
    UNREACHABLE("operation evaluation for const")

#define Calculate2(_operation_) \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_u64 _operation_ rhs.as_u64) : \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_u64 _operation_ rhs.as_s64) : \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_u64 _operation_ rhs.as_f64) : \
    (TypeIsUnsigned(lhs.typeInfo) && TypeIsBool(rhs.typeInfo))     ? (lhs.as_u64 _operation_ rhs.as_bool) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_s64 _operation_ rhs.as_u64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_s64 _operation_ rhs.as_s64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_s64 _operation_ rhs.as_f64) : \
    (TypeIsSigned(lhs.typeInfo)   && TypeIsBool(rhs.typeInfo))     ? (lhs.as_s64 _operation_ rhs.as_bool) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_f64 _operation_ rhs.as_u64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_f64 _operation_ rhs.as_s64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_f64 _operation_ rhs.as_f64) : \
    (TypeIsFloat(lhs.typeInfo)    && TypeIsBool(rhs.typeInfo))     ? (lhs.as_f64 _operation_ rhs.as_bool) : \
    (TypeIsBool(lhs.typeInfo)     && TypeIsUnsigned(rhs.typeInfo)) ? (lhs.as_bool _operation_ rhs.as_u64) : \
    (TypeIsBool(lhs.typeInfo)     && TypeIsSigned(rhs.typeInfo))   ? (lhs.as_bool _operation_ rhs.as_s64) : \
    (TypeIsBool(lhs.typeInfo)     && TypeIsFloat(rhs.typeInfo))    ? (lhs.as_bool _operation_ rhs.as_f64) : \
    (TypeIsBool(lhs.typeInfo)     && TypeIsBool(rhs.typeInfo))     ? (lhs.as_bool _operation_ rhs.as_bool) : \
    UNREACHABLE("operation evaluation for const")

// u8 + u8
// u8 u16 u32 u64 s8 s16 s32 s64
// f32 f64
// TODO: check for overflow
ConstValue _add(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "add operation can only be performed on numbers");
    bool isSigned = TypeIsSigned(lhs) || TypeIsSigned(rhs);
    ConstValue result = {0};
    result.typeInfo->symbolType = lhs.typeInfo->symbolType > rhs.typeInfo->symbolType ? lhs.typeInfo->symbolType : rhs.typeInfo->symbolType;
    if(TypeIsFloat(lhs) || TypeIsFloat(lhs)) {
        result.as_f64 = Calculate(+);
    } else {
        if(isSigned) {
            result.as_s64 = Calculate(+);
        } else {
            result.as_u64 = Calculate(+);
        }
    }

    return result;
}

// TODO: check for underflow
ConstValue _sub(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "subtract operation can only be performed on numbers");
    bool isSigned = TypeIsSigned(lhs) || TypeIsSigned(rhs);
    ConstValue result = {0};
    result.typeInfo->symbolType = lhs.typeInfo->symbolType > rhs.typeInfo->symbolType ? lhs.typeInfo->symbolType : rhs.typeInfo->symbolType;
    if(TypeIsFloat(lhs) || TypeIsFloat(lhs)) {
        result.as_f64 = Calculate(-);
    } else {
        if(isSigned) {
            result.as_s64 = Calculate(-);
        } else {
            result.as_u64 = Calculate(-);
        }
    }

    return result;
}

// TODO: check for overflow
ConstValue _mul(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "multiply operation can only be performed on numbers");
    bool isSigned = TypeIsSigned(lhs) || TypeIsSigned(rhs);
    ConstValue result = {0};
    result.typeInfo->symbolType = lhs.typeInfo->symbolType > rhs.typeInfo->symbolType ? lhs.typeInfo->symbolType : rhs.typeInfo->symbolType;
    if(TypeIsFloat(lhs) || TypeIsFloat(lhs)) {
        result.as_f64 = Calculate(*);
    } else {
        if(isSigned) {
            result.as_s64 = Calculate(*);
        } else {
            result.as_u64 = Calculate(*);
        }
    }

    return result;
}

// TODO: check for underflow
ConstValue _div(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "division operation can only be performed on numbers");
    bool isSigned = TypeIsSigned(lhs) || TypeIsSigned(rhs);
    ConstValue result = {0};
    result.typeInfo->symbolType = lhs.typeInfo->symbolType > rhs.typeInfo->symbolType ? lhs.typeInfo->symbolType : rhs.typeInfo->symbolType;
    if(TypeIsFloat(lhs) || TypeIsFloat(lhs)) {
        result.as_f64 = Calculate(/);
    } else {
        if(isSigned) {
            result.as_s64 = Calculate(/);
        } else {
            result.as_u64 = Calculate(/);
        }
    }

    return result;
}

ConstValue _less(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "less than operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate(<);

    return result;
}

ConstValue _greater(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "greater than operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate(>);

    return result;
}

ConstValue _less_eq(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "less than or equal operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate(<=);

    return result;
}

ConstValue _greater_eq(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs) && TypeIsNumber(rhs), "greater than or equal operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate(>=);

    return result;
}

// ints floats bools
ConstValue _equals(ConstValue lhs, ConstValue rhs) {
    assert(
        (TypeIsNumber(lhs) || TypeIsBool(lhs)) &&
        (TypeIsNumber(rhs) || TypeIsBool(rhs)),
        "equals operation can only be performed on numbers and booleans"
    );

    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate2(==);

    return result;
}

ConstValue _not_equals(ConstValue lhs, ConstValue rhs) {
    assert(
        (TypeIsNumber(lhs) && TypeIsNumber(rhs)) ||
        (TypeIsBool(lhs) && TypeIsBool(rhs)),
        "not equals operation can only be performed on numbers and booleans"
    );

    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    result.as_bool = Calculate2(!=);

    return result;
}

#undef Calculate
