#include "typechecker.h"

// u8 + u8
// u8 u16 u32 u64 s8 s16 s32 s64
// f32 f64
// TODO: check for overflow
ConstValue _add(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "add operation can only be performed on numbers");
    ConstValue result = {0};

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        result.typeInfo = lhs.typeInfo;
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_f64 = lhs.as_f64 + rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_s64 = lhs.as_s64 + rhs.as_s64;
            } else {
                result.as_u64 = lhs.as_u64 + rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

// TODO: check for underflow
ConstValue _sub(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "subtract operation can only be performed on numbers");
    ConstValue result = {0};

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        result.typeInfo = lhs.typeInfo;
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_f64 = lhs.as_f64 - rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_s64 = lhs.as_s64 - rhs.as_s64;
            } else {
                result.as_u64 = lhs.as_u64 - rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

// TODO: check for overflow
ConstValue _mul(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "multiply operation can only be performed on numbers");
    ConstValue result = {0};

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        result.typeInfo = lhs.typeInfo;
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_f64 = lhs.as_f64 * rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_s64 = lhs.as_s64 * rhs.as_s64;
            } else {
                result.as_u64 = lhs.as_u64 * rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

// TODO: check for underflow
ConstValue _div(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "division operation can only be performed on numbers");
    ConstValue result = {0};

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        result.typeInfo = lhs.typeInfo;
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_f64 = lhs.as_f64 / rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_s64 = lhs.as_s64 / rhs.as_s64;
            } else {
                result.as_u64 = lhs.as_u64 / rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

ConstValue _less(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "less than operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_bool = lhs.as_f64 < rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_bool = lhs.as_s64 < rhs.as_s64;
            } else {
                result.as_bool = lhs.as_u64 < rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

ConstValue _greater(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "greater than operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_bool = lhs.as_f64 > rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_bool = lhs.as_s64 > rhs.as_s64;
            } else {
                result.as_bool = lhs.as_u64 > rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

ConstValue _less_eq(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "less than or equal operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_bool = lhs.as_f64 <= rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_bool = lhs.as_s64 <= rhs.as_s64;
            } else {
                result.as_bool = lhs.as_u64 <= rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

ConstValue _greater_eq(ConstValue lhs, ConstValue rhs) {
    assert(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo), "greater than or equal operation can only be performed on numbers");
    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;

    if(TypeMatch(lhs.typeInfo, rhs.typeInfo)) {
        bool isSigned = TypeIsSigned(lhs.typeInfo);

        if(TypeIsFloat(lhs.typeInfo)) {
            result.as_bool = lhs.as_f64 >= rhs.as_f64;
        } else {
            if(isSigned) {
                result.as_bool = lhs.as_s64 >= rhs.as_s64;
            } else {
                result.as_bool = lhs.as_u64 >= rhs.as_u64;
            }
        }
    } else {
        UNREACHABLE("operation evaluation for const");
    }

    return result;
}

// ints floats bools
ConstValue _equals(ConstValue lhs, ConstValue rhs) {
    assert(
        (TypeIsInt(lhs.typeInfo) && TypeIsInt(rhs.typeInfo)) ||
        (TypeIsFloat(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo)) ||
        (TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)),
        "equals operation can only be performed on numbers and booleans"
    );

    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL;
    if(TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) {
        result.as_bool = lhs.as_bool == rhs.as_bool;
    } else if(TypeIsFloat(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo)) {
        result.as_bool = lhs.as_f64 == rhs.as_f64;
    } else {
        if(0);
        else if(TypeIsSigned(lhs.typeInfo)   && TypeIsSigned(rhs.typeInfo))   result.as_bool = lhs.as_s64 == rhs.as_s64;
        else if(TypeIsUnsigned(lhs.typeInfo) && TypeIsUnsigned(rhs.typeInfo)) result.as_bool = lhs.as_u64 == rhs.as_u64;
        else if(TypeIsSigned(lhs.typeInfo)   && TypeIsUnsigned(rhs.typeInfo)) {
            s64 as_signed = rhs.as_u64;
            if(as_signed < 0) result.as_bool = FALSE;
            else result.as_bool = lhs.as_s64 == rhs.as_s64;
        } else if(TypeIsUnsigned(lhs.typeInfo) && TypeIsSigned(rhs.typeInfo)) {
            s64 as_signed = lhs.as_u64;
            if(as_signed < 0) result.as_bool = FALSE;
            else result.as_bool = lhs.as_s64 == rhs.as_s64;
        }
    }

    return result;
}

ConstValue _not_equals(ConstValue lhs, ConstValue rhs) {
    assert(
        (TypeIsInt(lhs.typeInfo) && TypeIsInt(rhs.typeInfo)) ||
        (TypeIsFloat(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo)) ||
        (TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)),
        "not equals operation can only be performed on numbers and booleans"
    );

    ConstValue result = {0};
    result.typeInfo->symbolType = TYPE_BOOL; // NOTE: i think this is a null dereference
    if(TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) {
        result.as_bool = lhs.as_bool != rhs.as_bool;
    } else if(TypeIsFloat(lhs.typeInfo) && TypeIsFloat(rhs.typeInfo)) {
        result.as_bool = lhs.as_f64 != rhs.as_f64;
    } else {
        if(0);
        else if(TypeIsSigned(lhs.typeInfo)   && TypeIsSigned(rhs.typeInfo))   result.as_bool = lhs.as_s64 == rhs.as_s64;
        else if(TypeIsUnsigned(lhs.typeInfo) && TypeIsUnsigned(rhs.typeInfo)) result.as_bool = lhs.as_u64 == rhs.as_u64;
        else if(TypeIsSigned(lhs.typeInfo)   && TypeIsUnsigned(rhs.typeInfo)) {
            s64 as_signed = rhs.as_u64;
            if(as_signed < 0) result.as_bool = TRUE;
            else result.as_bool = lhs.as_s64 == rhs.as_s64;
        } else if(TypeIsUnsigned(lhs.typeInfo) && TypeIsSigned(rhs.typeInfo)) {
            s64 as_signed = lhs.as_u64;
            if(as_signed < 0) result.as_bool = TRUE;
            else result.as_bool = lhs.as_s64 == rhs.as_s64;
        }
    }

    return result;
}
