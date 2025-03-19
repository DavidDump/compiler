#include "typechecker.h"
#include "operatorFunctions.h"

#include <string.h>
#include <math.h>

// typedef ConstValue (*EvalBinaryFuncPtr)(ConstValue lhs, ConstValue rhs);

// typedef struct Operation {
//     TokenType op;
//     Type lhs;
//     Type rhs;
//     EvalBinaryFuncPtr func;
// } Operation;

// Operation Operations[] = {
//     {.lhs = TYPE_U8,  .op = TokenType_ADD, .rhs = TYPE_U8,  .func = _add_u8_u8},
// };

ConstValue evaluateBinaryExpression(ConstValue lhs, Token operator, ConstValue rhs) {
    // TODO: static assert on the number of operators
    if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_ADD) {
        return _add(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_SUB) {
        return _sub(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_MUL) {
        return _mul(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_DIV) {
        return _div(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_LESS) {
        return _less(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_GREATER) {
        return _greater(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_LESS_EQ) {
        return _less_eq(lhs, rhs);
    } else if(TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_GREATER_EQ) {
        return _greater_eq(lhs, rhs);
    } else if(((TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) || (TypeIsNumber(rhs.typeInfo) && TypeIsNumber(lhs.typeInfo))) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_COMPARISON) {
        return _equals(lhs, rhs);
    } else if(((TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) || (TypeIsNumber(rhs.typeInfo) && TypeIsNumber(lhs.typeInfo))) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_NOT_EQUALS) {
        return _not_equals(lhs, rhs);
    } else {
        ERROR_VA(operator.loc, "Invalid binary operation: %s "STR_FMT" %s", TypeStr[lhs.typeInfo->symbolType], STR_PRINT(operator.value), TypeStr[rhs.typeInfo->symbolType]);
    }

    // NOTE: add custom operator definitions here

    return (ConstValue){0}; // NOTE: to silence warning
}

ConstValue evaluateUnaryExpression(Token operator, ConstValue value) {
    if(operator.type == TokenType_SUB) {
        if(!TypeIsNumber(value.typeInfo)) {
            Location loc = {.filename = STR(""), .collum = 1, .line = 1}; // TODO: fix loc
            ERROR(loc, "Can only perform the - unary operator on a number");
        }
        switch(value.typeInfo->symbolType) {
            case TYPE_NONE:
            case TYPE_COUNT: {
                UNREACHABLE("invalid case, none or count");
            } break;

            // TODO: error on unsigned type
            case TYPE_U8:
            case TYPE_U16:
            case TYPE_U32:
            case TYPE_U64: {
                value.as_u64 = -value.as_u64;
            } break;
            case TYPE_S8:
            case TYPE_S16:
            case TYPE_S32:
            case TYPE_S64: {
                value.as_s64 = -value.as_s64;
            } break;
            case TYPE_F32:
            case TYPE_F64: {
                value.as_f64 = -value.as_f64;
            } break;

            case TYPE_STRING:
            case TYPE_BOOL:
            case TYPE_VOID:
            case TYPE_ARRAY:
            case TYPE_FUNCTION: {
                Location loc = {.filename = STR(""), .collum = 1, .line = 1}; // TODO: fix loc
                ERROR_VA(loc, "Can not perform - unary operator on: %s", TypeStr[value.typeInfo->symbolType]);
            } break;
        }
    } else {
        UNREACHABLE_VA("invalid unary operator: %s", TokenTypeStr[operator.type]);
    }

    return value;
}

// TODO: if any of the scope structs change i need to update here
// static_assert(sizeof(TypecheckedGlobalScope) == ?);
// static_assert(sizeof(TypecheckedGenericScope) == ?);
bool isSymbolDefined(TypecheckedScope* containing, Hashmap(String, ConstValue)* constants, String id) {
    ConstValue tmp = {0};
    TypeInfo*  tmp2 = 0;

    if(HashmapGet(String, ConstValue)(constants, id, &tmp)) return TRUE;

    TypecheckedScope* it = containing;
    while(it) {
        if(HashmapGet(String, ConstValue)(&it->functions, id, &tmp)) return TRUE;
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, id, &tmp2)) return TRUE;

        it = it->parent;
    }

    return FALSE;
}

void saveVariable(TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, String id, TypeInfo* type) {
    // check for redefinition
    if(isSymbolDefined(scope, constants, id)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    }

    // save the symbol
    if(!HashmapSet(String, TypeInfoPtr)(&scope->variables, id, type)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, scope->variables.capacity, scope->variables.size, STR_PRINT(id));
    }
}

// containing is the scope that this constant is declared in
void saveConstant(TypecheckedScope* containing, Hashmap(String, ConstValue)* constants, String id, ConstValue val) {
    // check for redefinition
    if(isSymbolDefined(containing, constants, id)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    }

    // TODO: params somehow
    // for(u64 i = 0; i < it->params.size; ++i) {
    //     StringAndType arg = it->params.data[i];
    //     if(StringEquals(arg.id, id)) {
    //         Location loc = {0}; // TODO: fix
    //         ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    //     }
    // }

    // save the symbol
    if(!HashmapSet(String, ConstValue)(constants, id, val)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, constants->capacity, constants->size, STR_PRINT(id));
    }
}

void saveFunction(TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, String id, ConstValue val) {
    // check for redefinition
    if(isSymbolDefined(scope, constants, id)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    }

    // TODO: params somehow
    // for(u64 i = 0; i < it->params.size; ++i) {
    //     StringAndType arg = it->params.data[i];
    //     if(StringEquals(arg.id, id)) {
    //         Location loc = {0}; // TODO: fix
    //         ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    //     }
    // }

    // save the function
    if(!HashmapSet(String, ConstValue)(&scope->functions, id, val)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, scope->functions.capacity, scope->functions.size, STR_PRINT(id));
    }
}

TypecheckedScope* TypecheckedScopeInit(Arena* mem, TypecheckedScope* parent) {
    TypecheckedScope* result = arena_alloc(mem, sizeof(TypecheckedScope));
    result->parent = parent;
    HashmapInit(result->functions, 0x100); // TODO: init size
    HashmapInit(result->variables, 0x100); // TODO: init size
    if(parent) ArrayAppend(parent->children, result);
    return result;
}

EvaluateConstantResult evaluateConstant(Expression* expr, Arena* mem, Hashmap(String, ConstValue)* evaluatedConstatants) {
    EvaluateConstantResult result = {0};

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            result.val.typeInfo = TypeInitSimple(mem, TYPE_S64);
            result.val.as_s64 = StringToS64(expr->expr.INT_LIT.value);
        } break;
        case ExpressionType_FLOAT_LIT: {
            result.val.typeInfo = TypeInitSimple(mem, TYPE_F64);
            u64 fractPartLen = expr->expr.FLOAT_LIT.fractPart.length;
            s64 wholePart = StringToS64(expr->expr.FLOAT_LIT.wholePart);
            s64 fractPart = StringToS64(expr->expr.FLOAT_LIT.fractPart);

            result.val.as_f64 = ((f64)wholePart) + ((f64)fractPart / (10 * fractPartLen));
        } break;
        case ExpressionType_STRING_LIT: {
            result.val.typeInfo = TypeInitSimple(mem, TYPE_STRING);
            result.val.as_String = expr->expr.STRING_LIT.value;
        } break;
        case ExpressionType_BOOL_LIT: {
            result.val.typeInfo = TypeInitSimple(mem, TYPE_BOOL);
            if(StringEqualsCstr(expr->expr.BOOL_LIT.value, "true")) result.val.as_bool = TRUE;
            else if(StringEqualsCstr(expr->expr.BOOL_LIT.value, "false")) result.val.as_bool = FALSE;
        } break;
        case ExpressionType_SYMBOL: {
            String key = expr->expr.SYMBOL.identifier;
            if(!HashmapGet(String, ConstValue)(evaluatedConstatants, key, &result.val)) {
                // not evaluated yet
                result.error = ConstantEvaluationError_UNDEFINED_SYMBOL;
            }
        } break;
        case ExpressionType_FUNCTION_CALL: {
            Location loc = {.filename = STR(""), .collum = 1, .line = 1}; // TODO: fix loc
            ERROR(loc, "Function call cannot be used in a constant, use #run");
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            EvaluateConstantResult lhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.lhs, mem, evaluatedConstatants);
            ConstValue lhs = lhsRes.val;
            if(lhsRes.error) {
                result.error = TRUE;
                break;
            }

            EvaluateConstantResult rhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.rhs, mem, evaluatedConstatants);
            ConstValue rhs = rhsRes.val;
            if(rhsRes.error) {
                result.error = TRUE;
                break;
            }

            Token op = expr->expr.BINARY_EXPRESSION.operator;
            if(lhs.typeInfo->symbolType == TYPE_FUNCTION || rhs.typeInfo->symbolType == TYPE_FUNCTION) {
                ERROR_VA(op.loc, "Cannot perform binary operation: "STR_FMT" on a function literal", STR_PRINT(op.value));
            }

            result.val = evaluateBinaryExpression(lhs, op, rhs);
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            EvaluateConstantResult valRes = evaluateConstant(expr->expr.UNARY_EXPRESSION.expr, mem, evaluatedConstatants);
            ConstValue val = valRes.val;
            if(valRes.error) {
                result.error = TRUE;
                break;
            }

            Token op = expr->expr.UNARY_EXPRESSION.operator;
            if(val.typeInfo->symbolType == TYPE_FUNCTION) {
                ERROR_VA(op.loc, "Cannot perform unary operation: "STR_FMT" on a function literal", STR_PRINT(op.value));
            }

            result.val = evaluateUnaryExpression(op, val);
        } break;
        case ExpressionType_FUNCTION_LIT: {
            UNREACHABLE("invalid type ExpressionType_FUNCTION_LIT in evaluateConstant: all functions should be skipped before we get to this part");
        } break;
    }

    return result;
}

Expression ConstValueToExpression(Arena* mem, ConstValue value) {
    Expression result = {0};
    
    switch(value.typeInfo->symbolType) {
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64: {
            result.type = ExpressionType_INT_LIT;
            result.expr.INT_LIT.value = StringFromU64(mem, value.as_u64);
        } break;
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_S64: {
            if(value.as_s64 < 0) {
                // negative
                s64 invertedNumber = -value.as_s64;
                Expression* num = arena_alloc(mem, sizeof(Expression));
                num->type = ExpressionType_INT_LIT;
                num->expr.INT_LIT.value = StringFromS64(mem, invertedNumber);

                result.type = ExpressionType_UNARY_EXPRESSION;
                result.expr.UNARY_EXPRESSION.operator = (Token){.value = StringFromCstrLit("-"), .type = TokenType_SUB};
                result.expr.UNARY_EXPRESSION.expr = num;
            } else {
                result.type = ExpressionType_INT_LIT;
                result.expr.INT_LIT.value = StringFromS64(mem, value.as_s64);
            }
        } break;
        case TYPE_F32:
        case TYPE_F64: {
            result.type = ExpressionType_FLOAT_LIT;
            f64 whole = 0.0f;
            f64 fract = modf(value.as_f64, &whole);
            result.expr.FLOAT_LIT.wholePart = StringFromF64(mem, whole);
            result.expr.FLOAT_LIT.fractPart = StringFromF64(mem, fract);
        } break;
        case TYPE_STRING: {
            result.type = ExpressionType_STRING_LIT;
            result.expr.STRING_LIT.value = value.as_String;
        } break;
        case TYPE_BOOL: {
            result.type = ExpressionType_BOOL_LIT;
            result.expr.BOOL_LIT.value = (value.as_bool ? StringFromCstrLit("true") : StringFromCstrLit("false"));
        } break;
        case TYPE_FUNCTION: {
            UNIMPLEMENTED("ConstValueToExpression: TYPE_FUNCTION");
            result.type = ExpressionType_FUNCTION_LIT;

            result.expr.FUNCTION_LIT.typeInfo = value.typeInfo->functionInfo;
            // result.expr.FUNCTION_LIT.scope = ;
        } break;
        case TYPE_ARRAY: {
            UNIMPLEMENTED("ConstValueToExpression: TYPE_ARRAY");
        } break;
        case TYPE_NONE:
        case TYPE_COUNT:
        case TYPE_VOID:
            UNREACHABLE("invalid type");
            break;
    }

    return result;
}

ConstResult findConstant(Hashmap(String, ConstValue)* constants, String identifier) {
    ConstValue val = {0};
    if(HashmapGet(String, ConstValue)(constants, identifier, &val)) {
        return (ConstResult){.value = val};
    }

    return (ConstResult){.err = TRUE};
}

TypeResult typecheckFindFunctionType(TypecheckedScope* scope, String id) {
    TypecheckedScope* it = scope;
    while(it) {
        ConstValue value = {0};
        if(HashmapGet(String, ConstValue)(&it->functions, id, &value)) return (TypeResult){.typeInfo = value.typeInfo};
        it = it->parent;
    }

    Location loc = {0};
    ERROR_VA(loc, "Undefined function: "STR_FMT, STR_PRINT(id));
    return (TypeResult){0}; // remove warning;
}

// symbol means both constant and variable
#if 0
TypeResult findSymbolType(TypecheckedScope scope, Hashmap(String, ConstValue)* constants, String id) {
    ConstValue val1 = {0};
    TypeInfo*  val2 = 0;
    if(HashmapGet(String, ConstValue)(constants, id, &val1)) return (TypeResult){.typeInfo = val1.typeInfo};
    
    TypecheckedScope it = scope;
    while(it.type != TypecheckedScopeType_NONE) {
        switch(it.type) {
            case TypecheckedScopeType_NONE: {
                UNREACHABLE("TypecheckedScopeType_NONE invalid type: cant store function");
            } break;
            case TypecheckedScopeType_GLOBAL: {
                TypecheckedGlobalScope* s = it.scope.as_global;

                if(HashmapGet(String, ConstValue)(&s->functions, id, &val1)) return (TypeResult){.typeInfo = val1.typeInfo};
                if(HashmapGet(String, TypeInfoPtr)(&s->variables, id, &val2)) return (TypeResult){.typeInfo = val2};

                it.type = TypecheckedScopeType_NONE;
            } break;
            case TypecheckedScopeType_GENERIC: {
                TypecheckedGenericScope* s = it.scope.as_generic;

                if(HashmapGet(String, TypeInfoPtr)(&s->variables, id, &val2)) return (TypeResult){.typeInfo = val2};

                it = s->parent;
            } break;
        }
    }

    // TODO: something about params
    // for(u64 i = 0; i < it->params.size; ++i) {
    //     StringAndType arg = it->params.data[i];
    //     if(StringEquals(arg.id, identifier)) return (TypeResult){.typeInfo = arg.type};
    // }

    return (TypeResult){.err = TRUE};
}
#endif

TypeResult findVariableType(TypecheckedScope* scope, String id) {
    TypeInfo*  result = 0;
    TypecheckedScope* it = scope;
    while(it) {
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, id, &result)) return (TypeResult){.typeInfo = result};

        it = it->parent;
    }

    // TODO: something about params
    // for(u64 i = 0; i < it->params.size; ++i) {
    //     StringAndType arg = it->params.data[i];
    //     if(StringEquals(arg.id, identifier)) return (TypeResult){.typeInfo = arg.type};
    // }

    return (TypeResult){.err = TRUE};
}

// expected is the type that the expression should be to pass typechecking,
// can be used when assigning number literals to symbols that have a concrete types,
// this value can be null to just use the default type
TypecheckedExpression* typecheckExpression(Arena* mem, Expression* expr, TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, TypeInfo* expected) {
    TypecheckedExpression* result = arena_alloc(mem, sizeof(TypecheckedExpression));
    memcpy(result, expr, sizeof(Expression));
    // NOTE: this has a high likelyhood of fucking me in the ass later on

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            if(TypeIsInt(expected)) {
                result->typeInfo = expected;
            } else {
                TypeInfo* t = TypeInitSimple(mem, TypeDefaultInt());
                result->typeInfo = t;
            }
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_FLOAT_LIT: {
            if(TypeIsFloat(expected)) {
                result->typeInfo = expected;
            } else {
                TypeInfo* t = TypeInitSimple(mem, TypeDefaultFloat());
                result->typeInfo = t;
            }
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_STRING_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TYPE_STRING);
            result->typeInfo = t;
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_BOOL_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TYPE_BOOL);
            result->typeInfo = t;
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_SYMBOL: {
            // look up in the constants or symbols and find the type
            String id = expr->expr.SYMBOL.identifier;

            ConstResult res1 = findConstant(constants, id);
            if(res1.err) {
                TypeResult res2 = findVariableType(scope, id);
                if(res2.err) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Undefined symbol: "STR_FMT, STR_PRINT(id));
                }
                result->typeInfo = res2.typeInfo;
            } else {
                Expression converted = ConstValueToExpression(mem, res1.value);
                memcpy(result, &converted, sizeof(Expression));
                result->typeInfo = res1.value.typeInfo;
            }
        } break;
        case ExpressionType_FUNCTION_CALL: {
            // look up in the constants or symbols and find the definition
            String id = expr->expr.FUNCTION_CALL.identifier;
            Array(ExpressionPtr) args = expr->expr.FUNCTION_CALL.args;

            TypeResult res = typecheckFindFunctionType(scope, id);
            if(res.err) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Undefined symbol used as function call: "STR_FMT, STR_PRINT(id));
            }

            TypeInfo* funcInfo = res.typeInfo;
            if(funcInfo->functionInfo.args.size != args.size) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Incorrect number of arguments for funnction call, provided: %llu, expected: %llu", args.size, funcInfo->functionInfo.args.size);
            }

            Array(TypecheckedExpressionPtr) typecheckedArgs = {0};
            for(u64 i = 0; i < args.size; ++i) {
                FunctionArg fnArg = funcInfo->functionInfo.args.data[i];
                Expression* providedArg = args.data[i];
                TypeInfo* expectedArgType = fnArg.type;

                TypecheckedExpression* providedArgType = typecheckExpression(mem, providedArg, scope, constants, expectedArgType);
                if(!TypeMatch(expectedArgType, providedArgType->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "provided argument number %llu, of function "STR_FMT" has incompatible type \""STR_FMT"\", with expected type \""STR_FMT"\"",
                        i + 1,
                        STR_PRINT(id),
                        STR_PRINT(TypeToString(mem, providedArgType->typeInfo)),
                        STR_PRINT(TypeToString(mem, expectedArgType))
                    );
                }

                ArrayAppend(typecheckedArgs, providedArgType);
            }
            result->expr.FUNCTION_CALL.args = typecheckedArgs; // NOTE: this needs to be set here becouse the types are different

            result->typeInfo = funcInfo->functionInfo.returnType;
            // NOTE: for now all function calls are non constant values, but it really depends on:
            //       - is the funtion being called a pure funtion
            //       - are all the funtion parameters constant values
            result->typeInfo->isConstant = FALSE;
        } break;
        case ExpressionType_FUNCTION_LIT: {
            FunctionInfo fnInfo = expr->expr.FUNCTION_LIT.typeInfo;
            // Scope* functionScope = expr->expr.FUNCTION_LIT.scope;

            result->typeInfo->symbolType = TYPE_FUNCTION;
            result->typeInfo->functionInfo = fnInfo;
            result->typeInfo->isConstant = TRUE;
            // TODO: this is where the function should be added to global scope for codegen
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            // 1. verify the type binary operation can be performed on the types
            // 2. get the type after the operation is performed

            Token op = expr->expr.BINARY_EXPRESSION.operator;
            Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
            Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;

            TypecheckedExpression* lhsType = 0;
            TypecheckedExpression* rhsType = 0;
            if(
                (lhs->type == ExpressionType_INT_LIT || lhs->type == ExpressionType_FLOAT_LIT) &&
                !(rhs->type == ExpressionType_INT_LIT || rhs->type == ExpressionType_FLOAT_LIT)
            ) {
                // lhs needs to be cast
                rhsType = typecheckExpression(mem, rhs, scope, constants, expected);
                lhsType = typecheckExpression(mem, lhs, scope, constants, rhsType->typeInfo);
            } else if(
                !(lhs->type == ExpressionType_INT_LIT || lhs->type == ExpressionType_FLOAT_LIT) &&
                (rhs->type == ExpressionType_INT_LIT || rhs->type == ExpressionType_FLOAT_LIT)
            ) {
                // rhs needs to be cast
                lhsType = typecheckExpression(mem, lhs, scope, constants, expected);
                rhsType = typecheckExpression(mem, rhs, scope, constants, lhsType->typeInfo);
            } else {
                lhsType = typecheckExpression(mem, lhs, scope, constants, expected);
                rhsType = typecheckExpression(mem, rhs, scope, constants, expected);
            }

            // TODO: static assert on the number of operators
            TypeInfo* typeBool = TypeInitSimple(mem, TYPE_BOOL);
            if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_ADD) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_SUB) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_MUL) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_DIV) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_LESS) {
                result->typeInfo = typeBool;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_GREATER) {
                result->typeInfo = typeBool;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_LESS_EQ) {
                result->typeInfo = typeBool;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_GREATER_EQ) {
                result->typeInfo = typeBool;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_COMPARISON) {
                result->typeInfo = typeBool;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_NOT_EQUALS) {
                result->typeInfo = typeBool;
            } else {
                ERROR_VA(op.loc, "Invalid binary operation: %s "STR_FMT" %s", TypeStr[lhsType->typeInfo->symbolType], STR_PRINT(op.value), TypeStr[rhsType->typeInfo->symbolType]);
            }

            result->typeInfo->isConstant = lhsType->typeInfo->isConstant && rhsType->typeInfo->isConstant;
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            // 1. verify the type unary operation can be performed on the type of the value
            // 2. get the type after the operation is performed

            Token op = expr->expr.UNARY_EXPRESSION.operator;
            Expression* opExpr = expr->expr.UNARY_EXPRESSION.expr;
            TypecheckedExpression* typeInfo = typecheckExpression(mem, opExpr, scope, constants, expected);

            // TODO: static assert on the number of operators
            if(TypeIsNumber(typeInfo->typeInfo) && op.type == TokenType_SUB) {
                result->typeInfo = typeInfo->typeInfo;
            }

            result->typeInfo->isConstant = typeInfo->typeInfo->isConstant;
        } break;
    }

    return result;
}

typedef struct FunctionsInScope {
    Array(String) names;
    Array(ExpressionPtr) values;
} FunctionsInScope;

FunctionsInScope typecheckProcessConsts(Arena* mem, Scope scope, Hashmap(String, ConstValue)* target) {
    FunctionsInScope result = {0};

    Hashmap(String, ExpressionPtr)* constants;
    switch(scope.type) {
        case ScopeType_NONE: {
            UNREACHABLE("ScopeType_NONE is invalid type: cannot eval consts");
        } break;
        case ScopeType_GLOBAL: {
            constants = &scope.scope.as_global->constants;
        } break;
        case ScopeType_GENERIC: {
            constants = &scope.scope.as_generic->constants;
        } break;
    }

#define OTHER_BUCKET (currentIt == 1 ? 0 : 1)
#define CURRENT_BUCKET (currentIt)
    u64 currentIt = 0;

    Array(String) deferedNames[2] = {0};
    Array(ExpressionPtr) deferedExprs[2] = {0};

    // initial processing of consts
    HashmapFor(String, ExpressionPtr, it, constants) {
        String key = it->key;
        Expression* value = it->value;

        if(value->type == ExpressionType_FUNCTION_LIT) {
            ArrayAppend(result.names, key);
            ArrayAppend(result.values, value);
            continue;
        }

        EvaluateConstantResult res = evaluateConstant(value, mem, target);
        if(res.error == ConstantEvaluationError_UNDEFINED_SYMBOL) {
            // add to other bucket, defer
            ArrayAppend(deferedNames[CURRENT_BUCKET], key);
            ArrayAppend(deferedExprs[CURRENT_BUCKET], value);
        } else {
            if(!HashmapSet(String, ConstValue)(target, key, res.val)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->capacity, target->size, STR_PRINT(key));
            }
        }
    }

    // resolve unordered dependencies in consts
    u64 processedCount = 0;
    for(;;) {
        for(u64 i = 0; i < deferedNames[CURRENT_BUCKET].size; ++i) {
            String id = deferedNames[CURRENT_BUCKET].data[i];
            Expression* expr = deferedExprs[CURRENT_BUCKET].data[i];

            EvaluateConstantResult res = evaluateConstant(expr, mem, target);
            if(res.error == ConstantEvaluationError_UNDEFINED_SYMBOL) {
                // add to other bucket, defer
                ArrayAppend(deferedNames[OTHER_BUCKET], id);
                ArrayAppend(deferedExprs[OTHER_BUCKET], expr);
            } else {
                processedCount += 1;
                if(!HashmapSet(String, ConstValue)(target, id, res.val)) {
                    UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->capacity, target->size, STR_PRINT(id));
                }
            }
        }

        if(deferedNames[OTHER_BUCKET].size == 0) break;
        if(processedCount == 0) {
            Location loc = {0}; // TODO: fix
            ERROR(loc, "Circular dependencies in constants");
        }

        // empty current buckets
        deferedNames[CURRENT_BUCKET].size = 0;
        deferedExprs[CURRENT_BUCKET].size = 0;

        // swap the two buckets
        if(currentIt == 0) currentIt = 1;
        else if(currentIt == 1) currentIt = 0;
        processedCount = 0;
    }

    free(deferedNames->data);
    free(deferedExprs->data);
    return result;

#undef OTHER_BUCKET
#undef CURRENT_BUCKET
}

void typecheckFunctions(Arena* mem, Hashmap(String, ConstValue)* constants, TypecheckedScope* parent, FunctionsInScope functions) {
    for(u64 i = 0; i < functions.names.size; ++i) {
        String id = functions.names.data[i];
        Expression* expr = functions.values.data[i];
        assert(expr->type == ExpressionType_FUNCTION_LIT, "Expected function lit in typecheckFunctions");

        GenericScope* fnScope = expr->expr.FUNCTION_LIT.scope;
        TypeInfo* returnType = expr->expr.FUNCTION_LIT.typeInfo.returnType;

        TypeInfo* fnTypeInfo = TypeInitSimple(mem, TYPE_FUNCTION);
        fnTypeInfo->functionInfo = expr->expr.FUNCTION_LIT.typeInfo;

        TypecheckedScope* resultScope = typecheckScope(mem, fnScope, parent, constants, returnType);

        ConstValue asd = {0};
        asd.typeInfo = fnTypeInfo;
        asd.as_function = resultScope;
        saveFunction(parent, constants, id, asd);
    }
}

// NOTE: this was the messege left, where we checked is a scope is top level or not:
            // NOTE: temporarily removed StatementType_VAR_DECL_ASSIGN as a valid top level statement
            //       global variables need to be a known value at compile time because we set them by writing the value into the output file
            //       currently we dont have a way to store this information
            // NOTE: should probably add a TypecheckedExpression, to solve this issue
            // NOTE: StatementType_DIRECTIVE is temporary until compiler directives are formalized

// expectedReturnType is used for typechecking return statement
// constants is a hashmap of evaluated consts that was declared in a parent scope
TypecheckedScope* typecheckScope(Arena* mem, GenericScope* scope, TypecheckedScope* parent, Hashmap(String, ConstValue)* constants, TypeInfo* expectedReturnType) {
    TypecheckedScope* result = TypecheckedScopeInit(mem, parent);
    Hashmap(String, ConstValue) constantsInThisScope = {0}; // these are the constants declared in this scope
    HashmapInit(constantsInThisScope, 0x100); // TODO: better default size
    
    HashmapFor(String, ConstValue, it, constants) {
        String key = it->key;
        ConstValue value = it->value;

        if(!HashmapSet(String, ConstValue)(&constantsInThisScope, key, value)) {
            UNREACHABLE("failed to instert into hashmap");
        }
    }

    // evaluate constants
    FunctionsInScope functions = typecheckProcessConsts(mem, makeScopeFromGeneric(scope), &constantsInThisScope);

    // infer the types in all the statements
    for(u64 i = 0; i < scope->statements.size; ++i) {
        Statement* statement = scope->statements.data[i];

        switch(statement->type) {
            case StatementType_NONE:
            case StatementType_COUNT: {
                UNREACHABLE("invalid node types");
            } break;

            case StatementType_VAR_CONST: break;
            case StatementType_VAR_DECL: {
                String id = statement->statement.VAR_DECL.identifier;
                TypeInfo* type = statement->statement.VAR_DECL.type;

                saveVariable(result, &constantsInThisScope, id, type);
            } break;
            case StatementType_VAR_DECL_ASSIGN: {
                String id = statement->statement.VAR_DECL_ASSIGN.identifier;
                Expression* expr = statement->statement.VAR_DECL_ASSIGN.expr;
                TypeInfo* type = statement->statement.VAR_DECL_ASSIGN.type;

                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, type);
                if(inferedType->typeInfo->symbolType == TYPE_VOID) {
                    Location loc = {0}; // TODO: fix location
                    ERROR(loc, "Cannot assign void type");
                }

                if(type->symbolType == TYPE_NONE) {
                    type = inferedType->typeInfo;
                } else {
                    if(!TypeMatch(type, inferedType->typeInfo)) {
                        Location loc = {0}; // TODO: fix location
                        ERROR_VA(
                            loc,
                            "Type mismatch, trying to assign to type: "STR_FMT", expression type: "STR_FMT,
                            STR_PRINT(TypeToString(mem, type)),
                            STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                        );
                    }
                }

                saveVariable(result, &constantsInThisScope, id, type);

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = inferedType;
                ArrayAppend(result->statements, typechecked);
            } break;
            case StatementType_VAR_REASSIGN: {
                String id = statement->statement.VAR_REASSIGN.identifier;
                Expression* expr = statement->statement.VAR_REASSIGN.expr;

                // find variable
                TypeResult expectedRes = findVariableType(result, id);
                if(expectedRes.err) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Trying to assign to an undefined variable: "STR_FMT, STR_PRINT(id));
                }
                TypeInfo* expectedType = expectedRes.typeInfo;
                TypecheckedExpression* inferedType  = typecheckExpression(mem, expr, result, &constantsInThisScope, expectedType);

                if(!TypeMatch(expectedType, inferedType->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Type mismatch in assignment, expected: "STR_FMT", but got: "STR_FMT,
                        STR_PRINT(TypeToString(mem, expectedType)),
                        STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = inferedType;
                ArrayAppend(result->statements, typechecked);
            } break;
            case StatementType_RET: {
                Expression* expr = statement->statement.RET.expr;
                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, expectedReturnType);
                // TODO: what do when returning from a void func, typecheck expression will seg fault

                if(!TypeMatch(expectedReturnType, inferedType->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Type mismatch in return, function expected expected: "STR_FMT", but got: "STR_FMT,
                        STR_PRINT(TypeToString(mem, expectedReturnType)),
                        STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.RET.expr = inferedType;
                ArrayAppend(result->statements, typechecked);
            } break;
            case StatementType_IF: {
                Array(ConditionalBlock) blocks = statement->statement.IF.blocks;
                GenericScope* elze = statement->statement.IF.elze;
                bool hasElse = statement->statement.IF.hasElse;

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.IF.hasElse = hasElse;

                for(u64 h = 0; h < blocks.size; ++h) {
                    ConditionalBlock block = blocks.data[h];
                    Expression* blockExpr = block.expr;
                    GenericScope* blockScope = block.scope;

                    TypeInfo* boolType = TypeInitSimple(mem, TYPE_BOOL);
                    TypecheckedExpression* inferedType = typecheckExpression(mem, blockExpr, result, &constantsInThisScope, boolType);
                    if(!TypeMatch(boolType, inferedType->typeInfo)) {
                        Location loc = {0}; // TODO: fix loc
                        ERROR_VA(
                            loc,
                            "Expression has incorrect type, if conditions need to have bool type, but got: "STR_FMT,
                            STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                        );
                    }

                    TypechekedConditionalBlock typecheckedBlock = {0};
                    typecheckedBlock.scope = typecheckScope(mem, blockScope, result, &constantsInThisScope, expectedReturnType);
                    typecheckedBlock.expr = inferedType;
                    ArrayAppend(typechecked.node.IF.blocks, typecheckedBlock);
                }

                if(hasElse) {
                    typechecked.node.IF.elze = typecheckScope(mem, elze, result, &constantsInThisScope, expectedReturnType);
                }

                ArrayAppend(result->statements, typechecked);
            } break;
            case StatementType_LOOP: {
                Expression* expr = statement->statement.LOOP.expr;
                GenericScope* loopScope = statement->statement.LOOP.scope;

                TypeInfo* boolType = TypeInitSimple(mem, TYPE_BOOL);
                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, boolType);
                if(!(TypeMatch(boolType, inferedType->typeInfo) || TypeIsNumber(inferedType->typeInfo))) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Expression has incorrect type, loop expressions need to have bool or numeric type, but got: "STR_FMT,
                        STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.LOOP.expr = inferedType;
                typechecked.node.LOOP.scope = typecheckScope(mem, loopScope, result, &constantsInThisScope, expectedReturnType);
                ArrayAppend(result->statements, typechecked);
            } break;
            case StatementType_EXPRESSION: {
                Expression* expr = statement->statement.EXPRESSION.expr;
                TypecheckedExpression* typeInfo = typecheckExpression(mem, expr, result, &constantsInThisScope, NULL);
                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.EXPRESSION.expr = typeInfo;
                ArrayAppend(result->statements, typechecked);
            } break;

            case StatementType_DIRECTIVE: break;
        }
    }

    // typecheck all the function bodies
    typecheckFunctions(mem, &constantsInThisScope, result, functions);

    free(functions.names.data);
    free(functions.values.data);

    free(constantsInThisScope.pairs);
    return result;
}

TypecheckedScope* typecheckGlobalScope(Arena* mem, GlobalScope* scope) {
    TypecheckedScope* result = TypecheckedScopeInit(mem, NULL);
    
    // initial processing of consts
    Hashmap(String, ConstValue) constants = {0};
    HashmapInit(constants, 0x100); // TODO: better default size
    FunctionsInScope functions = typecheckProcessConsts(mem, makeScopeFromGlobal(scope), &constants);

    // typecheck functions
    typecheckFunctions(mem, &constants, result, functions);

    free(functions.names.data);
    free(functions.values.data);

    return result;
}

TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult) {
    GlobalScope* globalScope = parseResult->globalScope;
    return typecheckGlobalScope(mem, globalScope);
}

/*
typecheckTopLevelScope :: () {
    // iterate statements
    // only valid top level statements are constants and variable decl or decl and assing
    // evaluate all the constants and save the indecies of functions
    // infer the types of all the variables - global
    // iterate all the function bodies and call typecheckScope
}

typecheckScope :: () {
    // iterate statements
    // all statements are valid
    // evaluate all the constants and save the indecies of functions
    // infer the types of all the variables
    // iterate all the function bodies, and children scopes and call typecheckScope
}
*/

// TODO: function variables not done fully, their bodies never get typechecked and as a result they never get generated
