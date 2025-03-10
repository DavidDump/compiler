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
        UNREACHABLE("invalid unary operator");
    }

    return value;
}

void saveVariable(TypecheckedScope* scope, String id, TypeInfo* type) {
    // check for redefinition
    TypecheckedScope* it = scope;
    while(it) {
        for(u64 i = 0; i < it->params.size; ++i) {
            StringAndType arg = it->params.data[i];
            if(StringEquals(arg.id, id)) {
                Location loc = {0}; // TODO: fix
                ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
            }
        }

        TypeInfo* tmp1 = {0};
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, id, &tmp1)) {
            Location loc = {0}; // TODO: fix
            ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
        }

        ConstValue tmp2 = {0};
        if(HashmapGet(String, ConstValue)(&it->constants, id, &tmp2)) {
            Location loc = {0}; // TODO: fix
            ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
        }

        it = it->parent;
    }

    // save the symbol
    if(!HashmapSet(String, TypeInfoPtr)(&scope->variables, id, type)) {
        UNREACHABLE("Failed to insert into hashmap");
    }
}

void saveConstant(TypecheckedScope* scope, String id, ConstValue val) {
    // check for redefinition
    TypecheckedScope* it = scope;
    while(it) {
        for(u64 i = 0; i < it->params.size; ++i) {
            StringAndType arg = it->params.data[i];
            if(StringEquals(arg.id, id)) {
                Location loc = {0}; // TODO: fix
                ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
            }
        }

        TypeInfo* tmp1 = {0};
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, id, &tmp1)) {
            Location loc = {0}; // TODO: fix
            ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
        }

        ConstValue tmp2 = {0};
        if(HashmapGet(String, ConstValue)(&it->constants, id, &tmp2)) {
            Location loc = {0}; // TODO: fix
            ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
        }

        it = it->parent;
    }


    // save the symbol
    if(!HashmapSet(String, ConstValue)(&scope->constants, id, val)) {
        UNREACHABLE("Failed to insert into hashmap");
    }
}

TypecheckedScope* TypecheckedScopeInit(Arena* mem, TypecheckedScope* parent) {
    TypecheckedScope* result = arena_alloc(mem, sizeof(TypecheckedScope));
    result->parent = parent;
    HashmapInit(result->constants, 0x100); // TODO: init size
    HashmapInit(result->variables, 0x100); // TODO: init size
    return result;
}

EvaluateConstantResult evaluateConstant(Expression* expr, Arena* mem, Hashmap(String, ConstValue)* evaluatedConstatants) {
    EvaluateConstantResult result1 = {0};
    ConstValue result = {0};

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            result.typeInfo = TypeInitSimple(mem, TYPE_S64);
            result.as_s64 = StringToS64(expr->expr.INT_LIT.value);
        } break;
        case ExpressionType_FLOAT_LIT: {
            result.typeInfo = TypeInitSimple(mem, TYPE_F64);
            u64 fractPartLen = expr->expr.FLOAT_LIT.fractPart.length;
            s64 wholePart = StringToS64(expr->expr.FLOAT_LIT.wholePart);
            s64 fractPart = StringToS64(expr->expr.FLOAT_LIT.fractPart);

            result.as_f64 = ((f64)wholePart) + ((f64)fractPart / (10 * fractPartLen));
        } break;
        case ExpressionType_STRING_LIT: {
            result.typeInfo = TypeInitSimple(mem, TYPE_STRING);
            result.as_String = expr->expr.STRING_LIT.value;
        } break;
        case ExpressionType_BOOL_LIT: {
            result.typeInfo = TypeInitSimple(mem, TYPE_BOOL);
            if(StringEqualsCstr(expr->expr.BOOL_LIT.value, "true")) result.as_bool = TRUE;
            else if(StringEqualsCstr(expr->expr.BOOL_LIT.value, "false")) result.as_bool = FALSE;
        } break;
        case ExpressionType_SYMBOL: {
            String key = expr->expr.SYMBOL.identifier;
            ConstValue val = {0};
            if(!HashmapGet(String, ConstValue)(evaluatedConstatants, key, &val)) {
                // not evaluated yet
                result1.error = ConstantEvaluationError_UNDEFINED_SYMBOL;
            } else {
                result = val;
            }
        } break;
        case ExpressionType_FUNCTION_CALL: {
            Location loc = {.filename = STR(""), .collum = 1, .line = 1}; // TODO: fix loc
            ERROR(loc, "Function call cannot be used in a constant, use #run");
        } break;
        case ExpressionType_FUNCTION_LIT: {
            // NOTE: this case is technically not an error we just want to propagate to the caller that this was a function literal
            result1.error = ConstantEvaluationError_FUNCTION_LITERAL;

            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
            bool isExtern = expr->expr.FUNCTION_LIT.isExtern;

            TypecheckedScope* resultScope = TypecheckedScopeInit(mem, NULL); // NOTE: later set the scope correcly
            Array(StringAndType) tmp = {0};
            for(u64 i = 0; i < args.size; ++i) {
                FunctionArg arg = args.data[i];
                String argId = arg.id;
                TypeInfo* argType = arg.type;

                StringAndType asd = {.id = argId, .type = argType};
                ArrayAppend(resultScope->params, asd);
                ArrayAppend(tmp, asd);
            }

            result.typeInfo = TypeInitSimple(mem, TYPE_FUNCTION);
            result.typeInfo->functionInfo.isExternal = isExtern;
            result.typeInfo->functionInfo.args = tmp;
            result.typeInfo->functionInfo.returnType = returnType;
            result.as_function = resultScope;
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            EvaluateConstantResult lhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.lhs, mem, evaluatedConstatants);
            ConstValue lhs = lhsRes.val;
            if(lhsRes.error) {
                result1.error = TRUE;
                break;
            }

            EvaluateConstantResult rhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.rhs, mem, evaluatedConstatants);
            ConstValue rhs = rhsRes.val;
            if(rhsRes.error) {
                result1.error = TRUE;
                break;
            }

            Token op = expr->expr.BINARY_EXPRESSION.operator;
            if(lhs.typeInfo->symbolType == TYPE_FUNCTION || rhs.typeInfo->symbolType == TYPE_FUNCTION) {
                ERROR_VA(op.loc, "Cannot perform binary operation: "STR_FMT" on a function literal", STR_PRINT(op.value));
            }

            result = evaluateBinaryExpression(lhs, op, rhs);
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            EvaluateConstantResult valRes = evaluateConstant(expr->expr.UNARY_EXPRESSION.expr, mem, evaluatedConstatants);
            ConstValue val = valRes.val;
            if(valRes.error) {
                result1.error = TRUE;
                break;
            }

            Token op = expr->expr.UNARY_EXPRESSION.operator;
            if(val.typeInfo->symbolType == TYPE_FUNCTION) {
                ERROR_VA(op.loc, "Cannot perform unary operation: "STR_FMT" on a function literal", STR_PRINT(op.value));
            }

            result = evaluateUnaryExpression(op, val);
        } break;
    }

    result1.val = result;
    return result1;
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
            
            Array(FunctionArg) args = {0};
            for(u64 i = 0; i < value.typeInfo->functionInfo.args.size; ++i) {
                StringAndType dat = value.typeInfo->functionInfo.args.data[i];
                FunctionArg arg = {.id = dat.id, .type = dat.type};
                ArrayAppend(args, arg);
            }
            
            result.expr.FUNCTION_LIT.args = args;
            result.expr.FUNCTION_LIT.isExtern = value.typeInfo->functionInfo.isExternal;
            result.expr.FUNCTION_LIT.returnType = value.typeInfo->functionInfo.returnType;
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

ConstResult findConstant(TypecheckedScope* scope, String identifier) {
    TypecheckedScope* it = scope;
    while(it) {
        ConstValue val = {0};
        if(HashmapGet(String, ConstValue)(&it->constants, identifier, &val)) {
            return (ConstResult){.value = val};
        }

        it = it->parent;
    }

    return (ConstResult){.err = TRUE};
}

// symbol means both constant and variable
TypeResult findSymbolType(TypecheckedScope* scope, String identifier) {
    TypecheckedScope* it = scope;
    while(it) {
        for(u64 i = 0; i < it->params.size; ++i) {
            StringAndType arg = it->params.data[i];
            if(StringEquals(arg.id, identifier)) return (TypeResult){.typeInfo = arg.type};
        }

        ConstValue val = {0};
        if(HashmapGet(String, ConstValue)(&it->constants, identifier, &val)) {
            return (TypeResult){.typeInfo = val.typeInfo};
        }

        TypeInfo* info = {0};
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, identifier, &info)) {
            return (TypeResult){.typeInfo = info};
        }
        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

TypeResult findVariableType(TypecheckedScope* scope, String identifier) {
    TypecheckedScope* it = scope;
    while(it) {
        for(u64 i = 0; i < it->params.size; ++i) {
            StringAndType arg = it->params.data[i];
            if(StringEquals(arg.id, identifier)) return (TypeResult){.typeInfo = arg.type};
        }

        TypeInfo* info = {0};
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, identifier, &info)) {
            return (TypeResult){.typeInfo = info};
        }
        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

TypecheckedExpression* typecheckExpression(Arena* mem, Expression* expr, TypecheckedScope* scope) {
    TypecheckedExpression* result = arena_alloc(mem, sizeof(TypecheckedExpression));
    memcpy(result, expr, sizeof(Expression));
    // NOTE: this has a high likelyhood of fucking me in the ass later on

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TypeDefaultInt());
            result->typeInfo = t;
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_FLOAT_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TypeDefaultFloat());
            result->typeInfo = t;
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

            ConstResult res1 = findConstant(scope, id);
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

            TypeResult res = findSymbolType(scope, id);
            if(res.err) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Undefined symbol used as function call: "STR_FMT, STR_PRINT(id));
            }

            TypeInfo* funcInfo = res.typeInfo;
            if(funcInfo->symbolType != TYPE_FUNCTION) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Trying to use non function symbol \""STR_FMT"\" as function", STR_PRINT(id));
            }

            if(funcInfo->functionInfo.args.size != args.size) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Incorrect number of arguments for funnction call, provided: %llu, expected: %llu", args.size, funcInfo->functionInfo.args.size);
            }

            Array(TypecheckedExpressionPtr) typecheckedArgs = {0};
            for(u64 i = 0; i < args.size; ++i) {
                StringAndType fnArg = funcInfo->functionInfo.args.data[i];
                Expression* providedArg = args.data[i];
                TypeInfo* expectedArgType = fnArg.type;
                TypecheckedExpression* providedArgType = typecheckExpression(mem, providedArg, scope);

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
            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
            bool isExtern = expr->expr.FUNCTION_LIT.isExtern;
            // Scope* functionScope = expr->expr.FUNCTION_LIT.scope;

            // TODO: deduplicate, FUNCTION_LIT and FunctionInfo are basically the same struct
            Array(StringAndType) tmp = {0};
            for(u64 i = 0; i < args.size; ++i) {
                FunctionArg arg = args.data[i];
                String argId = arg.id;
                TypeInfo* argType = arg.type;
                StringAndType asd = {.id = argId, .type = argType};
                ArrayAppend(tmp, asd);
            }

            result->typeInfo->symbolType = TYPE_FUNCTION;
            result->typeInfo->functionInfo.args = tmp;
            result->typeInfo->functionInfo.isExternal = isExtern;
            result->typeInfo->functionInfo.returnType = returnType;
            result->typeInfo->isConstant = TRUE;
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            // 1. verify the type binary operation can be performed on the types
            // 2. get the type after the operation is performed

            Token op = expr->expr.BINARY_EXPRESSION.operator;
            Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
            Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;
            TypecheckedExpression* lhsType = typecheckExpression(mem, lhs, scope);
            TypecheckedExpression* rhsType = typecheckExpression(mem, rhs, scope);

            // TODO: static assert on the number of operators
            if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_ADD) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_SUB) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_MUL) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_DIV) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_LESS) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_GREATER) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_LESS_EQ) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsNumber(lhsType->typeInfo) && TypeIsNumber(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_GREATER_EQ) {
                result->typeInfo = lhsType->typeInfo;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_COMPARISON) {
                result->typeInfo = lhsType->typeInfo;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_NOT_EQUALS) {
                result->typeInfo = lhsType->typeInfo;
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
            TypecheckedExpression* typeInfo = typecheckExpression(mem, opExpr, scope);

            // TODO: static assert on the number of operators
            if(TypeIsNumber(typeInfo->typeInfo) && op.type == TokenType_SUB) {
                result->typeInfo = typeInfo->typeInfo;
            }

            result->typeInfo->isConstant = typeInfo->typeInfo->isConstant;
        } break;
    }

    return result;
}

// expectedReturnType is used for typechecking return statement
// target is the scope to parse the statements into
TypecheckedScope* typecheckScope2(Arena* mem, Scope* scope, TypecheckedScope* target, TypeInfo* expectedReturnType, bool isTopLevel) {
#define OTHER_BUCKET (currentIt == 1 ? 0 : 1)
#define CURRENT_BUCKET (currentIt)
    TypecheckedScope* result = target;
    
    // evaluate constants
    Array(ASTNodePtr) buckets[2] = {0};
    for(u64 i = 0; i < scope->statements.size; ++i) {
        ASTNode* statement = scope->statements.data[i];
        if(isTopLevel) {
            // NOTE: temporarily removed ASTNodeType_VAR_DECL_ASSIGN as a valid top level statement
            //       global variables need to be a known value at compile time because we set them by writing the value into the output file
            //       currently we dont have a way to store this information
            // NOTE: should probably add a TypecheckedExpression, to solve this issue
            // NOTE: ASTNodeType_DIRECTIVE is temporary until compiler directives are formalized
            if(!(statement->type == ASTNodeType_VAR_CONST || statement->type == ASTNodeType_VAR_DECL || statement->type == ASTNodeType_DIRECTIVE /* || statement->type == ASTNodeType_VAR_DECL_ASSIGN */)) {
                Location loc = {0}; // TODO: fix
                ERROR_VA(loc, "Invalid toplevel statments: %s", ASTNodeTypeStr[statement->type]);
            }
        }
        ArrayAppend(buckets[0], statement);
    }

    // NOTE: these are the functions that need to be typechecked once the current scope is done
    Array(ExpressionPtr) fnScopes = {0};
    Array(String) fnNames = {0};
    Array(ConstValue) fnValues = {0};

    u64 currentIt = 0;
    u64 processedCount = 0;
    while(TRUE) {
        for(u64 i = 0; i < buckets[CURRENT_BUCKET].size; ++i) {
            ASTNode* statement = buckets[CURRENT_BUCKET].data[i];

            if(statement->type != ASTNodeType_VAR_CONST) continue;

            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            EvaluateConstantResult res = evaluateConstant(expr, mem, &result->constants);

            if(res.error == ConstantEvaluationError_UNDEFINED_SYMBOL) {
                // add to other bucket
                ArrayAppend(buckets[OTHER_BUCKET], statement);
            } else if(res.error == ConstantEvaluationError_FUNCTION_LITERAL) {
                // record the function scope for later typechecking
                assert(expr->type == ExpressionType_FUNCTION_LIT, "only function literal here");
                ArrayAppend(fnScopes, expr);
                ArrayAppend(fnNames, id);
                ArrayAppend(fnValues, res.val);

                saveConstant(result, id, res.val);
                // TODO: should there be a `processedCount++;` here? how could that fuck up

                if(!StringEquals(result->constants.last->key, id)) UNREACHABLE("The last inserted element is not the same as the last linked list element");
            } else {
                saveConstant(result, id, res.val);
                processedCount++;
            }
        }

        if(buckets[OTHER_BUCKET].size == 0) break;
        if(processedCount == 0) {
            Location loc = {0}; // TODO: fix
            ERROR(loc, "Circular dependencies in constants");
        }

        // empty current bucket
        buckets[CURRENT_BUCKET].size = 0;

        // swap the two buckets
        if(currentIt == 0) currentIt = 1;
        else if(currentIt == 1) currentIt = 0;
        processedCount = 0;
    }

    // infer the types in all the statements
    for(u64 i = 0; i < scope->statements.size; ++i) {
        // NOTE: only the correct ASTNodeTypes should remain here in case of a top level scope, we error out before here if not
        ASTNode* statement = scope->statements.data[i];

        switch(statement->type) {
            case ASTNodeType_NONE:
            case ASTNodeType_COUNT: {
                UNREACHABLE("invalid node types");
            } break;

            case ASTNodeType_VAR_CONST: break;
            case ASTNodeType_VAR_DECL: {
                String id = statement->node.VAR_DECL.identifier;
                TypeInfo* type = statement->node.VAR_DECL.type;

                saveVariable(result, id, type);
            } break;
            case ASTNodeType_VAR_DECL_ASSIGN: {
                String id = statement->node.VAR_DECL_ASSIGN.identifier;
                Expression* expr = statement->node.VAR_DECL_ASSIGN.expr;
                TypeInfo* type = statement->node.VAR_DECL_ASSIGN.type;

                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result);
                if(inferedType->typeInfo->symbolType == TYPE_VOID) {
                    Location loc = {0}; // TODO: fix location
                    ERROR(loc, "Cannot assign void type");
                }

                if(type->symbolType == TYPE_VOID) {
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

                saveVariable(result, id, type);

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = inferedType;
                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_VAR_REASSIGN: {
                String id = statement->node.VAR_REASSIGN.identifier;
                Expression* expr = statement->node.VAR_REASSIGN.expr;

                // find variable
                TypeResult expectedRes = findVariableType(result, id);
                if(expectedRes.err) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Trying to assign to an undefined variable: "STR_FMT, STR_PRINT(id));
                }
                TypeInfo* expectedType = expectedRes.typeInfo;
                TypecheckedExpression* inferedType  = typecheckExpression(mem, expr, result);

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
            case ASTNodeType_RET: {
                Expression* expr = statement->node.RET.expr;
                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result);
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
            case ASTNodeType_IF: {
                Array(ConditionalBlock) blocks = statement->node.IF.blocks;
                Scope* elze = statement->node.IF.elze;
                bool hasElse = statement->node.IF.hasElse;

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.IF.hasElse = hasElse;

                for(u64 h = 0; h < blocks.size; ++h) {
                    ConditionalBlock block = blocks.data[h];
                    Expression* blockExpr = block.expr;
                    Scope* blockScope = block.scope;

                    TypecheckedExpression* inferedType = typecheckExpression(mem, blockExpr, result);
                    TypeInfo boolType = {0};
                    boolType.symbolType = TYPE_BOOL;
                    if(!TypeMatch(&boolType, inferedType->typeInfo)) {
                        Location loc = {0}; // TODO: fix loc
                        ERROR_VA(
                            loc,
                            "Expression has incorrect type, if conditions need to have bool type, but got: "STR_FMT,
                            STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                        );
                    }

                    TypechekedConditionalBlock typecheckedBlock = {0};
                    typecheckedBlock.scope = typecheckScope(mem, blockScope, result, expectedReturnType, FALSE);
                    typecheckedBlock.expr = inferedType;
                    ArrayAppend(typechecked.node.IF.blocks, typecheckedBlock);
                }

                if(hasElse) {
                    typechecked.node.IF.elze = typecheckScope(mem, elze, result, expectedReturnType, FALSE);
                }

                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_LOOP: {
                Expression* expr = statement->node.LOOP.expr;
                Scope* scope = statement->node.LOOP.scope;

                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result);
                TypeInfo boolType = {0};
                boolType.symbolType = TYPE_BOOL;
                if(!(TypeMatch(&boolType, inferedType->typeInfo) || TypeIsNumber(inferedType->typeInfo))) {
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
                typechecked.node.LOOP.scope = typecheckScope(mem, scope, result, expectedReturnType, FALSE);
                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_EXPRESSION: {
                Expression* expr = statement->node.EXPRESSION.expr;
                TypecheckedExpression* typeInfo = typecheckExpression(mem, expr, result);
                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.EXPRESSION.expr = typeInfo;
                ArrayAppend(result->statements, typechecked);
            } break;

            case ASTNodeType_DIRECTIVE: break;
        }
    }

    // typecheck all the function bodies
    assert(fnScopes.size == fnNames.size, "names size does not match scopes size");
    for(u64 i = 0; i < fnScopes.size; ++i) {
        Expression* expr = fnScopes.data[i];
        String id = fnNames.data[i];
        ConstValue fnValue = fnValues.data[i];

        TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
        Scope* fnScope = expr->expr.FUNCTION_LIT.scope;

        TypecheckedScope* target = fnValue.as_function;
        target->parent = result; // NOTE: the parent doesnt get set in the `evaluateExpression()` call
        fnValue.as_function = typecheckScope2(mem, fnScope, target, returnType, FALSE);

        // TODO: incorrect in case of a hash collision
        u64 index = HashmapHashFunc(String)(id) % result->constants.capacity;
        ArrayAppend(result->functionIndicies, index);
    }

#undef OTHER_BUCKET
#undef CURRENT_BUCKET
    return result;
}

TypecheckedScope* typecheckScope(Arena* mem, Scope* scope, TypecheckedScope* parent, TypeInfo* expectedReturnType, bool isTopLevel) {
    TypecheckedScope* target = TypecheckedScopeInit(mem, parent);
    return typecheckScope2(mem, scope, target, expectedReturnType, isTopLevel);
}

TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult) {
    Scope* globalScope = parseResult->globalScope;
    return typecheckScope(mem, globalScope, NULL, NULL, TRUE);
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
