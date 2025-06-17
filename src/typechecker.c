#include "typechecker.h"
#include "operatorFunctions.h"

#include <string.h>
#include <math.h>

ConstValue evaluateBinaryExpression(ConstValue lhs, Token operator, ConstValue rhs) {
    STATIC_ASSERT(BIN_OPERATORS_COUNT == 13); // binary operator count has changed
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
    } else if(TypeIsInt(lhs.typeInfo) && TypeIsInt(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_AND) {
        return _bin_and(lhs, rhs);
    } else if(TypeIsInt(lhs.typeInfo) && TypeIsInt(rhs.typeInfo) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_OR) {
        return _bin_or(lhs, rhs);
    } else if(((TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) || (TypeIsNumber(rhs.typeInfo) && TypeIsNumber(lhs.typeInfo))) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_COMPARISON) {
        return _equals(lhs, rhs);
    } else if(((TypeIsBool(lhs.typeInfo) && TypeIsBool(rhs.typeInfo)) || (TypeIsNumber(rhs.typeInfo) && TypeIsNumber(lhs.typeInfo))) && TypeMatch(lhs.typeInfo, rhs.typeInfo) && operator.type == TokenType_NOT_EQUALS) {
        return _not_equals(lhs, rhs);
    } else if(operator.type == TokenType_AS && TypeIsType(rhs.typeInfo) && (TypeIsNumber(lhs.typeInfo) && TypeIsNumber(rhs.as_type))) {
        return _as(lhs, rhs);
    } else {
        Arena tmp = {0};
        String lhsType = TypeToString(&tmp, lhs.typeInfo);
        String rhsType = TypeToString(&tmp, rhs.typeInfo);
        ERROR_VA(operator.loc, "Invalid binary operation: "STR_FMT" "STR_FMT" "STR_FMT, STR_PRINT(lhsType), STR_PRINT(operator.value), STR_PRINT(rhsType));
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
            case TYPE_TYPE:
            case TYPE_STRUCT_DEF:
            case TYPE_STRUCT_LIT:
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

void saveParam(TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, String id, TypeInfo* type) {
    // check for redefinition
    if(isSymbolDefined(scope, constants, id)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    }

    // save the symbol
    if(!HashmapSet(String, TypeInfoPtr)(&scope->parameters, id, type)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, scope->parameters.capacity, scope->parameters.size, STR_PRINT(id));
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

void saveStruct(TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, String id, TypeInfo* val) {
    // check for redefinition
    if(isSymbolDefined(scope, constants, id)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of symbol: "STR_FMT, STR_PRINT(id));
    }

    // save the struct
    if(!HashmapSet(String, TypeInfoPtr)(&scope->structs, id, val)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, scope->functions.capacity, scope->functions.size, STR_PRINT(id));
    }
}

ConstResult findConstant(Hashmap(String, ConstValue)* constants, String identifier) {
    ConstValue val = {0};
    if(HashmapGet(String, ConstValue)(constants, identifier, &val)) {
        return (ConstResult){.value = val};
    }

    return (ConstResult){.err = TRUE};
}

TypeResult findStruct(TypecheckedScope* scope, String identifier) {
    TypecheckedScope* it = scope;
    while(it) {
        TypeInfo* val = 0;
        if(HashmapGet(String, TypeInfoPtr)(&it->structs, identifier, &val)) return (TypeResult){.typeInfo = val};
        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

TypeResult typecheckFindFunctionType(TypecheckedScope* scope, String id) {
    TypecheckedScope* it = scope;
    while(it) {
        ConstValue value = {0};
        if(HashmapGet(String, ConstValue)(&it->functions, id, &value)) return (TypeResult){.typeInfo = value.typeInfo};
        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

TypeResult findVariableType(TypecheckedScope* scope, String id) {
    TypeInfo* result = 0;
    TypecheckedScope* it = scope;
    while(it) {
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, id, &result)) return (TypeResult){.typeInfo = result};
        if(HashmapGet(String, TypeInfoPtr)(&it->parameters, id, &result)) return (TypeResult){.typeInfo = result};

        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

TypecheckedScope* TypecheckedScopeInit(Arena* mem, TypecheckedScope* parent) {
    TypecheckedScope* result = arena_alloc(mem, sizeof(TypecheckedScope));
    result->parent = parent;
    HashmapInit(result->functions, 0x100);  // TODO: init size
    HashmapInit(result->variables, 0x100);  // TODO: init size
    HashmapInit(result->parameters, 0x100); // TODO: init size
    HashmapInit(result->structs, 0x100);    // TODO: init size
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
                result.error = TRUE;
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
            UNREACHABLE("invalid type ExpressionType_FUNCTION_LIT and ExpressionType_STRUCT_DEF in evaluateConstant: all functions and structs should be skipped before we get to this part");
        } break;
        case ExpressionType_TYPE: {
            result.val.typeInfo = TypeInitSimple(mem, TYPE_TYPE);
            if(expr->expr.TYPE.type->type == ParsedTypeType_SIMPLE) {
                result.val.as_type = expr->expr.TYPE.type->as_simple.typeInfo;
            } else if(expr->expr.TYPE.type->type == ParsedTypeType_SYMBOL) {
                Token idTok = expr->expr.TYPE.type->as_symbol.identifier;
                ConstResult constRes = findConstant(evaluatedConstatants, idTok.value);
                if(constRes.err) ERROR_VA(idTok.loc, "Undefined symbol: "STR_FMT, STR_PRINT(idTok.value));

                assertf(constRes.value.typeInfo->symbolType == TYPE_TYPE, "expected simple type, got: "STR_FMT, STR_PRINT(TypeToString(mem, constRes.value.typeInfo)));
                result.val = constRes.value;
            } else if(expr->expr.TYPE.type->type == ParsedTypeType_STRUCT) {
                UNREACHABLE("struct");
            } else {
                UNREACHABLE_VA("unknown ParsedTypeType: %i", expr->expr.TYPE.type->type);
            }
        } break;
        case ExpressionType_STRUCT_LIT: {
            UNIMPLEMENTED("Structure literals in constants, ex.: `UP :: Vec3{0, 1, 0};`");

            if(!expr->expr.STRUCT_LIT.idProvided) {
                Location loc = {0}; // TODO: fix loc
                ERROR(loc, "Can only use struct literals with explicit types, in constan symbol declaration");
            }

            Token id = expr->expr.STRUCT_LIT.id;
            ConstValue structDefTypeInfo = {0};
            if(!HashmapGet(String, ConstValue)(evaluatedConstatants, id.value, &structDefTypeInfo)) {
                ERROR_VA(id.loc, "Undeclared struct: "STR_FMT, STR_PRINT(id.value));
            }

            TypeInfo* typeInfo = TypeInitSimple(mem, TYPE_STRUCT_LIT);
            typeInfo->structName = id.value;
            // switch(expr->expr.STRUCT_LIT.type) {
            //     case StructInitializerListType_NONE: UNREACHABLE("StructInitializerListType_NONE invalid here"); break;

            //     case StructInitializerListType_POSITIONAL: {
            //         for(u64 i = 0; i < expr->expr.STRUCT_LIT.positionalInitializerList.size; ++i) {
            //             // TODO: this should check if the expression types match the field types
            //             Expression* initExpr = expr->expr.STRUCT_LIT.positionalInitializerList.data[i];

            //             FunctionArg arg = {0};
            //             arg.
            //             ArrayAppend(typeInfo->structInfo.fields, );
            //         }
            //     } break;
            //     case StructInitializerListType_DESIGNATED: {

            //     } break;
            // }

            // NOTE: these two are the same struct, but defined in two separate places
            STATIC_ASSERT(sizeof(result.val.as_structLit) == sizeof(expr->expr.STRUCT_LIT));
            memcpy(&result.val.as_structLit, &expr->expr.STRUCT_LIT, sizeof(expr->expr.STRUCT_LIT));
            result.val.typeInfo = typeInfo;
        } break;
        case ExpressionType_FIELD_ACCESS: {
            UNIMPLEMENTED("accessing struct literal fields in a constant declaration");
        } break;
        case ExpressionType_ARRAY_ACCESS: {
            UNIMPLEMENTED("indexing arrays in a constant declaration");
        } break;
    }

    return result;
}

// same as `ConstValueToExpression` except it doesnt allocated memory for result, but uses the pointer provided by the caller
TypecheckedExpression* ConstValueToExpressionInto(Arena* mem, ConstValue value, TypecheckedExpression* result) {
    switch(value.typeInfo->symbolType) {
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64: {
            result->type = ExpressionType_INT_LIT;
            result->expr.INT_LIT.value = StringFromU64(mem, value.as_u64);
            result->typeInfo = value.typeInfo;
        } break;
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_S64: {
            if(value.as_s64 < 0) {
                // negative
                s64 invertedNumber = -value.as_s64;
                TypecheckedExpression* num = arena_alloc(mem, sizeof(TypecheckedExpression));
                num->type = ExpressionType_INT_LIT;
                num->expr.INT_LIT.value = StringFromS64(mem, invertedNumber);
                num->typeInfo = value.typeInfo; // TODO: verify if this is correct, or should be `TypeInitSimple(mem, TypeDefaultInt());` instead

                result->type = ExpressionType_UNARY_EXPRESSION;
                result->expr.UNARY_EXPRESSION.operator = (Token){.value = StringFromCstrLit("-"), .type = TokenType_SUB};
                result->expr.UNARY_EXPRESSION.expr = num;
                result->typeInfo = num->typeInfo;
            } else {
                result->type = ExpressionType_INT_LIT;
                result->expr.INT_LIT.value = StringFromS64(mem, value.as_s64);
                result->typeInfo = value.typeInfo;
            }
        } break;
        case TYPE_F32:
        case TYPE_F64: {
            result->type = ExpressionType_FLOAT_LIT;
            f64 whole = 0.0f;
            f64 fract = modf(value.as_f64, &whole);
            result->expr.FLOAT_LIT.wholePart = StringFromF64(mem, whole);
            result->expr.FLOAT_LIT.fractPart = StringFromF64(mem, fract);
            result->typeInfo = value.typeInfo;
        } break;
        case TYPE_STRING: {
            result->type = ExpressionType_STRING_LIT;
            result->expr.STRING_LIT.value = value.as_String;
            result->typeInfo = value.typeInfo;
        } break;
        case TYPE_BOOL: {
            result->type = ExpressionType_BOOL_LIT;
            result->expr.BOOL_LIT.value = (value.as_bool ? StringFromCstrLit("true") : StringFromCstrLit("false"));
            result->typeInfo = value.typeInfo;
        } break;
        case TYPE_FUNCTION: {
            UNIMPLEMENTED("ConstValueToExpression: TYPE_FUNCTION");
            result->type = ExpressionType_FUNCTION_LIT;

            result->expr.FUNCTION_LIT.typeInfo = *value.typeInfo->functionInfo;
            result->typeInfo = value.typeInfo;
            // result.expr.FUNCTION_LIT.scope = ;
        } break;
        case TYPE_TYPE: {
            UNIMPLEMENTED("ConstValueToExpression: TYPE_TYPE");
        } break;
        case TYPE_STRUCT_DEF: {
            UNIMPLEMENTED("ConstValueToExpression: TYPE_STRUCT_DEF");

            // result.expr.STRUCT_DEF.scope = ;
        } break;
        case TYPE_STRUCT_LIT: {
            // NOTE: these two are the same struct, but defined in two separate places
            STATIC_ASSERT(sizeof(value.as_structLit) == sizeof(result->expr.STRUCT_LIT));
            memcpy(&result->expr.STRUCT_LIT, &value.as_structLit, sizeof(value.as_structLit));
            result->typeInfo = value.typeInfo;
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

TypecheckedExpression* ConstValueToExpression(Arena* mem, ConstValue value) {
    TypecheckedExpression* result = arena_alloc(mem, sizeof(TypecheckedExpression));
    return ConstValueToExpressionInto(mem, value, result);
}

// scope is the scope that has to contain the struct definition in case the type is a struct, it also searches parents
TypeInfo* parsedTypeToTypeInfo(ParsedType* type, Hashmap(String, ConstValue)* constants, TypecheckedScope* scope) {
    // assert(expr->type == ExpressionType_TYPE || expr->type == ExpressionType_SYMBOL, "Can only convert type or symbol Expressions to TypeInfo");
    switch(type->type) {
        case ParsedTypeType_NONE: UNREACHABLE("ParsedTypeType_NONE invalid here"); break;
        case ParsedTypeType_SIMPLE: {
            return type->as_simple.typeInfo;
        } break;
        case ParsedTypeType_STRUCT: {
            GlobalScope* structScope = type->as_struct.scope;
            UNUSED(structScope);
            UNIMPLEMENTED("ParsedTypeType_STRUCT in parsedTypeToTypeInfo");
        } break;
        case ParsedTypeType_SYMBOL: {
            Token idToken = type->as_symbol.identifier;
            ConstResult constRes = findConstant(constants, idToken.value);
            if(constRes.err) {
                TypeResult structRes = findStruct(scope, idToken.value);
                if(structRes.err) {
                    ERROR_VA(idToken.loc, "Undefined symbol: "STR_FMT, STR_PRINT(idToken.value));
                }

                return structRes.typeInfo;
            } else {
                ConstValue value = constRes.value;
                if(!TypeIsType(value.typeInfo)) {
                    Arena tmp = {0};
                    ERROR_VA(
                        idToken.loc,
                        "Trying to use none type symbol as type: "STR_FMT": "STR_FMT,
                        STR_PRINT(idToken.value),
                        STR_PRINT(TypeToString(&tmp, value.typeInfo))
                    );
                }

                return value.as_type;
            }
        } break;
    }

    UNREACHABLE("parsedTypeToTypeInfo");
    return 0; // NOTE: to silence warning
}

// expected is the type that the expression should be to pass typechecking,
// can be used when assigning number literals to symbols that have a concrete types,
// this value can be null to just use the default type
TypecheckedExpression* typecheckExpression(Arena* mem, Expression* expr, TypecheckedScope* scope, Hashmap(String, ConstValue)* constants, TypeInfo* expected) {
    TypecheckedExpression* result = arena_alloc(mem, sizeof(TypecheckedExpression));
    memcpy(result, expr, sizeof(Expression));
    STATIC_ASSERT(sizeof(TypecheckedExpression) - sizeof(TypeInfoPtr) == sizeof(Expression));
    // NOTE: this has a high likelyhood of fucking me in the ass later on

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            if(TypeIsInt(expected)) {
                result->typeInfo = expected;
            } else {
                TypeInfo* t = TypeInitSimple(mem, TypeDefaultInt());
                result->typeInfo = t;
            }
        } break;
        case ExpressionType_FLOAT_LIT: {
            if(TypeIsFloat(expected)) {
                result->typeInfo = expected;
            } else {
                TypeInfo* t = TypeInitSimple(mem, TypeDefaultFloat());
                result->typeInfo = t;
            }
        } break;
        case ExpressionType_STRING_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TYPE_STRING);
            result->typeInfo = t;
        } break;
        case ExpressionType_BOOL_LIT: {
            TypeInfo* t = TypeInitSimple(mem, TYPE_BOOL);
            result->typeInfo = t;
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
                ConstValueToExpressionInto(mem, res1.value, result);
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
            if(funcInfo->functionInfo->args.size != args.size) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Incorrect number of arguments for funnction call, provided: %llu, expected: %llu", args.size, funcInfo->functionInfo->args.size);
            }

            Array(TypecheckedExpressionPtr) typecheckedArgs = {0};
            for(u64 i = 0; i < args.size; ++i) {
                FunctionArg fnArg = funcInfo->functionInfo->args.data[i];
                Expression* providedArg = args.data[i];
                TypeInfo* expectedArgType = parsedTypeToTypeInfo(fnArg.type, constants, scope);

                TypecheckedExpression* providedArgType = typecheckExpression(mem, providedArg, scope, constants, expectedArgType);
                if(!TypeMatch(expectedArgType, providedArgType->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Provided argument number %llu, of function \""STR_FMT"\" has incompatible type \""STR_FMT"\", with expected type \""STR_FMT"\"",
                        i + 1,
                        STR_PRINT(id),
                        STR_PRINT(TypeToString(mem, providedArgType->typeInfo)),
                        STR_PRINT(TypeToString(mem, expectedArgType))
                    );
                }

                ArrayAppend(typecheckedArgs, providedArgType);
            }
            result->expr.FUNCTION_CALL.args = typecheckedArgs; // NOTE: this needs to be set here becouse the types are different

            result->typeInfo = parsedTypeToTypeInfo(funcInfo->functionInfo->returnType, constants, scope);
        } break;
        case ExpressionType_FUNCTION_LIT: {
            // NOTE: this is the case of foo := (bar: u8) -> u8 { ... }
            UNIMPLEMENTED("typecheckExpression: ExpressionType_FUNCTION_LIT");
            FunctionInfo* fnInfo = expr->expr.FUNCTION_LIT.typeInfo;
            // Scope* functionScope = expr->expr.FUNCTION_LIT.scope;

            // NOTE: typeInfo is NULL here
            result->typeInfo->symbolType = TYPE_FUNCTION;
            result->typeInfo->functionInfo = fnInfo;
            // TODO: this is where the function should be added to global scope for codegen
        } break;
        #if 0
        case ExpressionType_STRUCT_DEF: {
            GlobalScope* structScope = expr->expr.STRUCT_DEF.scope;
            TypecheckedScope* typechecked = typecheckGlobalScope(mem, structScope, constants);

            Array(FunctionArg) fields = {0};
            HashmapFor(String, TypeInfoPtr, it, &typechecked->variables) {
                String key = it->key;
                TypeInfo* typeInfo = it->value;

                Expression* typeExpr = arena_alloc(mem, sizeof(Expression));
                typeExpr->type = ExpressionType_TYPE;
                typeExpr->expr.TYPE.typeInfo = typeInfo;

                FunctionArg field = {.id = key, .type = typeExpr};
                ArrayAppend(fields, field);
            }

            result->typeInfo->symbolType = TYPE_STRUCT_DEF;
            result->typeInfo->structInfo.fields = fields;
        } break;
        #endif
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

            STATIC_ASSERT(BIN_OPERATORS_COUNT == 13); // binary operator count has changed
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
            } else if(TypeIsInt(lhsType->typeInfo) && TypeIsInt(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_AND) {
                result->typeInfo = lhsType->typeInfo;
            } else if(TypeIsInt(lhsType->typeInfo) && TypeIsInt(rhsType->typeInfo) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_OR) {
                result->typeInfo = lhsType->typeInfo;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_COMPARISON) {
                result->typeInfo = typeBool;
            } else if(((TypeIsBool(lhsType->typeInfo) && TypeIsBool(rhsType->typeInfo)) || (TypeIsNumber(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo))) && TypeMatch(lhsType->typeInfo, rhsType->typeInfo) && op.type == TokenType_NOT_EQUALS) {
                result->typeInfo = typeBool;
            } else if(op.type == TokenType_AS && TypeIsType(rhsType->typeInfo) && TypeIsNumber(lhsType->typeInfo)) {
                // TODO: accually check if the cast is valid
                TypeInfo* typeInfo = parsedTypeToTypeInfo(rhsType->expr.TYPE.typeInfo, constants, scope);
                if(TypeIsNumber(typeInfo)) {
                    result->typeInfo = typeInfo;
                } else {
                    // NOTE: this is just a hack,
                    //       converting the `ParsedType` to `TypeInfo` and
                    //       checking if its a number in one expression in long and
                    //       i dont want to type it twice
                    goto err;
                }
            } else {
                err:
                ERROR_VA(op.loc, "Invalid binary operation in expression: %s "STR_FMT" %s", TypeStr[lhsType->typeInfo->symbolType], STR_PRINT(op.value), TypeStr[rhsType->typeInfo->symbolType]);
            }

            result->expr.BINARY_EXPRESSION.lhs = lhsType;
            result->expr.BINARY_EXPRESSION.rhs = rhsType;
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
            } else {
                ERROR_VA(op.loc, "Invalid unary operation in expression: "STR_FMT" %s", STR_PRINT(op.value), TypeStr[typeInfo->typeInfo->symbolType]);
            }

            result->expr.UNARY_EXPRESSION.expr = typeInfo;
        } break;
        case ExpressionType_TYPE: {
            // parsedTypeToTypeInfo(expr->expr.TYPE.type, constants, scope);
            result->typeInfo = TypeInitSimple(mem, TYPE_TYPE);
        } break;
        case ExpressionType_STRUCT_LIT: {
            if(expr->expr.STRUCT_LIT.idProvided) {
                // foo := vec2{ ... };
                Token id = expr->expr.STRUCT_LIT.id;
                TypeResult res = findStruct(scope, id.value);
                if(res.err) {
                    ERROR_VA(id.loc, "Undeclared struct: "STR_FMT, STR_PRINT(id.value));
                }
                result->typeInfo = res.typeInfo;

                if(TypeIsStructDef(expected) && !TypeMatch(expected, result->typeInfo)) {
                    ERROR_VA(
                        id.loc,
                        "Cannot assign struct of type: "STR_FMT" to struct of type: "STR_FMT,
                        STR_PRINT(TypeToString(mem, result->typeInfo)),
                        STR_PRINT(TypeToString(mem, expected))
                    );
                }
            } else {
                // foo : vec2 = { ... };
                if(!TypeIsStructDef(expected)) {
                    Location loc = {0};
                    ERROR_VA(loc, "Cannot assign struct to type: "STR_FMT, STR_PRINT(TypeToString(mem, expected)));
                }

                result->typeInfo = expected;
            }

            assertf(TypeIsStructDef(result->typeInfo), "expected structure definition, got: "STR_FMT, STR_PRINT(TypeToString(mem, result->typeInfo)));

            switch(expr->expr.STRUCT_LIT.type) {
                case StructInitializerListType_NONE: UNREACHABLE("StructInitializerListType_NONE invalid here"); break;

                case StructInitializerListType_POSITIONAL: {
                    Array(ExpressionPtr) list = expr->expr.STRUCT_LIT.positionalInitializerList;
                    Array(TypecheckedField) fields = result->typeInfo->structInfo.fields;

                    if(list.size != fields.size) {
                        Location loc = {0};
                        if(expr->expr.STRUCT_LIT.idProvided) loc = expr->expr.STRUCT_LIT.id.loc;
                        ERROR_VA(
                            loc,
                            "Incorrect number of initializers in struct literal, got: %llu, expected: %llu",
                            list.size,
                            fields.size
                        );
                    }

                    Array(TypecheckedExpressionPtr) initializerList = {0};
                    for(u64 i = 0; i < list.size; ++i) {
                        Expression* initializerExpr = list.data[i];
                        TypecheckedField field = fields.data[i];
                        TypeInfo* fieldType = field.type;
                        TypecheckedExpression* initType = typecheckExpression(mem, initializerExpr, scope, constants, fieldType);
                        if(!TypeMatch(fieldType, initType->typeInfo)) {
                            Location loc = {0};
                            if(expr->expr.STRUCT_LIT.idProvided) loc = expr->expr.STRUCT_LIT.id.loc;
                            ERROR_VA(
                                loc,
                                "Type missmatch in struct initializer, in field `."STR_FMT"`, got type: "STR_FMT", expected: "STR_FMT,
                                STR_PRINT(field.id),
                                STR_PRINT(TypeToString(mem, initType->typeInfo)),
                                STR_PRINT(TypeToString(mem, fieldType))
                            );
                        }
                        ArrayAppend(initializerList, initType);
                    }
                    result->expr.STRUCT_LIT.positionalInitializerList = initializerList;
                } break;
                case StructInitializerListType_DESIGNATED: {
                    Array(NamedInitializer) list = expr->expr.STRUCT_LIT.namedInitializerList;
                    Array(TypecheckedField) fields = result->typeInfo->structInfo.fields;

                    if(list.size != fields.size) {
                        Location loc = {0};
                        if(expr->expr.STRUCT_LIT.idProvided) loc = expr->expr.STRUCT_LIT.id.loc;
                        ERROR_VA(
                            loc,
                            "Incorrect number of initializers in struct literal, got: %llu, expected: %llu",
                            list.size,
                            fields.size
                        );
                    }

                    Array(TypecheckedNamedInitializer) initializerList = {0};
                    for(u64 i = 0; i < list.size; ++i) {
                        NamedInitializer initializer = list.data[i];
                        String initId = initializer.id;
                        TypecheckedField field = {0};
                        bool found = FALSE;
                        for(u64 h = 0; h < fields.size; ++h) {
                            TypecheckedField it = fields.data[h];
                            if(StringEquals(it.id, initId)) {
                                found = TRUE;
                                field = it;
                                break;
                            }
                        }
                        if(!found) {
                            Location loc = {0};
                            if(expr->expr.STRUCT_LIT.idProvided) loc = expr->expr.STRUCT_LIT.id.loc;
                            ERROR_VA(
                                loc,
                                "Structure doesnt contain a field named: `"STR_FMT"`",
                                STR_PRINT(initId)
                            );
                        }

                        TypeInfo* fieldType = field.type;
                        TypecheckedExpression* initType = typecheckExpression(mem, initializer.expr, scope, constants, fieldType);
                        if(!TypeMatch(fieldType, initType->typeInfo)) {
                            Location loc = {0};
                            if(expr->expr.STRUCT_LIT.idProvided) loc = expr->expr.STRUCT_LIT.id.loc;
                            ERROR_VA(
                                loc,
                                "Type missmatch in struct initializer, in field `."STR_FMT"`, got type: "STR_FMT", expected: "STR_FMT,
                                STR_PRINT(field.id),
                                STR_PRINT(TypeToString(mem, initType->typeInfo)),
                                STR_PRINT(TypeToString(mem, fieldType))
                            );
                        }

                        TypecheckedNamedInitializer tmp = {
                            .id = initId,
                            .expr = initType,
                        };
                        ArrayAppend(initializerList, tmp);
                    }
                    result->expr.STRUCT_LIT.namedInitializerList = initializerList;
                } break;
            }
        } break;
        case ExpressionType_FIELD_ACCESS: {
            Token variableName = expr->expr.FIELD_ACCESS.variableName;
            Token fieldName = expr->expr.FIELD_ACCESS.fieldName;

            TypeResult res = findVariableType(scope, variableName.value);
            if(res.err) {
                ERROR_VA(variableName.loc, "Undeclared variable: "STR_FMT, STR_PRINT(variableName.value));
            }
            TypeInfo* structType = res.typeInfo;

            s64 foundIndex = -1;
            for(u64 i = 0; i < structType->structInfo.fields.size; ++i) {
                TypecheckedField field = structType->structInfo.fields.data[i];
                if(StringEquals(field.id, fieldName.value)) {
                    foundIndex = i;
                    break;
                }
            }

            if(foundIndex == -1) {
                ERROR_VA(
                    fieldName.loc,
                    "Structure doesnt contain a field named: `"STR_FMT"`",
                    STR_PRINT(fieldName.value)
                );
            }

            TypecheckedField field = structType->structInfo.fields.data[foundIndex];
            result->typeInfo = field.type;
        } break;
        case ExpressionType_ARRAY_ACCESS: {
            Token id = expr->expr.ARRAY_ACCESS.id;
            Expression* index = expr->expr.ARRAY_ACCESS.index;

            TypeResult res = findVariableType(scope, id.value);
            if(res.err) ERROR_VA(id.loc, "Undeclared variable: "STR_FMT, STR_PRINT(id.value));
            TypeInfo* arrayType = res.typeInfo;

            TypeInfo* elementType = 0;
            if(arrayType->symbolType == TYPE_ARRAY) {
                elementType = arrayType->arrayInfo.elementType;
            } else if(arrayType->isPointer) {
                elementType = arrayType;
            } else {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Can only index array types or pointers, got: "STR_FMT, STR_PRINT(TypeToString(mem, arrayType)));
            }

            TypecheckedExpression* typecheckedIndex = typecheckExpression(mem, index, scope, constants, NULL);
            if(!TypeIsInt(typecheckedIndex->typeInfo)) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Can only index arrays using integer types, found: "STR_FMT, STR_PRINT(TypeToString(mem, typecheckedIndex->typeInfo)));
            }
            result->expr.ARRAY_ACCESS.index = typecheckedIndex;

            if(
                (TypeIsInt(expected) && TypeIsInt(elementType)) ||
                (TypeIsFloat(expected) && TypeIsFloat(elementType))
            ) {
                result->typeInfo = expected;
            } else {
                result->typeInfo = elementType;
            }
        } break;
    }

    return result;
}

typedef struct FunctionsInScope {
    Array(String) fnNames;
    Array(ExpressionPtr) fnValues;
    Array(String) structNames;
    Array(ExpressionPtr) structValues;
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
            ArrayAppend(result.fnNames, key);
            ArrayAppend(result.fnValues, value);
            continue;
        }

        if(value->type == ExpressionType_TYPE && value->expr.TYPE.type->type == ParsedTypeType_STRUCT) {
            ArrayAppend(result.structNames, key);
            ArrayAppend(result.structValues, value);
            continue;
        }

        EvaluateConstantResult res = evaluateConstant(value, mem, target);
        if(res.error) {
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
            if(res.error) {
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
    for(u64 i = 0; i < functions.fnNames.size; ++i) {
        String id = functions.fnNames.data[i];
        Expression* expr = functions.fnValues.data[i];
        assert(expr->type == ExpressionType_FUNCTION_LIT, "Expected function lit in typecheckFunctions");

        GenericScope* fnScope = expr->expr.FUNCTION_LIT.scope;
        // NOTE: this return type really doesnt have to happen here and can be moved to after the arguments have been typechecked,
        //       this will probably be needed later once the args can contain types that may be used as return types
        TypeInfo* returnType = parsedTypeToTypeInfo(expr->expr.FUNCTION_LIT.typeInfo->returnType, constants, parent);

        TypeInfo* fnTypeInfo = TypeInitSimple(mem, TYPE_FUNCTION);
        fnTypeInfo->functionInfo = expr->expr.FUNCTION_LIT.typeInfo;

        TypecheckedScope* resultScope = TypecheckedScopeInit(mem, parent);

        for(u64 h = 0; h < fnTypeInfo->functionInfo->args.size; ++h) {
            FunctionArg arg = fnTypeInfo->functionInfo->args.data[h];
            String argId = arg.id;
            ParsedType* argType = arg.type;
            TypeInfo* argTypeInfo = parsedTypeToTypeInfo(argType, constants, resultScope);
            saveParam(resultScope, constants, argId, argTypeInfo);
        }

        typecheckScopeInto(mem, fnScope, resultScope, constants, returnType);

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
void typecheckScopeInto(Arena* mem, GenericScope* scope, TypecheckedScope* result, Hashmap(String, ConstValue)* constants, TypeInfo* expectedReturnType) {
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
                ParsedType* type = statement->statement.VAR_DECL.type;
                TypeInfo* typeInfo = parsedTypeToTypeInfo(type, &constantsInThisScope, result);

                saveVariable(result, &constantsInThisScope, id, typeInfo);
            } break;
            case StatementType_VAR_DECL_ASSIGN: {
                String id = statement->statement.VAR_DECL_ASSIGN.identifier;
                Expression* expr = statement->statement.VAR_DECL_ASSIGN.expr;
                ParsedType* type = statement->statement.VAR_DECL_ASSIGN.type;
                TypeInfo* typeInfo = parsedTypeToTypeInfo(type, &constantsInThisScope, result);

                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, typeInfo);
                if(inferedType->typeInfo->symbolType == TYPE_VOID) {
                    Location loc = {0}; // TODO: fix location
                    ERROR(loc, "Cannot assign void type");
                }

                if(typeInfo->symbolType == TYPE_NONE) {
                    typeInfo = inferedType->typeInfo;
                } else {
                    if(!TypeMatch(typeInfo, inferedType->typeInfo)) {
                        Location loc = {0}; // TODO: fix location
                        ERROR_VA(
                            loc,
                            "Type mismatch, trying to assign to type: "STR_FMT", expression type: "STR_FMT,
                            STR_PRINT(TypeToString(mem, typeInfo)),
                            STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                        );
                    }
                }

                saveVariable(result, &constantsInThisScope, id, typeInfo);

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = inferedType;
                typechecked.node.VAR_ACCESS.isArray = FALSE;
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
                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, expectedType);

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
                typechecked.node.VAR_ACCESS.isArray = FALSE;
                ArrayAppend(result->statements, typechecked);
                // TODO: StatementType_ARRAY_REASSIGN and StatementType_VAR_REASSIGN can probably be collapsed into one case eventually
            } break;
            case StatementType_ARRAY_REASSIGN: {
                String id = statement->statement.ARRAY_REASSIGN.identifier;
                Expression* expr = statement->statement.ARRAY_REASSIGN.expr;
                Expression* index = statement->statement.ARRAY_REASSIGN.index;

                // find variable
                TypeResult expectedRes = findVariableType(result, id);
                if(expectedRes.err) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Trying to assign to an undefined variable: "STR_FMT, STR_PRINT(id));
                }
                TypeInfo* varType = expectedRes.typeInfo;
                TypeInfo* expectedType = 0;
                if(varType->symbolType == TYPE_ARRAY) {
                    expectedType = varType->arrayInfo.elementType;
                } else if(varType->isPointer) {
                    expectedType = varType;
                } else {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Can only index array types or pointers, found: "STR_FMT, STR_PRINT(TypeToString(mem, varType)));
                }

                TypecheckedExpression* inferedType = typecheckExpression(mem, expr, result, &constantsInThisScope, expectedType);
                if(!TypeMatch(expectedType, inferedType->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Type mismatch in assignment, expected: "STR_FMT", but got: "STR_FMT,
                        STR_PRINT(TypeToString(mem, expectedType)),
                        STR_PRINT(TypeToString(mem, inferedType->typeInfo))
                    );
                }

                TypecheckedExpression* typecheckedIndex = typecheckExpression(mem, index, result, &constantsInThisScope, NULL);
                if(!TypeIsInt(typecheckedIndex->typeInfo)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(loc, "Can only index arrays using integer types, found: "STR_FMT, STR_PRINT(TypeToString(mem, typecheckedIndex->typeInfo)));
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = inferedType;
                typechecked.node.VAR_ACCESS.isArray = TRUE;
                typechecked.node.VAR_ACCESS.index = typecheckedIndex;
                ArrayAppend(result->statements, typechecked);
                // TODO: StatementType_ARRAY_REASSIGN and StatementType_VAR_REASSIGN can probably be collapsed into one case eventually
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
    // TODO: functions need to be added into the scope.functions hashmap before their scopes are typechecked
    //       or functions that havent beed typechecked yet will report undefined errors,
    //      potential fix: when constants are resolved the functions have to be added but marked as not typechecked
    typecheckFunctions(mem, &constantsInThisScope, result, functions);

    free(constantsInThisScope.pairs);
}

TypecheckedScope* typecheckScope(Arena* mem, GenericScope* scope, TypecheckedScope* parent, Hashmap(String, ConstValue)* constants, TypeInfo* expectedReturnType) {
    TypecheckedScope* result = TypecheckedScopeInit(mem, parent);
    typecheckScopeInto(mem, scope, result, constants, expectedReturnType);
    return result;
}

TypecheckedScope* typecheckGlobalScope(Arena* mem, GlobalScope* scope, Hashmap(String, ConstValue)* constants) {
    TypecheckedScope* result = TypecheckedScopeInit(mem, NULL);
    
    // initial processing of consts
    Hashmap(String, ConstValue) constantsInThisScope = {0};
    HashmapInit(constantsInThisScope, 0x100); // TODO: better default size

    if(constants) {
        HashmapFor(String, ConstValue, it, constants) {
            String key = it->key;
            ConstValue value = it->value;

            if(!HashmapSet(String, ConstValue)(&constantsInThisScope, key, value)) {
                UNREACHABLE("failed to instert into hashmap");
            }
        }
    }

    FunctionsInScope functions = typecheckProcessConsts(mem, makeScopeFromGlobal(scope), &constantsInThisScope);

    // typecheck structs
    for(u64 i = 0; i < functions.structNames.size; ++i) {
        String id = functions.structNames.data[i];
        Expression* expr = functions.structValues.data[i];
        assert(expr->type == ExpressionType_TYPE && expr->expr.TYPE.type->type == ParsedTypeType_STRUCT, "Expected struct def in typecheckStructs");

        GlobalScope* structScope = expr->expr.TYPE.type->as_struct.scope;
        TypecheckedScope* typechecked = typecheckGlobalScope(mem, structScope, &constantsInThisScope);
        typechecked->parent = result;

        TypeInfo* structTypeInfo = TypeInitSimple(mem, TYPE_STRUCT_DEF);
        Array(TypecheckedField) fields = {0};
        HashmapFor(String, TypeInfoPtr, it, &typechecked->variables) {
            String key = it->key;
            TypeInfo* type = it->value;

            TypecheckedField field = {.id = key, .type = type};
            ArrayAppend(fields, field);
        }
        structTypeInfo->structInfo.fields = fields;

        saveStruct(result, &constantsInThisScope, id, structTypeInfo);
    }

    // typecheck global variables
    HashmapFor(String, StatementPtr, it, &scope->variables) {
        String id = it->key;
        Statement* statement = it->value;
        assert(statement->type == StatementType_VAR_DECL || statement->type == StatementType_VAR_DECL_ASSIGN, "Only declarations are statements");
        if(statement->type == StatementType_VAR_DECL_ASSIGN) UNIMPLEMENTED("Global initializers");

        if(statement->type == StatementType_VAR_DECL) {
            ParsedType* type = statement->statement.VAR_DECL.type;
            TypeInfo* typeInfo = parsedTypeToTypeInfo(type, &constantsInThisScope, result);
            if(!HashmapSet(String, TypeInfoPtr)(&result->variables, id, typeInfo)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, result->variables.capacity, result->variables.size, STR_PRINT(id));
            }
        } else {
            UNREACHABLE("incorrect statement type");
        }
    }

    // typecheck functions
    // TODO: functions need to be added into the scope.functions hashmap before their scopes are typechecked
    //       or functions that havent beed typechecked yet will report undefined errors,
    //       potential fix: when constants are resolved the functions have to be added but marked as not typechecked
    typecheckFunctions(mem, &constantsInThisScope, result, functions);

    return result;
}

TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult) {
    GlobalScope* globalScope = parseResult->globalScope;
    return typecheckGlobalScope(mem, globalScope, NULL);
}

// TODO: function variables not done fully, their bodies never get typechecked and as a result they never get generated
