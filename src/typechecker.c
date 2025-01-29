#include "typechecker.h"
#include "operatorFunctions.c"

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
    if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_ADD) {
        return _add(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_SUB) {
        return _sub(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_MUL) {
        return _mul(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_DIV) {
        return _div(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_LESS) {
        return _less(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_GREATER) {
        return _greater(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_LESS_EQ) {
        return _less_eq(lhs, rhs);
    } else if(TypeIsNumber(lhs) && TypeIsNumber(rhs) && operator.type == TokenType_GREATER_EQ) {
        return _greater_eq(lhs, rhs);
    } else if((TypeIsNumber(lhs) || TypeIsBool(lhs)) && (TypeIsNumber(rhs) || TypeIsBool(rhs)) && operator.type == TokenType_COMPARISON) {
        return _equals(lhs, rhs);
    } else if((TypeIsNumber(lhs) || TypeIsBool(lhs)) && (TypeIsNumber(rhs) || TypeIsBool(rhs)) && operator.type == TokenType_NOT_EQUALS) {
        return _not_equals(lhs, rhs);
    } else {
        ERROR_VA(operator.loc, "Invalid binary operation: %s "STR_FMT" %s", TypeStr[lhs.typeInfo->symbolType], STR_PRINT(operator.value), TypeStr[rhs.typeInfo->symbolType]);
    }

    // NOTE: add custom operator definitions here

    return (ConstValue){0}; // NOTE: to silence warning
}

ConstValue evaluateUnaryExpression(Token operator, ConstValue value) {
    if(operator.type == TokenType_SUB) {
        if(!isNumberType(value.typeInfo)) {
            Location loc = {.filename = STR(""), .collum = 1, .line = 1}; // TODO: fix loc
            ERROR(loc, "Can only perform the - unary operator on a number");
        }
        switch(value.typeInfo->symbolType) {
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
        }
    } else {
        UNREACHABLE("invalid unary operator");
    }

    return value;
}

EvaluateConstantResult evaluateConstant(Expression* expr, Hashmap(String, ConstValue)* evaluatedConstatants) {
    EvaluateConstantResult result1 = {0};
    ConstValue result = {0};

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            result.typeInfo->symbolType = TYPE_S64;
            result.as_s64 = StringToS64(expr->expr.INT_LIT.value);
        } break;
        case ExpressionType_FLOAT_LIT: {
            result.typeInfo->symbolType = TYPE_F64;
            u64 fractPartLen = expr->expr.FLOAT_LIT.fractPart.length;
            s64 wholePart = StringToS64(expr->expr.FLOAT_LIT.wholePart);
            s64 fractPart = StringToS64(expr->expr.FLOAT_LIT.fractPart);

            result.as_f64 = ((f64)wholePart) + ((f64)fractPart / (10 * fractPartLen));
        } break;
        case ExpressionType_STRING_LIT: {
            result.typeInfo->symbolType = TYPE_STRING;
            result.as_String = expr->expr.STRING_LIT.value;
        } break;
        case ExpressionType_BOOL_LIT: {
            result.typeInfo->symbolType = TYPE_BOOL;
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
            // NOTE: this case is technically not an error we just want to propagate to the caller that this was a function litral
            result1.error = ConstantEvaluationError_FUNCTION_LITERAL;

            // TODO: remove
            // Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            // TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
            // bool isExtern = expr->expr.FUNCTION_LIT.isExtern;
            // Scope* scope = expr->expr.FUNCTION_LIT.scope;

            // result.typeInfo->symbolType = TYPE_FUNCTION;
            // result.typeInfo->functionInfo.isExternal = isExtern;
            // result.as_function = scope;
            // for(u64 i = 0; i < args.size; ++i) {
            //     FunctionArg arg = args.data[i];
            //     ArrayAppend(result.typeInfo->functionInfo.argTypes, arg.type);
            // }
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            EvaluateConstantResult lhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.lhs, evaluatedConstatants);
            ConstValue lhs = lhsRes.val;
            if(lhsRes.error) {
                result1.error = TRUE;
                break;
            }

            EvaluateConstantResult rhsRes = evaluateConstant(expr->expr.BINARY_EXPRESSION.rhs, evaluatedConstatants);
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
            EvaluateConstantResult valRes = evaluateConstant(expr->expr.UNARY_EXPRESSION.expr, evaluatedConstatants);
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

// symbol means both constant and variable
TypeResult findSymbolType(TypecheckedScope* scope, String identifier) {
    TypecheckedScope* it = scope;
    while(it) {
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
        TypeInfo* info = {0};
        if(HashmapGet(String, TypeInfoPtr)(&it->variables, identifier, &info)) {
            return (TypeResult){.typeInfo = info};
        }
        it = it->parent;
    }

    return (TypeResult){.err = TRUE};
}

static TypeInfo baseTypes[BaseTypesIndex_COUNT] = {
    [BaseTypesIndex_S64]    = {.symbolType = TYPE_S64},
    [BaseTypesIndex_F64]    = {.symbolType = TYPE_F64},
    [BaseTypesIndex_STRING] = {.symbolType = TYPE_STRING},
    [BaseTypesIndex_BOOL]   = {.symbolType = TYPE_BOOL},
};

TypeInfo* inferExpressionType(Expression* expr, TypecheckedScope* scope) {
    TypeInfo* result = {0};

    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            // NOTE: scuffed way of returning a stable pointer without allocation
            result->symbolType = &baseTypes[BaseTypesIndex_S64];
        } break;
        case ExpressionType_FLOAT_LIT: {
            // NOTE: scuffed way of returning a stable pointer without allocation
            result->symbolType = &baseTypes[BaseTypesIndex_F64];
        } break;
        case ExpressionType_STRING_LIT: {
            // NOTE: scuffed way of returning a stable pointer without allocation
            result->symbolType = &baseTypes[BaseTypesIndex_STRING];
        } break;
        case ExpressionType_BOOL_LIT: {
            // NOTE: scuffed way of returning a stable pointer without allocation
            result->symbolType = &baseTypes[BaseTypesIndex_BOOL];
        } break;
        case ExpressionType_SYMBOL: {
            // look up in the constants or symbols and find the type
            String id = expr->expr.SYMBOL.identifier;

            TypeResult res = findSymbolType(scope, id);
            if(res.err) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Undefined symbol: "STR_FMT, STR_PRINT(id));
            }

            result = res.typeInfo;
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

            TypeInfo funcInfo = *res.typeInfo;
            if(funcInfo.symbolType != TYPE_FUNCTION) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Trying to use non function symbol \""STR_FMT"\" as function", STR_PRINT(id));
            }

            if(funcInfo.functionInfo.argTypes.size != args.size) {
                Location loc = {0}; // TODO: fix loc
                ERROR_VA(loc, "Incorrect number of arguments for funnction call, provided: %llu, expected: %llu", args.size, funcInfo.functionInfo.argTypes.size);
            }

            for(u64 i = 0; i < args.size; ++i) {
                Expression* providedArg = args.data[i];
                TypeInfo expectedArgType = *funcInfo.functionInfo.argTypes.data[i];
                TypeInfo providedArgType = *inferExpressionType(providedArg, scope);

                if(!typesMatch(expectedArgType, providedArgType)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "provided argument number %llu has incompatible type \""STR_FMT"\", with expected type \""STR_FMT"\"",
                        i + 1,
                        STR_PRINT(typeToString(providedArgType)),
                        STR_PRINT(typeToString(expectedArgType))
                    );
                }
            }

            result = funcInfo.functionInfo.returnType;
        } break;
        case ExpressionType_FUNCTION_LIT: {
            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
            bool isExtern = expr->expr.FUNCTION_LIT.isExtern;
            Scope* functionScope = expr->expr.FUNCTION_LIT.scope;

            Array(TypeInfoPtr) argTypes = {0};
            for(u64 h = 0; h < args.size; ++h) {
                FunctionArg arg = args.data[h];
                ArrayAppend(argTypes, arg.type);
            }

            result->symbolType = TYPE_FUNCTION;
            result->functionInfo.argTypes = argTypes;
            result->functionInfo.isExternal = isExtern;
            result->functionInfo.returnType = returnType;
        } break;
        case ExpressionType_BINARY_EXPRESSION: {
            // 1. verify the type binary operation can be performed on the types
            // 2. get the type after the operation is performed
            UNIMPLEMENTED("");

            Token op = expr->expr.BINARY_EXPRESSION.operator;
            Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
            Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;
            TypeInfo* lhsType = inferExpressionType(lhs, scope);
            TypeInfo* rhsType = inferExpressionType(rhs, scope);
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            // 1. verify the type unary operation can be performed on the type of the value
            // 2. get the type after the operation is performed
            UNIMPLEMENTED("");

            Token op = expr->expr.UNARY_EXPRESSION.operator;
            Expression* opExpr = expr->expr.UNARY_EXPRESSION.expr;
        } break;
    }

    return result;
}

void saveVariable(TypecheckedScope* scope, String id, TypeInfo* type) {
    // check for redefinition
    TypeInfo* tmp = {0};
    if(HashmapGet(String, TypeInfoPtr)(&scope->variables, id, &tmp)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of variable: "STR_FMT, STR_PRINT(id));
    }

    // save the symbol
    if(!HashmapSet(String, TypeInfoPtr)(&scope->variables, id, type)) {
        UNREACHABLE("Failed to insert into hashmap");
    }
}

void saveConstant(TypecheckedScope* scope, String id, ConstValue val) {
    // check for redefinition
    ConstValue tmp = {0};
    if(HashmapGet(String, ConstValue)(&scope->constants, id, &tmp)) {
        Location loc = {0}; // TODO: fix
        ERROR_VA(loc, "Redefinition of constant: "STR_FMT, STR_PRINT(id));
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

// expectedReturnType is used for typechecking return statement
TypecheckedScope* typecheckScope(Arena* mem, Scope* scope, TypecheckedScope* parent, TypeInfo* expectedReturnType, bool isTopLevel) {
#define OTHER_BUCKET (currentIt == 1 ? 0 : 1)
#define CURRENT_BUCKET (currentIt)
    TypecheckedScope* result = TypecheckedScopeInit(mem, parent);
    
    // evaluate constants
    Array(ASTNodePtr) buckets[2] = {0};
    for(u64 i = 0; i < scope->statements.size; ++i) {
        ASTNode* statement = scope->statements.data[i];
        if(isTopLevel) {
            if(statement->type != ASTNodeType_VAR_CONST || statement->type != ASTNodeType_VAR_DECL || statement->type != ASTNodeType_VAR_DECL_ASSIGN) {
                Location loc = {0}; // TODO: fix
                ERROR_VA(loc, "Invalid toplevel statments: %s", ASTNodeTypeStr[statement->type]);
            }
        }
        ArrayAppend(buckets[0], statement);
    }

    // NOTE: these are the functions that need to be typechecked onece the current scope is done
    Array(ExpressionPtr) fnScopes = {0};
    Array(String) fnNames = {0};

    u64 currentIt = 0;
    u64 processedCount = 0;
    while(TRUE) {
        for(u64 i = 0; i < buckets[CURRENT_BUCKET].size; ++i) {
            ASTNode* statement = buckets[CURRENT_BUCKET].data[i];

            if(statement->type != ASTNodeType_VAR_CONST) continue;

            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            EvaluateConstantResult res = evaluateConstant(expr, &result->constants);

            if(res.error == ConstantEvaluationError_UNDEFINED_SYMBOL) {
                // add to other bucket
                ArrayAppend(buckets[OTHER_BUCKET], statement);
            } else if(res.error == ConstantEvaluationError_FUNCTION_LITERAL) {
                // record the function scope for later typechecking
                assert(expr->type == ExpressionType_FUNCTION_LIT, "only function literal here");
                ArrayAppend(fnScopes, expr);
                ArrayAppend(fnNames, id);
            } else {
                saveConstant(&result, id, res.val);
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
            case ASTNodeType_VAR_DECL: {
                String id = statement->node.VAR_DECL.identifier;
                TypeInfo* type = statement->node.VAR_DECL.type;

                saveVariable(&result, id, type);
            } break;
            case ASTNodeType_VAR_DECL_ASSIGN: {
                String id = statement->node.VAR_DECL_ASSIGN.identifier;
                Expression* expr = statement->node.VAR_DECL_ASSIGN.expr;
                TypeInfo* type = statement->node.VAR_DECL_ASSIGN.type;

                if(type->symbolType == TYPE_VOID) {
                    type = inferExpressionType(expr, &result);
                }

                saveVariable(&result, id, type);

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = expr;
                typechecked.node.VAR_ACCESS.type = type;
                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_VAR_REASSIGN: {
                String id = statement->node.VAR_REASSIGN.identifier;
                Expression* expr = statement->node.VAR_REASSIGN.expr;

                // find variable
                TypeResult expectedRes = findVariableType(&result, id);
                if(expectedRes.err) {
                    Location loc = {0}; // TODO:
                    ERROR_VA(loc, "Trying to assign to an undefined variable: "STR_FMT, STR_PRINT(id));
                }
                TypeInfo* expectedType = expectedRes.typeInfo;
                TypeInfo* inferedType  = inferExpressionType(expr, &result);

                if(!typesMatch(expectedType, inferedType)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Type mismatch in assignment, expected: "STR_FMT", but got: "STR_FMT,
                        STR_PRINT(typeToString(*expectedType)),
                        STR_PRINT(typeToString(*inferedType))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.VAR_ACCESS.identifier = id;
                typechecked.node.VAR_ACCESS.expr = expr;
                typechecked.node.VAR_ACCESS.type = inferedType;
                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_RET: {
                Expression* expr = statement->node.RET.expr;
                TypeInfo* inferedType = inferExpressionType(expr, &result);

                if(!typesMatch(expectedReturnType, inferedType)) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Type mismatch in return, function expected expected: "STR_FMT", but got: "STR_FMT,
                        STR_PRINT(typeToString(*expectedReturnType)),
                        STR_PRINT(typeToString(*inferedType))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.RET.expr = expr;
                typechecked.node.RET.type = inferedType;
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

                    TypeInfo* inferedType = inferExpressionType(blockExpr, &result);
                    TypeInfo* boolType = &baseTypes[BaseTypesIndex_BOOL];
                    if(!typesMatch(boolType, inferedType)) {
                        Location loc = {0}; // TODO: fix loc
                        ERROR_VA(
                            loc,
                            "Expression has incorrect type, if conditions need to have bool type, but got: "STR_FMT,
                            STR_PRINT(typeToString(*inferedType))
                        );
                    }

                    TypechekedConditionalBlock typecheckedBlock = {0};
                    typecheckedBlock.scope = typecheckScope(mem, blockScope, &result, expectedReturnType, FALSE);
                    typecheckedBlock.expr = blockExpr;
                    typecheckedBlock.type = inferedType;
                    ArrayAppend(typechecked.node.IF.blocks, typecheckedBlock);
                }

                if(hasElse) {
                    typechecked.node.IF.elze = typecheckScope(mem, elze, &result, expectedReturnType, FALSE);
                }

                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_LOOP: {
                Expression* expr = statement->node.LOOP.expr;
                Scope* scope = statement->node.LOOP.scope;

                TypeInfo* inferedType = inferExpressionType(expr, &result);
                TypeInfo* boolType = &baseTypes[BaseTypesIndex_BOOL];
                if(!(typesMatch(boolType, inferedType) || isNumberType(inferedType))) {
                    Location loc = {0}; // TODO: fix loc
                    ERROR_VA(
                        loc,
                        "Expression has incorrect type, loop expressions need to have bool or numeric type, but got: "STR_FMT,
                        STR_PRINT(typeToString(*inferedType))
                    );
                }

                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.LOOP.expr = expr;
                typechecked.node.LOOP.type = inferedType;
                typechecked.node.LOOP.scope = typecheckScope(mem, scope, &result, expectedReturnType, FALSE);
                ArrayAppend(result->statements, typechecked);
            } break;
            case ASTNodeType_EXPRESSION: {
                Expression* expr = statement->node.EXPRESSION.expr;
                TypeInfo* typeInfo = inferExpressionType(expr, &result);
                TypecheckedStatement typechecked = {0};
                typechecked.type = statement->type;
                typechecked.node.EXPRESSION.expr = expr;
                typechecked.node.EXPRESSION.type = typeInfo;
                ArrayAppend(result->statements, typechecked);
            } break;
        }
    }

    // typecheck all the function bodies
    assert(fnScopes.size == fnNames.size, "names size does not match scopes size");
    for(u64 i = 0; i < fnScopes.size; ++i) {
        Expression* expr = fnScopes.data[i];
        String id = fnNames.data[i];

        Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
        TypeInfo* returnType = expr->expr.FUNCTION_LIT.returnType;
        bool isExtern = expr->expr.FUNCTION_LIT.isExtern;
        Scope* fnScope = expr->expr.FUNCTION_LIT.scope;

        ConstValue fnValue = {0};
        fnValue.typeInfo->symbolType = TYPE_FUNCTION;
        fnValue.typeInfo->functionInfo.isExternal = isExtern;
        fnValue.as_function = scope;
        for(u64 i = 0; i < args.size; ++i) {
            FunctionArg arg = args.data[i];
            ArrayAppend(fnValue.typeInfo->functionInfo.argTypes, arg.type);
        }

        fnValue.as_function = typecheckScope(mem, fnScope, &result, returnType, FALSE);
        saveConstant(&result, id, fnValue);

        // TODO: incorrect in case of a hash collision
        u64 index = HashmapHashFunc(String)(id) % result->constants.capacity;
        ArrayAppend(result->functionIndicies, index);
    }

#undef OTHER_BUCKET
#undef CURRENT_BUCKET
    return result;
}

#if 0
TypecheckedScope* typecheckTopLevelScope(Arena* mem, Scope* scope) {
#define OTHER_BUCKET (currentIt == 1 ? 0 : 1)
#define CURRENT_BUCKET (currentIt)
    TypecheckedScope* result = TypecheckedScopeInit(mem, NULL);

    // evaluate constants
    Array(ASTNodePtr) buckets[2] = {0};
    for(u64 i = 0; i < scope->statements.size; ++i) {
        ASTNode* statement = scope->statements.data[i];
        if(statement->type != ASTNodeType_VAR_CONST || statement->type != ASTNodeType_VAR_DECL || statement->type != ASTNodeType_VAR_DECL_ASSIGN) {
            Location loc = {0}; // TODO: fix
            ERROR_VA(loc, "Invalid toplevel statments: %s", ASTNodeTypeStr[statement->type]);
        }
        ArrayAppend(buckets[0], statement);
    }

    u64 currentIt = 0;
    u64 processedCount = 0;
    while(TRUE) {
        for(u64 i = 0; i < buckets[CURRENT_BUCKET].size; ++i) {
            ASTNode* statement = buckets[CURRENT_BUCKET].data[i];

            if(statement->type != ASTNodeType_VAR_CONST) continue;

            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            EvaluateConstantResult res = evaluateConstant(expr, &result->constants);

            if(res.error == ConstantEvaluationError_UNDEFINED_SYMBOL) {
                // add to other bucket
                ArrayAppend(buckets[OTHER_BUCKET], statement);
            } else {
                // check for redefinition
                ConstValue tmp = {0};
                if(HashmapGet(String, ConstValue)(&result->constants, id, &tmp)) {
                    Location loc = {0}; // TODO: fix
                    ERROR_VA(loc, "Redefinition of constant: "STR_FMT, STR_PRINT(id));
                }

                // save the symbol
                if(!HashmapSet(String, ConstValue)(&result->constants, id, res.val)) {
                    UNREACHABLE("Failed to insert into hashmap");
                }

                // record the index
                if(res.error == ConstantEvaluationError_FUNCTION_LITERAL) {
                    u64 index = HashmapHashFunc(String)(id) % result->constants.capacity;
                    ArrayAppend(result->functionIndicies, index);
                }
                processedCount++;
            }
        }

        if(buckets[OTHER_BUCKET].size == 0) break;
        if(processedCount == 0) {
            Location loc = {0}; // TODO: fix
            ERROR(loc, "Circular dependencies in constants");
        }

        // empty current bucket
        buckets[currentIt].size = 0;

        // swap the two buckets
        if(currentIt == 0) currentIt = 1;
        else if(currentIt == 1) currentIt = 0;
        processedCount = 0;
    }

    // infer the types of the global variables
    for(u64 i = 0; i < scope->statements.size; ++i) {
        ASTNode* statement = scope->statements.data[i];
        if(statement->type == ASTNodeType_VAR_DECL) {
            String id = statement->node.VAR_DECL.identifier;
            TypeInfo* type = statement->node.VAR_DECL.type;

            saveVariable(&result, id, type);
        } else if(statement->type == ASTNodeType_VAR_DECL_ASSIGN) {
            String id = statement->node.VAR_DECL_ASSIGN.identifier;
            Expression* expr = statement->node.VAR_DECL_ASSIGN.expr;
            TypeInfo* type = statement->node.VAR_DECL_ASSIGN.type;

            if(type->symbolType == TYPE_VOID) {
                type = inferExpressionType(expr, &result);
            }

            saveVariable(&result, id, type);
        } else {
            UNREACHABLE("");
        }
    }

    // typecheck all the function bodies
    for(u64 i = 0; i < result->functionIndicies.size; ++i) {
        u64 index = result->functionIndicies.data[i];
        String key = result->constants.pairs[index].key;
        ConstValue value = result->constants.pairs[index].value;
        assert(value.typeInfo->symbolType == TYPE_FUNCTION, "only function literals should remain here");
        TypeInfo* returnType = value.typeInfo->functionInfo.returnType;
        TypecheckedScope* funcScope = typecheckScope(mem, value.as_function, &result, returnType);
        // TODO: what do with funcScope???
    }

#undef OTHER_BUCKET
#undef CURRENT_BUCKET
    return result;
}
#endif

TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult) {
    Scope* globalScope = parseResult->globalScope;
    return typecheckScope(mem, globalScope, NULL, NULL, FALSE);
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
