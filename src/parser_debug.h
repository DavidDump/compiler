#include "parser.h"
#include "common.h"

#include <stdio.h> // printf

void StatementPrint(Statement* node, u64 indent);

#define genPrintHelper(...) do{for(u64 h = 0; h < indent; ++h) printf("    "); printf(__VA_ARGS__);}while(0)

void TypePrint(TypeInfo* type, u64 indent) {
    UNUSED(indent);
    // UNIMPLEMENTED("TypePrint");
    
    printf("%s", TypeStr[type->symbolType]);
}

void ExpressionPrint(Expression* expr, u64 indent) {
    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            String val = expr->expr.INT_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_FLOAT_LIT: {
            String wholePart = expr->expr.FLOAT_LIT.wholePart;
            String fractPart = expr->expr.FLOAT_LIT.fractPart;
            printf(" "STR_FMT"."STR_FMT, STR_PRINT(wholePart), STR_PRINT(fractPart));
        } break;
        case ExpressionType_STRING_LIT: {
            String val = expr->expr.STRING_LIT.value;
            if(val.length > 50) printf(" "STR_FMT"...", 50, val.str);
            else                printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_BOOL_LIT: {
            String val = expr->expr.STRING_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_SYMBOL: { // NOTE: identifier
            String id = expr->expr.SYMBOL.identifier;

            printf(" "STR_FMT, STR_PRINT(id));
        } break;        
        case ExpressionType_BINARY_EXPRESSION: {
            String op = expr->expr.BINARY_EXPRESSION.operator.value;
            ExpressionPrint(expr->expr.BINARY_EXPRESSION.lhs, indent + 1);
            printf(" "STR_FMT, STR_PRINT(op));
            ExpressionPrint(expr->expr.BINARY_EXPRESSION.rhs, indent + 1);
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            String op = expr->expr.UNARY_EXPRESSION.operator.value;
            printf(" "STR_FMT, STR_PRINT(op));
            ExpressionPrint(expr->expr.UNARY_EXPRESSION.expr, indent + 1);
        } break;
        case ExpressionType_FUNCTION_LIT: {
            // String id = expr->expr.FUNCTION_LIT.identifier;
            TypeInfo* retType = expr->expr.FUNCTION_LIT.returnType;
            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            GenericScope* scope = expr->expr.FUNCTION_LIT.scope;

            genPrintHelper("FUNCTION_LIT: {\n");
            // genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(retType, indent);
            printf(",\n");

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    String argId = args.data[i].id;
                    TypeInfo* argType = args.data[i].type;

                    genPrintHelper("        {id: "STR_FMT", type: ", STR_PRINT(argId));
                    TypePrint(argType, indent);
                    printf("},\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            
            // statements
            if(scope->statements.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->statements.size; ++i) {
                    StatementPrint(scope->statements.data[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ExpressionType_FUNCTION_CALL: {
            String id = expr->expr.FUNCTION_CALL.identifier;
            Array(ExpressionPtr) args = expr->expr.FUNCTION_CALL.args;

            genPrintHelper("FUNCTION_CALL: {\n");
            genPrintHelper("    id: "STR_FMT"\n", STR_PRINT(id));

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    for(u64 h = 0; h < indent + 2; ++h) printf("    ");
                    ExpressionPrint(args.data[i], indent + 1);
                    printf(",\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            genPrintHelper("}\n");
        } break;
    }
}

void VariablePrint(String id, Expression* expr, TypeInfo* type) {
    u64 indent = 0;
    genPrintHelper("VAR_DECL_ASSIGN: {\n");
    genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
    genPrintHelper("    type: ");
    TypePrint(type, 0);
    printf(",\n");
    genPrintHelper("    expr:");
    ExpressionPrint(expr, 0 + 1);
    printf(",\n");
    genPrintHelper("}\n");
}

void StatementPrint(Statement* node, u64 indent) {
    switch(node->type) {
        case StatementType_COUNT: break;
        case StatementType_NONE: break;

        case StatementType_VAR_DECL: {
            String id = node->statement.VAR_DECL.identifier;
            TypeInfo* type = node->statement.VAR_DECL.type;

            genPrintHelper("VAR_DECL: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type, indent);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_VAR_DECL_ASSIGN: {
            String id = node->statement.VAR_DECL_ASSIGN.identifier;
            TypeInfo* type = node->statement.VAR_DECL_ASSIGN.type;
            Expression* expr = node->statement.VAR_DECL_ASSIGN.expr;

            genPrintHelper("VAR_DECL_ASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type, indent);
            printf(",\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_VAR_REASSIGN: {
            String id = node->statement.VAR_REASSIGN.identifier;
            Expression* expr = node->statement.VAR_REASSIGN.expr;

            genPrintHelper("VAR_REASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_VAR_CONST: {
            String id = node->statement.VAR_CONST.identifier;
            Expression* expr = node->statement.VAR_CONST.expr;

            genPrintHelper("VAR_CONST: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_RET: {
            Expression* expr = node->statement.RET.expr;

            genPrintHelper("RET: {\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_IF: {
            Array(ConditionalBlock) blocks = node->statement.IF.blocks;
            bool hasElse = node->statement.IF.hasElse;
            GenericScope* elze = node->statement.IF.elze;

            // if block, always the 0 index
            {
                ConditionalBlock block = blocks.data[0];
                Expression* expr = block.expr;
                GenericScope* scope = block.scope;

                genPrintHelper("IF: {\n");
                genPrintHelper("    expr:");
                ExpressionPrint(expr, indent + 1);
                printf(",\n");

                // statements
                if(scope->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 i = 0; i < scope->statements.size; ++i) {
                        StatementPrint(scope->statements.data[i], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }

            // else if blocks
            for(u64 i = 1; i < blocks.size; ++i) {
                ConditionalBlock block = blocks.data[i];
                Expression* expr = block.expr;
                GenericScope* scope = block.scope;

                genPrintHelper("ELSE IF: {\n");
                genPrintHelper("    expr:");
                ExpressionPrint(expr, indent + 1);
                printf(",\n");

                // statements
                if(scope->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 h = 0; h < scope->statements.size; ++h) {
                        StatementPrint(scope->statements.data[h], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }

            if(hasElse) {
                // statements
                if(elze->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 i = 0; i < elze->statements.size; ++i) {
                        StatementPrint(elze->statements.data[i], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }
        } break;
        case StatementType_LOOP: {
            Expression* expr = node->statement.LOOP.expr;
            GenericScope* scope = node->statement.LOOP.scope;

            genPrintHelper("LOOP: {\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");

            // statements
            if(scope->statements.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->statements.size; ++i) {
                    StatementPrint(scope->statements.data[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case StatementType_EXPRESSION: {
            Expression* expr = node->statement.EXPRESSION.expr;
            ExpressionPrint(expr, indent);
        } break;
        case StatementType_DIRECTIVE: break;
    }
}
#undef genPrintHelper

void ASTPrint(Scope root) {
    switch(root.type) {
        case ScopeType_NONE: {
            UNREACHABLE("ScopeType_NONE is invalid: cannot be printed");
        } break;
        case ScopeType_GLOBAL: {
            GlobalScope* scope = root.scope.as_global;

            printf("Scope variables:\n");
            HashmapFor(String, TypeAndExpr, it, &scope->variables) {
                String id = it->key;
                TypeAndExpr value = it->value;
                TypeInfo* type = value.type;
                Expression* expr = value.expr;

                VariablePrint(id, expr, type);
            }

            printf("Scope constants:\n");
            HashmapFor(String, ExpressionPtr, it, &scope->constants) {
                String id = it->key;
                Expression* expr = it->value;
                printf("VAR_CONST: "STR_FMT, STR_PRINT(id));
                ExpressionPrint(expr, 0);
            }
        } break;
        case ScopeType_GENERIC: {
            GenericScope* scope = root.scope.as_generic;

            printf("Scope constants:\n");
            HashmapFor(String, ExpressionPtr, it, &scope->constants) {
                String id = it->key;
                Expression* expr = it->value;
                printf("VAR_CONST: "STR_FMT, STR_PRINT(id));
                ExpressionPrint(expr, 0);
            }

            printf("Scope statements:\n");
            for(u64 i = 0; i < scope->statements.size; ++i) {
                Statement* statement = scope->statements.data[i];
                StatementPrint(statement, 0);
            }
        } break;
    }
}
