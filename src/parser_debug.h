#ifndef COMP_PARSER_DEBUG_H
#define COMP_PARSER_DEBUG_H

#include "parser.h"
#include "common.h"

#include <stdio.h> // printf

void GlobalScopePrint(GlobalScope* scope, u64 indent);
void GenericScopePrint(GenericScope* scope, u64 indent);
void StatementPrint(Statement* node, u64 indent);

#define genPrintHelper(...) do{for(u64 h = 0; h < indent; ++h) printf("    "); printf(__VA_ARGS__);}while(0)

static Arena debugArena = {0};

void TypePrint(TypeInfo* type) {
    if(type == NULL || type->symbolType == TYPE_NONE) {
        printf(" INFERRED");
    } else {
        String typeStr = TypeToString(&debugArena, type);
        printf(" "STR_FMT, STR_PRINT(typeStr));
    }
}

TypeInfo* DebugParsedTypeToType(ParsedType* type) {
    assert(type->type == ParsedTypeType_SIMPLE, "Can only convert ParsedTypeType_SIMPLE to TypeInfo");
    return type->as_simple.typeInfo;
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
            TypeInfo* retType = DebugParsedTypeToType(expr->expr.FUNCTION_LIT.typeInfo->returnType);
            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.typeInfo->args;
            GenericScope* scope = expr->expr.FUNCTION_LIT.scope;

            printf("FUNCTION_LIT: {\n");
            genPrintHelper("    type: ");
            TypePrint(retType);
            printf(",\n");

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    String argId = args.data[i].id;
                    TypeInfo* argType = DebugParsedTypeToType(args.data[i].type);

                    genPrintHelper("        {id: "STR_FMT", type: ", STR_PRINT(argId));
                    TypePrint(argType);
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
            genPrintHelper("}");
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
            genPrintHelper("}");
        } break;
        case ExpressionType_TYPE: {
            ParsedType* type = expr->expr.TYPE.type;
            TypeInfo* typeInfo = DebugParsedTypeToType(type);
            TypePrint(typeInfo);
        } break;
        case ExpressionType_STRUCT_LIT: {
            if(expr->expr.STRUCT_LIT.idProvided) {
                String id = expr->expr.STRUCT_LIT.id.value;
                printf(STR_FMT, STR_PRINT(id));
            }
            printf("{");

            StructInitializerListType type = expr->expr.STRUCT_LIT.type;
            switch(type) {
                case StructInitializerListType_NONE: UNREACHABLE("StructInitializerListType_NONE is invalid here"); break;

                case StructInitializerListType_POSITIONAL: {
                    Array(ExpressionPtr) list = expr->expr.STRUCT_LIT.positionalInitializerList;
                    for(u64 i = 0; i < list.size; ++i) {
                        Expression* elem = list.data[i];
                        ExpressionPrint(elem, indent);
                        if(i < list.size - 1) printf(", ");
                    }
                } break;
                case StructInitializerListType_DESIGNATED: {
                    Array(NamedInitializer) list = expr->expr.STRUCT_LIT.namedInitializerList;
                    for(u64 i = 0; i < list.size; ++i) {
                        NamedInitializer elem = list.data[i];
                        printf("."STR_FMT" = ", STR_PRINT(elem.id));
                        ExpressionPrint(elem.expr, indent);
                        if(i < list.size - 1) printf(", ");
                    }
                } break;
            }

            printf("}");
        } break;
        case ExpressionType_FIELD_ACCESS: {
            String variableName = expr->expr.FIELD_ACCESS.variableName.value;
            String fieldName = expr->expr.FIELD_ACCESS.fieldName.value;
            printf(STR_FMT"."STR_FMT, STR_PRINT(variableName), STR_PRINT(fieldName));
        } break;
        case ExpressionType_ARRAY_ACCESS: {
            String id = expr->expr.ARRAY_ACCESS.id.value;
            Expression* index = expr->expr.ARRAY_ACCESS.index;
            printf(STR_FMT"[", STR_PRINT(id));
            ExpressionPrint(index, indent);
            printf("]");
        } break;
    }
}

void VariablePrint(String id, Expression* expr, TypeInfo* type) {
    u64 indent = 0;
    genPrintHelper("VAR_DECL_ASSIGN: (this is accually a lie it may not be decl assign) {\n");
    genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
    genPrintHelper("    type: ");
    TypePrint(type);
    printf(",\n");
    if(expr) {
        genPrintHelper("    expr:");
        ExpressionPrint(expr, 0 + 1);
        printf(",\n");
    }
    genPrintHelper("}\n");
}

void StatementPrint(Statement* node, u64 indent) {
    switch(node->type) {
        case StatementType_COUNT: break;
        case StatementType_NONE: break;

        case StatementType_VAR_DECL: {
            String id = node->statement.VAR_DECL.identifier;
            TypeInfo* type = DebugParsedTypeToType(node->statement.VAR_DECL.type);

            genPrintHelper("VAR_DECL: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case StatementType_VAR_DECL_ASSIGN: {
            String id = node->statement.VAR_DECL_ASSIGN.identifier;
            TypeInfo* type = DebugParsedTypeToType(node->statement.VAR_DECL_ASSIGN.type);
            Expression* expr = node->statement.VAR_DECL_ASSIGN.expr;

            genPrintHelper("VAR_DECL_ASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type);
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
        case StatementType_ARRAY_REASSIGN: {
            String id = node->statement.ARRAY_REASSIGN.identifier;
            Expression* expr = node->statement.ARRAY_REASSIGN.expr;
            Expression* index = node->statement.ARRAY_REASSIGN.index;

            genPrintHelper("ARRAY_REASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    index:");
            ExpressionPrint(index, indent + 1);
            printf(",\n");
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
                genPrintHelper("}");
            }

            if(blocks.size == 0) printf("\n");
            else printf(" ");

            // else if blocks
            for(u64 i = 1; i < blocks.size; ++i) {
                ConditionalBlock block = blocks.data[i];
                Expression* expr = block.expr;
                GenericScope* scope = block.scope;

                printf("ELSE IF: {\n");
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
                genPrintHelper("}");
            }

            if(!hasElse) printf("\n");
            else printf(" ");

            if(hasElse) {
                // statements
                printf("ELSE: {\n");
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
            printf("\n");
        } break;
        case StatementType_DIRECTIVE: break;
    }
}

void GlobalScopePrint(GlobalScope* scope, u64 indent) {
    genPrintHelper("Scope variables:\n");
    HashmapFor(String, StatementPtr, it, &scope->variables) {
        String id = it->key;
        Statement* value = it->value;
        assertf(value->type == StatementType_VAR_DECL || value->type == StatementType_VAR_DECL_ASSIGN, "Expected variable, got: %s", StatementTypeStr[value->type]);
        ParsedType* type = 0;
        Expression* expr = 0;
        if(value->type == StatementType_VAR_DECL) {
            type = value->statement.VAR_DECL.type;
        } else if(value->type == StatementType_VAR_DECL_ASSIGN) {
            type = value->statement.VAR_DECL_ASSIGN.type;
            expr = value->statement.VAR_DECL_ASSIGN.expr;
        }

        VariablePrint(id, expr, DebugParsedTypeToType(type));
    }

    genPrintHelper("Scope constants:\n");
    HashmapFor(String, ExpressionPtr, it, &scope->constants) {
        String id = it->key;
        Expression* expr = it->value;
        genPrintHelper("VAR_CONST: {\n");
        genPrintHelper("    name: "STR_FMT",\n", STR_PRINT(id));
        genPrintHelper("    expr: ");
        ExpressionPrint(expr, indent + 1);
        printf(",\n");
        genPrintHelper("}\n");
    }
}

void GenericScopePrint(GenericScope* scope, u64 indent) {
    genPrintHelper("Scope constants:\n");
    HashmapFor(String, ExpressionPtr, it, &scope->constants) {
        String id = it->key;
        Expression* expr = it->value;
        genPrintHelper("VAR_CONST: "STR_FMT, STR_PRINT(id));
        ExpressionPrint(expr, indent + 1);
        printf(",\n");
    }

    genPrintHelper("Scope statements:\n");
    for(u64 i = 0; i < scope->statements.size; ++i) {
        Statement* statement = scope->statements.data[i];
        StatementPrint(statement, indent + 1);
    }
}
#undef genPrintHelper

void RootPrint(Scope scope) {
    switch(scope.type) {
        case ScopeType_NONE: {
            UNREACHABLE("ScopeType_NONE is invalid: cannot be printed");
        } break;
        case ScopeType_GLOBAL: {
            GlobalScopePrint(scope.scope.as_global, 0);
        } break;
        case ScopeType_GENERIC: {
            GenericScopePrint(scope.scope.as_generic, 0);
        } break;
    }

    arena_free(&debugArena);
}

#endif // COMP_PARSER_DEBUG_H
