#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include "common.h"
#include "parser.h"
#include "types.h"
#include "dataStructures.h"
#include "commonTypes.h"

typedef struct TypecheckedScope TypecheckedScope;
typedef struct TypecheckedScope* TypecheckedScopePtr;
defArray(TypecheckedScopePtr);

typedef struct ConstValue {
    TypeInfo* typeInfo;
    union {
        u64 as_u64;
        s64 as_s64;
        f64 as_f64;
        String as_String;
        bool as_bool;
        TypecheckedScope* as_function;
        TypeInfo* as_type;
        TypecheckedScope* as_structDef;
        struct {
            Token id;
            bool idProvided;

            StructInitializerListType type;
            union {
                Array(ExpressionPtr) positionalInitializerList;
                Array(NamedInitializer) namedInitializerList;
            };
        } as_structLit;
    };
} ConstValue;
defArray(ConstValue);

typedef struct TypeResult {
    bool err;
    TypeInfo* typeInfo;
} TypeResult;

typedef struct ConstResult {
    bool err;
    ConstValue value;
} ConstResult;

typedef struct EvaluateConstantResult {
    bool error;
    ConstValue val;
} EvaluateConstantResult;

defHashmapFuncs(String, ConstValue)
defHashmapFuncs(String, TypeInfoPtr)

typedef struct TypecheckedExpression TypecheckedExpression;
typedef TypecheckedExpression* TypecheckedExpressionPtr;
defArray(TypecheckedExpressionPtr);

typedef struct TypecheckedNamedInitializer {
    String id;
    TypecheckedExpression* expr;
} TypecheckedNamedInitializer;
defArray(TypecheckedNamedInitializer);

typedef struct TypecheckedExpression {
    ExpressionType type;
    union {
        struct {
            Token operator;
            TypecheckedExpression* rhs;
            TypecheckedExpression* lhs;
        } BINARY_EXPRESSION;
        struct {
            Token operator;
            TypecheckedExpression* expr;
        } UNARY_EXPRESSION;
        struct {
            String value;
        } INT_LIT;
        struct {
            String wholePart;
            String fractPart;
        } FLOAT_LIT;
        struct {
            String value;
        } STRING_LIT;
        struct {
            String value;
        } BOOL_LIT;
        struct {
            String identifier;
        } SYMBOL;
        struct {
            String identifier;
            Array(TypecheckedExpressionPtr) args;
        } FUNCTION_CALL;
        struct {
            FunctionInfo typeInfo;
            GenericScope* scope;
        } FUNCTION_LIT;
        struct {
            ParsedType* typeInfo;
        } TYPE;
        struct {
            Token id;
            bool idProvided;

            StructInitializerListType type;
            union {
                Array(TypecheckedExpressionPtr) positionalInitializerList;
                Array(TypecheckedNamedInitializer) namedInitializerList;
            };
        } STRUCT_LIT;
        struct {
            Token variableName;
            Token fieldName;
        } FIELD_ACCESS;
        struct {
            Token id;
            u64 index; // TODO: make this into and Expression, so that you can do more complicated indexing
        } ARRAY_ACCESS;
    } expr;
    // NOTE: this needs to stay here so that the rest of the stuct can just be `memcpy`ed from Expression struct
    TypeInfo* typeInfo;
} TypecheckedExpression;

typedef struct TypechekedConditionalBlock {
    TypecheckedExpression* expr;
    TypecheckedScope* scope;
} TypechekedConditionalBlock;

defArray(TypechekedConditionalBlock);

typedef struct TypecheckedStatement {
    StatementType type;
    union {
        // NOTE: VAR_DECL_ASSIGN, VAR_REASSIGN and ARRAY_REASSIGN got combined into VAR_ACCESS
        struct {
            String identifier;
            TypecheckedExpression* expr;
            bool isArray;
            u64 index;
        } VAR_ACCESS;
        struct {
            TypecheckedExpression* expr;
        } RET;
        struct {
            Array(TypechekedConditionalBlock) blocks;
            bool hasElse;
            TypecheckedScope* elze;
        } IF;
        struct {
            TypecheckedExpression* expr;
            TypecheckedScope* scope;
        } LOOP;
        struct {
            TypecheckedExpression* expr;
        } EXPRESSION;
    } node;
} TypecheckedStatement;

defArray(TypecheckedStatement);
defHashmapFuncs(String, TypecheckedStatement)

typedef struct TypecheckedScope {
    TypecheckedScope* parent; // NOTE: if the parent is NULL this is the global scope
    Hashmap(String, TypeInfoPtr) variables;
    Hashmap(String, TypeInfoPtr) parameters; // function paremeters in function scopes
    Hashmap(String, ConstValue)  functions;
    Hashmap(String, TypeInfoPtr) structs;
    // Hashmap(String, TypeInfoPtr) enums; // TODO: enums not yet implemented

    Array(TypecheckedStatement) statements;
    Array(TypecheckedScopePtr) children;
} TypecheckedScope;

TypecheckedScope* typecheckScope(Arena* mem, GenericScope* scope, TypecheckedScope* parent, Hashmap(String, ConstValue)* constants, TypeInfo* expectedReturnType);
void typecheckScopeInto(Arena* mem, GenericScope* scope, TypecheckedScope* result, Hashmap(String, ConstValue)* constants, TypeInfo* expectedReturnType);
TypecheckedScope* typecheckGlobalScope(Arena* mem, GlobalScope* scope, Hashmap(String, ConstValue)* constants);
TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult);

#endif // TYPECHECKER_H
