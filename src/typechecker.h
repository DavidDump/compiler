#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include "common.h"
#include "parser.h"
#include "types.h"
#include "dataStructures.h"

typedef struct TypecheckedScope TypecheckedScope;

typedef struct ConstValue {
    TypeInfo* typeInfo;
    union {
        u64 as_u64;
        s64 as_s64;
        f64 as_f64;
        String as_String;
        bool as_bool;
        TypecheckedScope* as_function;
    };
} ConstValue;

typedef struct TypeResult {
    bool err;
    TypeInfo* typeInfo;
} TypeResult;

typedef enum ConstantEvaluationError {
    ConstantEvaluationError_NONE,
    ConstantEvaluationError_UNDEFINED_SYMBOL, // symbol has not yet been evaluated, defer for later
    ConstantEvaluationError_FUNCTION_LITERAL, // symbol is of type funtion literal and should be evaluated in the second pass
} ConstantEvaluationError;

typedef struct EvaluateConstantResult {
    ConstantEvaluationError error;
    ConstValue val;
} EvaluateConstantResult;

defHashmapFuncs(String, ConstValue);
defHashmapFuncs(String, TypeInfoPtr);
defArray(u64);

typedef enum BaseTypesIndex {
    BaseTypesIndex_S64,
    BaseTypesIndex_F64,
    BaseTypesIndex_STRING,
    BaseTypesIndex_BOOL,
    BaseTypesIndex_COUNT,
} BaseTypesIndex;

typedef struct TypechekedConditionalBlock {
    Expression* expr;
    TypecheckedScope* scope;
    TypeInfo* type;
} TypechekedConditionalBlock;

defArray(TypechekedConditionalBlock);

typedef struct TypecheckedStatement {
    ASTNodeType type;
    union Node {
        // NOTE: VAR_DECL_ASSIGN and VAR_REASSIGN got combined into VAR_ACCESS
        struct VAR_ACCESS {
            String identifier;
            Expression* expr;
            TypeInfo* type;
        } VAR_ACCESS;
        struct RET {
            Expression* expr;
            TypeInfo* type;
        } RET;
        struct IF {
            Array(TypechekedConditionalBlock) blocks;
            bool hasElse;
            TypecheckedScope* elze;
        } IF;
        struct LOOP {
            Expression* expr;
            TypecheckedScope* scope;
            TypeInfo* type;
        } LOOP;
        struct EXPRESSION {
            Expression* expr;
            TypeInfo* type;
        } EXPRESSION;
    } node;
} TypecheckedStatement;

defArray(TypecheckedStatement);

// NOTE: result of the typechecking operation should have
//       - list of all the functions to generate, this includes the ones that are defined inside another function body
//       - each scope should contain a list constants defined in that scope
typedef struct TypecheckedScope {
    struct TypecheckedScope* parent;
    Hashmap(String, ConstValue) constants;
    Hashmap(String, TypeInfoPtr) variables; // global or local variables, depending on if this is the toplevel scope

    Array(u64) functionIndicies; // the indicies in the constants hashmap which contain function literals
    Array(TypecheckedStatement) statements;
} TypecheckedScope;

TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult);

#endif // TYPECHECKER_H
