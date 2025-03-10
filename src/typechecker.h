#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include "common.h"
#include "parser.h"
#include "types.h"
#include "dataStructures.h"
#include "commonTypes.h"

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
defArray(ConstValue);

typedef struct TypeResult {
    bool err;
    TypeInfo* typeInfo;
} TypeResult;

typedef struct ConstResult {
    bool err;
    ConstValue value;
} ConstResult;

typedef enum ConstantEvaluationError {
    ConstantEvaluationError_NONE,
    ConstantEvaluationError_UNDEFINED_SYMBOL, // symbol has not yet been evaluated, defer for later
    ConstantEvaluationError_FUNCTION_LITERAL, // symbol is of type funtion literal and should be evaluated in the second pass
} ConstantEvaluationError;

typedef struct EvaluateConstantResult {
    ConstantEvaluationError error;
    ConstValue val;
} EvaluateConstantResult;

defHashmapFuncs(String, ConstValue)
defHashmapFuncs(String, TypeInfoPtr)

typedef struct TypecheckedExpression TypecheckedExpression;
typedef TypecheckedExpression* TypecheckedExpressionPtr;
defArray(TypecheckedExpressionPtr);
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
            TypeInfo* returnType;
            Array(FunctionArg) args;
            Scope* scope;
            bool isExtern;
        } FUNCTION_LIT;
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
    ASTNodeType type;
    union {
        // NOTE: VAR_DECL_ASSIGN and VAR_REASSIGN got combined into VAR_ACCESS
        struct {
            String identifier;
            TypecheckedExpression* expr;
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

// NOTE: result of the typechecking operation should have
//       - list of all the functions to generate, this includes the ones that are defined inside another function body
//       - each scope should contain a list constants defined in that scope
typedef struct TypecheckedScope {
    struct TypecheckedScope* parent;
    // TODO: is this needed anywhere, maybe remove `params`
    Array(StringAndType) params;            // in case the scope is a function scope these are the parameters of the function
    Hashmap(String, ConstValue) constants;  // TODO: remove constants, the value of a constant should replace the leaf SYMBOL node in the Expression
    Hashmap(String, TypeInfoPtr) variables; // global or local variables, depending on if this is the toplevel scope
    // global variables need to be constant, and evaluated
    // local variables only need to have a known type during compile time

    // TODO: this is probably not a good idea, remove later
    Array(u64) functionIndicies; // the indicies in the constants hashmap which contain function literals
    Array(TypecheckedStatement) statements;
} TypecheckedScope;

TypecheckedScope* typecheckScope(Arena* mem, Scope* scope, TypecheckedScope* parent, TypeInfo* expectedReturnType, bool isTopLevel);
TypecheckedScope* typecheck(Arena* mem, ParseResult* parseResult);

#endif // TYPECHECKER_H
