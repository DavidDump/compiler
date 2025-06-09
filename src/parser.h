#ifndef COMP_PARSER_NEW_H
#define COMP_PARSER_NEW_H

#define BIN_OPERATORS_COUNT 13

typedef struct Scope Scope;
typedef Scope* ScopePtr;

typedef struct GenericScope GenericScope;
typedef struct GlobalScope GlobalScope;

typedef struct ParsedType ParsedType;

typedef struct Expression Expression;
typedef Expression* ExpressionPtr;

typedef struct Statement Statement;
typedef Statement* StatementPtr;

typedef struct ConditionalBlock ConditionalBlock;
typedef struct ParseContext ParseContext;
typedef struct Operator Operator;
typedef struct ParseResult ParseResult;

#include "dataStructuresDefs.h"

defArray(ScopePtr);
defArray(ExpressionPtr);
defHashmapFuncs(String, ExpressionPtr)
defArray(StatementPtr);
defHashmapFuncs(String, StatementPtr)
defArray(ConditionalBlock);

#include "string.h"
#include "arena.h"
#include "types.h"
#include "lexer.h"

typedef enum ParsedTypeType {
    ParsedTypeType_NONE,
    ParsedTypeType_SIMPLE,
    ParsedTypeType_STRUCT,
    ParsedTypeType_SYMBOL,
} ParsedTypeType;

typedef struct ParsedType {
    ParsedTypeType type;
    union {
        struct {
            TypeInfo* typeInfo;
        } as_simple;
        struct {
            GlobalScope* scope;

            // NOTE: only for later once parameterized structs get added
            // Array(FunctionArg) args;
            // bool hasArgs;
        } as_struct;
        struct {
            Token identifier;
        } as_symbol;
    };
} ParsedType;

typedef enum StructInitializerListType {
    StructInitializerListType_NONE,
    StructInitializerListType_POSITIONAL,
    StructInitializerListType_DESIGNATED,
} StructInitializerListType;

typedef enum ExpressionType {
    // complex nodes
    ExpressionType_BINARY_EXPRESSION, // lsh + rhs
    ExpressionType_UNARY_EXPRESSION,  // -expr

    // leaf nodes
    ExpressionType_INT_LIT,           // 1
    ExpressionType_FLOAT_LIT,         // 1.0
    ExpressionType_STRING_LIT,        // "string"
    ExpressionType_BOOL_LIT,          // true/false
    ExpressionType_SYMBOL,            // foo
    ExpressionType_FUNCTION_CALL,     // bar()
    ExpressionType_FUNCTION_LIT,      // (arg: u64) -> u8 { ... }
    ExpressionType_TYPE,              // u8 OR struct { ... } OR symbol
    ExpressionType_STRUCT_LIT,        // Vec2{1, 2} OR Vec2{.x = 1, .y = 2} OR {1, 2} OR {.x = 1, .y = 2}
    ExpressionType_FIELD_ACCESS,      // vec.x
} ExpressionType;

extern char* ExpressionTypeStr[];

typedef struct NamedInitializer {
    String id;
    Expression* expr;
} NamedInitializer;
defArray(NamedInitializer);

typedef struct Expression {
    ExpressionType type;
    union Expr {
        struct BINARY_EXPRESSION {
            Token operator;
            Expression* rhs;
            Expression* lhs;
        } BINARY_EXPRESSION;
        struct UNARY_EXPRESSION {
            Token operator;
            Expression* expr;
        } UNARY_EXPRESSION;
        struct INT_LIT {
            String value;
        } INT_LIT;
        struct FLOAT_LIT {
            String wholePart;
            String fractPart;
        } FLOAT_LIT;
        struct STRING_LIT {
            String value;
        } STRING_LIT;
        struct BOOL_LIT {
            String value;
        } BOOL_LIT;
        struct SYMBOL {
            String identifier;
        } SYMBOL;
        struct FUNCTION_CALL {
            String identifier;
            Array(ExpressionPtr) args;
        } FUNCTION_CALL;
        struct FUNCTION_LIT {
            FunctionInfo* typeInfo;
            GenericScope* scope;
        } FUNCTION_LIT;
        struct TYPE {
            ParsedType* type;
        } TYPE;
        struct STRUCT_LIT {
            Token id;
            bool idProvided;

            StructInitializerListType type;
            union {
                Array(ExpressionPtr) positionalInitializerList;
                Array(NamedInitializer) namedInitializerList;
            };
        } STRUCT_LIT;
        struct FIELD_ACCESS {
            Token variableName;
            Token fieldName;
        } FIELD_ACCESS;
    } expr;
} Expression;

typedef enum StatementType {
    StatementType_NONE,
    
    StatementType_VAR_DECL,
    StatementType_VAR_DECL_ASSIGN,
    StatementType_VAR_REASSIGN,
    StatementType_VAR_CONST,
    StatementType_RET,
    StatementType_IF,
    StatementType_LOOP,
    StatementType_EXPRESSION,
    StatementType_DIRECTIVE,
    
    StatementType_COUNT,
} StatementType;

extern char* StatementTypeStr[StatementType_COUNT + 1];

typedef enum ScopeType {
    ScopeType_NONE,
    ScopeType_GLOBAL,
    ScopeType_GENERIC,
} ScopeType;

typedef struct Scope {
    ScopeType type;
    union {
        GenericScope* as_generic;
        GlobalScope* as_global;
    } scope;
} Scope;

typedef struct GenericScope {
    Scope parent;
    Hashmap(String, ExpressionPtr) constants;
    Array(StatementPtr) statements;
} GenericScope;

typedef struct GlobalScope {
    Scope parent;
    Hashmap(String, ExpressionPtr) constants;
    Hashmap(String, StatementPtr) variables;
} GlobalScope;

typedef struct ConditionalBlock {
    Expression* expr;
    GenericScope* scope;
} ConditionalBlock;

// not really an Statement anymore, should be renamed to Statement
typedef struct Statement {
    StatementType type;
    union {
        struct VAR_DECL {
            String identifier;
            ParsedType* type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            ParsedType* type;
            Expression* expr;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            Expression* expr;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            Expression* expr;
        } VAR_CONST;
        struct RET {
            Expression* expr;
        } RET;
        struct IF {
            Array(ConditionalBlock) blocks;
            bool hasElse;
            GenericScope* elze;
        } IF;
        struct LOOP {
            Expression* expr;
            GenericScope* scope;
        } LOOP;
        struct EXPRESSION {
            Expression* expr;
        } EXPRESSION;
    } statement;
} Statement;

typedef struct ParseContext {
    Array(Token) tokens;
	u64 index;
    Hashmap(String, LibName) importLibraries;
    String currentImportLibraryName;
    String currentSymbolName; // TODO: scuffed solution, so i dont have to pass the symbol name to `parseExpression()`

    // NOTE: this is a hack,
    //       Some expressions dont need a semicolon at the end, but the check happens outside of `parseExpression`,
    //       so we dont know what the expression type is. So we set this flag in the `parseLeaf` function,
    //       to indicate that once we return from `parseExpression`, we should skip the check.
    //       After the check should be cleared to false.
    bool skipSemicolon;
} ParseContext;

typedef struct Operator {
    TokenType type;
    s64 presedence;
} Operator;

typedef struct ParseResult {
    GlobalScope* globalScope;
    Hashmap(String, LibName) importLibraries;
} ParseResult;

Expression* parseExpression(ParseContext* ctx, Arena* mem);
Expression* parseDecreasingPresedence(ParseContext* ctx, Arena* mem, s64 minPrec);
Expression* parseLeaf(ParseContext* ctx, Arena* mem);
ParseResult Parse(Array(Token) tokens, Arena* mem);
Statement* parseStatement(ParseContext* ctx, Arena* mem, Scope containingScope);
ParsedType* parseType(ParseContext* ctx, Arena* mem);
bool isFunctionLit(ParseContext* ctx, Token next);
Scope makeScopeFromGlobal(GlobalScope* scope);
Scope makeScopeFromGeneric(GenericScope* scope);

bool parseCheckSemicolon(ParseContext* ctx);
// void parseGenericScopeInto(ParseContext* ctx, Arena* mem, GenericScope* target);
GlobalScope* parseGlobalScopeInto(ParseContext* ctx, Arena* mem, GlobalScope* globalScope);

#endif // COMP_PARSER_NEW_H
// TODO: instead of storing symbol information (like function, variable, constant, declaration) in hashmaps
//       store it in arrays and every time there is a `String identifier` in the AST
//       just store the index to that symbol in the "global" table
//       hashmaps are always slower that arrays,
//       this way once parsing is done all symbols are referred to using indecies, which makes typecking and codegen faster
