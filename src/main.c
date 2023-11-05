#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <stdlib.h> // calloc(), realloc(), free(), system()
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

// TODO: remove debug mode
#define COMP_DEBUG

typedef int bool;
#define TRUE 1
#define FALSE 0

#define UNUSED(x) (void)(x)
#define ERROR(loc, msg) printf("[ERROR] %.*s:%i:%i %s\n", (loc).filename.length, (loc).filename.str, (loc).line, (loc).collum, msg)

typedef struct String{
    const char* str;
    int length;
} String;

typedef struct StringNode StringNode;

typedef struct StringNode{
    String str;
    StringNode* next;
    StringNode* prev;
} StringNode;

typedef struct StringChain{
    StringNode* first;
    StringNode* last;
    int nodeCount; // dont know if need
} StringChain;

void StringChainAppend(StringChain* chain, Arena* mem, String str){
    StringNode* node = arena_alloc(mem, sizeof(StringNode));
    node->str = str;
    node->prev = chain->last;
    if(chain->last) chain->last->next = node;
    chain->last = node;
    if(!chain->first) chain->first = node;
    chain->nodeCount++;
}

// TODO: memory allocation in arena that gets passed in
String StringFromCstr(const char* cstr){
    String result = {};
    int len = strlen(cstr);
    result.str = calloc(len, sizeof(char));
    memcpy((char*)result.str, cstr, len);
    result.length = len;
    return result;
}

String StringFromArray(const char* arr, int size){
    String result = {};
    result.str = calloc(size, sizeof(char));
    memcpy((char*)result.str, arr, size);
    result.length = size;
    return result;
}

bool StringEquals(String str1, String str2){
    if(str1.length != str2.length) return FALSE;
    for(int i = 0; i < str1.length; i++){
        if(str1.str[i] != str2.str[i]) return FALSE;
    }
    return TRUE;
}

bool StringEqualsCstr(String str1, const char* cstr){
    if(str1.length != strlen(cstr)) return FALSE;
    for(int i = 0; i < str1.length; i++){
        if(str1.str[i] != cstr[i]) return FALSE;
    }
    return TRUE;
}

String EntireFileRead(const char* filePath){
    FILE* f = fopen(filePath, "rb");
    
    if(f){
        fseek(f, 0, SEEK_END);
        int fileSize = ftell(f);
        fseek(f, 0, SEEK_SET);
        
        char* tmpFileBuffer = calloc(fileSize + 1, sizeof(char));
        fread(tmpFileBuffer, sizeof(char), fileSize, f);
        fclose(f);

        String str = StringFromCstr(tmpFileBuffer);
        free(tmpFileBuffer);
        return str;
    }else{
        printf("[ERROR] Failed to open file: %s\n", filePath);
        return (String){};
    }
}

bool EntireFileWrite(const char* filePath, StringChain data){
    FILE* f = fopen(filePath, "wb");

    if(f){
        StringNode* current = data.first;
        while(current != NULL){
            fprintf(f, "%.*s", current->str.length, current->str.str);
            current = current->next;
        }
        fclose(f);
        return TRUE;
    }else{
        printf("[ERROR] Failed to open file: %s\n", filePath);
        return FALSE;
    }
}

typedef struct Tokenizer{
    String source;
    String filename;
    int index;
} Tokenizer;

typedef struct Location{
    String filename;
    int line;
    int collum;
} Location;

typedef enum TokenType{
    TokenType_NONE,
    
    TokenType_RETURN,
    TokenType_TYPE,
    TokenType_SEMICOLON,
    TokenType_COLON,
    TokenType_DOUBLECOLON,
    TokenType_RARROW,
    TokenType_LPAREN,
    TokenType_RPAREN,
    TokenType_LSCOPE,
    TokenType_RSCOPE,
    TokenType_LBRACKET,
    TokenType_RBRACKET,
    TokenType_COMMA,
    TokenType_OPERATOR,
    TokenType_ASSIGNMENT,
    TokenType_COMPARISON,
    TokenType_IDENTIFIER,
    TokenType_INT_LITERAL,
    
    TokenType_COUNT,
} TokenType;

static char* TokenTypeStr[TokenType_COUNT + 1] = {
    [TokenType_NONE]        = "NONE",
    
    [TokenType_RETURN]      = "RETURN",
    [TokenType_TYPE]        = "TYPE",
    [TokenType_SEMICOLON]   = "SEMICOLON",
    [TokenType_COLON]       = "COLON",
    [TokenType_DOUBLECOLON] = "DOUBLECOLON",
    [TokenType_RARROW]      = "RARROW",
    [TokenType_LPAREN]      = "LPAREN",
    [TokenType_RPAREN]      = "RPAREN",
    [TokenType_LSCOPE]      = "LSCOPE",
    [TokenType_RSCOPE]      = "RSCOPE",
    [TokenType_LBRACKET]    = "LBRACKET",
    [TokenType_RBRACKET]    = "RBRACKET",
    [TokenType_COMMA]       = "COMMA",
    [TokenType_OPERATOR]    = "OPERATOR",
    [TokenType_ASSIGNMENT]  = "ASSIGNMENT",
    [TokenType_COMPARISON]  = "COMPARISON",
    [TokenType_IDENTIFIER]  = "IDENTIFIER",
    [TokenType_INT_LITERAL] = "INT_LITERAL",
    
    [TokenType_COUNT]       = "COUNT",
};

typedef struct Token{
    TokenType type;
    String value;
    Location loc;
} Token;

typedef struct TokenArray{
    Token* tokens;
    int size;
    int capacity;
} TokenArray;

char TokenizerPeek(Tokenizer* tokenizer, int offset){
    if(tokenizer->index + offset > tokenizer->source.length || tokenizer->index + offset < 0) return 0;
    return tokenizer->source.str[tokenizer->index + offset];
}

char TokenizerConsume(Tokenizer* tokenizer){
    if(tokenizer->index + 1 > tokenizer->source.length) return 0;
    return tokenizer->source.str[tokenizer->index++];
}

bool isLetter(char c){
    return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'));
}

bool isNumber(char c){
    return ('0' <= c && c <= '9');
}

bool isWhitespace(char c){
    return (c == ' ' || c == '\n' || c == '\t' || c == '\v' || c == '\r');
}

#if 0
bool isSpecial(char c){
    return (c == ';');
}
#endif

bool isOperator(char c){
    return (c == '+' || c == '-' || c == '*' || c == '/');
}

void TokenArrayAddToken(TokenArray* arr, String value, TokenType type, String filename, int line, int collum){
    if(arr->size >= arr->capacity){
        size_t newCap = arr->capacity * 2;
        if(newCap == 0) newCap = 1;
        arr->tokens = realloc(arr->tokens, newCap * sizeof(Token));
        arr->capacity = newCap;
    }

    Token tok = {};
    tok.value = value;
    tok.type = type;
    tok.loc.filename = filename;
    tok.loc.line = line;
    tok.loc.collum = collum;
    arr->tokens[arr->size++] = tok;
}

TokenArray Tokenize(Tokenizer* tokenizer){
    TokenArray tokens = {};
    int lineNum = 1;
    int collumNum = 1;
    for(char c = TokenizerConsume(tokenizer); c != 0; c = TokenizerConsume(tokenizer), collumNum++){
        // special case: line comment
        if(c == '/'){
            char next = TokenizerPeek(tokenizer, 0);
            if(next == '/'){
                while((c = TokenizerConsume(tokenizer)) != '\n');
            }
        }

        if(isLetter(c)){
            // keywords and identifiers
            char* start = (char*)&tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && (isLetter(*end) || isNumber(*end))) end++;
            int len = end - start;

            String value = StringFromArray(start, len);
            TokenType type = TokenType_NONE;
            // compare keywords and types
            if(StringEqualsCstr(value, "return"))   type = TokenType_RETURN;
            else if(StringEqualsCstr(value, "u8"))  type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "u16")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "u32")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "u64")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "s8"))  type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "s16")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "s32")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "s64")) type = TokenType_TYPE;
            else if(StringEqualsCstr(value, "string")) type = TokenType_TYPE;
            else type = TokenType_IDENTIFIER;

            TokenArrayAddToken(&tokens, value, type, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isNumber(c)){
            // numbers
            char* start = (char*)&tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && isNumber(*end)) end++;
            int len = end - start;

            TokenArrayAddToken(&tokens, StringFromArray(start, len), TokenType_INT_LITERAL, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isOperator(c)){
            // operators

            // special case -> operator
            if(c == '-'){
                char next = TokenizerPeek(tokenizer, 0);
                if(next == '>'){
                    TokenArrayAddToken(&tokens, StringFromCstr("->"), TokenType_RARROW, tokenizer->filename, lineNum, collumNum);
                    
                    TokenizerConsume(tokenizer);
                    collumNum++;
                    continue;
                }
            }

            char* curr = (char*)&tokenizer->source.str[tokenizer->index - 1];
            TokenArrayAddToken(&tokens, StringFromArray(curr, 1), TokenType_OPERATOR, tokenizer->filename, lineNum, collumNum);
        }else if(c == ';'){
            // semicolon
            TokenArrayAddToken(&tokens, StringFromCstr(";"), TokenType_SEMICOLON, tokenizer->filename, lineNum, collumNum);
        }else if(c == '='){
            // equals
            char next = TokenizerPeek(tokenizer, 0);
            if(next == '='){
                TokenArrayAddToken(&tokens, StringFromCstr("=="), TokenType_COMPARISON, tokenizer->filename, lineNum, collumNum);
                
                TokenizerConsume(tokenizer);
                collumNum++;
            }else{
                TokenArrayAddToken(&tokens, StringFromCstr("="), TokenType_ASSIGNMENT, tokenizer->filename, lineNum, collumNum);
            }
        }else if(c == ':'){
            // : or :: operator
            char next = TokenizerPeek(tokenizer, 0);
            if(next == ':'){
                // :: operator
                TokenArrayAddToken(&tokens, StringFromCstr("::"), TokenType_DOUBLECOLON, tokenizer->filename, lineNum, collumNum);
                
                TokenizerConsume(tokenizer);
                collumNum++;
            }else{
                // : operator
                TokenArrayAddToken(&tokens, StringFromCstr(":"), TokenType_COLON, tokenizer->filename, lineNum, collumNum);
            }
        }else if(c == '('){
            // left paren
            TokenArrayAddToken(&tokens, StringFromCstr("("), TokenType_LPAREN, tokenizer->filename, lineNum, collumNum);
        }else if(c == ')'){
            // right paren
            TokenArrayAddToken(&tokens, StringFromCstr(")"), TokenType_RPAREN, tokenizer->filename, lineNum, collumNum);
        }else if(c == '{'){
            // open scope
            TokenArrayAddToken(&tokens, StringFromCstr("{"), TokenType_LSCOPE, tokenizer->filename, lineNum, collumNum);
        }else if(c == '}'){
            // close scope
            TokenArrayAddToken(&tokens, StringFromCstr("}"), TokenType_RSCOPE, tokenizer->filename, lineNum, collumNum);
        }else if(c == '['){
            // left bracket
            TokenArrayAddToken(&tokens, StringFromCstr("["), TokenType_LBRACKET, tokenizer->filename, lineNum, collumNum);
        }else if(c == ']'){
            // right bracket
            TokenArrayAddToken(&tokens, StringFromCstr("]"), TokenType_RBRACKET, tokenizer->filename, lineNum, collumNum);
        }else if(c == ','){
            // comma
            TokenArrayAddToken(&tokens, StringFromCstr(","), TokenType_COMMA, tokenizer->filename, lineNum, collumNum);
        }else if(c == '\n'){
            lineNum++;
            collumNum = 0;
        }
        
        #ifdef COMP_DEBUG
        else if(c == ' '){
            // NOTE: space is ignored but this case is needed here for debug print
            continue;
        }else if(c == '\r'){
            // NOTE: carrige return is ignored but this case is needed here for debug print
            continue;
        }else{
            printf("[ERROR] Unhandled char by the tokenizer: \'%c\' at %.*s:%i:%i\n", c, tokenizer->filename.length, tokenizer->filename.str, lineNum, collumNum);
        }
        #endif // COMP_DEBUG
    }
    return tokens;
}

void TokensPrint(TokenArray* tokens){
    for(int i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        printf("%.*s:%i:%i\t { %-12s %-8.*s }\n", t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum, TokenTypeStr[t.type], t.value.length, t.value.str);
    }
}

// TODO: move into separate file so forward declarations are a bit nicer
typedef struct NodeExpresion NodeExpresion;

typedef struct NodeBinExpresion{
    Token* left;
    Token* operator;
    NodeExpresion* right;
} NodeBinExpresion;

typedef enum NodeExpresionType{
    NodeExpresionType_NONE,
    NodeExpresionType_INT_LIT,
    NodeExpresionType_BIN_EXP,
    NodeExpresionType_COUNT,
} NodeExpresionType;

static char* NodeExpresionTypeStr[NodeExpresionType_COUNT + 1] = {
    [NodeExpresionType_NONE]    = "NONE",
    [NodeExpresionType_INT_LIT] = "INT_LIT",
    [NodeExpresionType_BIN_EXP] = "BIN_EXP",
    [NodeExpresionType_COUNT]   = "COUNT",
};

typedef struct NodeExpresion{
    NodeExpresionType type;
    union{
        Token* intLit;
        NodeBinExpresion* binExp;
    };
} NodeExpresion;

typedef struct NodeKeywordRet{
    NodeExpresion* exp;
} NodeKeywordRet;

typedef struct NodeKeywordVarDefAssignment{
    Token* identifier;
    NodeExpresion* exp;
} NodeKeywordVarDefAssignment;

typedef enum NodeKeywordVarType{
    NodeKeywordVarType_NONE,
    NodeKeywordVarType_DEF_ONLY,
    NodeKeywordVarType_DEF_ASSIGNMENT,
    NodeKeywordVarType_COUNT,
} NodeKeywordVarType;

static char* NodeKeywordVarTypeStr[NodeKeywordVarType_COUNT + 1] = {
    [NodeKeywordVarType_NONE]           = "NONE",
    [NodeKeywordVarType_DEF_ONLY]       = "DEF_ONLY",
    [NodeKeywordVarType_DEF_ASSIGNMENT] = "DEF_ASSIGNMENT",
    [NodeKeywordVarType_COUNT]          = "COUNT",
};

typedef struct NodeKeywordVar{
    NodeKeywordVarType type;
    union{
        Token* defIdentifier;
        NodeKeywordVarDefAssignment* assignment;
    };
} NodeKeywordVar;

typedef enum NodeKeywordType{
    NodeKeywordType_NONE,
    NodeKeywordType_RET,
    NodeKeywordType_VAR,
    NodeKeywordType_COUNT,
} NodeKeywordType;

static char* NodeKeywordTypeStr[NodeKeywordType_COUNT + 1] = {
    [NodeKeywordType_NONE]  = "NONE",
    [NodeKeywordType_RET]   = "RET",
    [NodeKeywordType_VAR]   = "VAR",
    [NodeKeywordType_COUNT] = "COUNT",
};

typedef struct NodeKeyword{
    NodeKeywordType type;
    union{
        NodeKeywordRet* ret;
        NodeKeywordVar* var;
    };
} NodeKeyword;

typedef enum NodeStatementType{
    NodeStatementType_NONE,
    NodeStatementType_KEYWORD,
    NodeStatementType_EXPRESION,
    NodeStatementType_COUNT,
} NodeStatementType;

static char* NodeStatementTypeStr[NodeStatementType_COUNT + 1] = {
    [NodeStatementType_NONE]        = "NONE",
    [NodeStatementType_KEYWORD]     = "KEYWORD",
    [NodeStatementType_EXPRESION]   = "EXPRESION",
    [NodeStatementType_COUNT]       = "COUNT",
};

typedef struct NodeStatement{
    NodeStatementType type;
    union{
        NodeExpresion* exp;
        NodeKeyword* keyword;
    };
} NodeStatement;

typedef struct NodeRoot{
    NodeStatement* stmts;
    int count;
    int capacity;
} NodeRoot;

typedef struct Parser{
    TokenArray tokens;
    int index;
    Arena mem;
    NodeRoot root;
} Parser;

Token* ParserPeek(Parser* parser, int offset){
    if(parser->index + offset > parser->tokens.size || parser->index + offset < 0) return NULL;
    return &parser->tokens.tokens[parser->index + offset];
}

Token* ParserConsume(Parser* parser){
    if(parser->index + 1 > parser->tokens.size) return NULL;
    return &parser->tokens.tokens[parser->index++];
}

// TODO: move into separate file so forward declarations are a bit nicer
NodeExpresion* ParseExpresion(Parser* parser);

NodeBinExpresion* ParseBinExpresion(Parser* parser){
    Token* left = ParserPeek(parser, 0);
    Token* operator = ParserPeek(parser, 1);
    Token* right = ParserPeek(parser, 2);
    if(left->type == TokenType_INT_LITERAL && operator->type == TokenType_OPERATOR && right->type == TokenType_INT_LITERAL){
        ParserConsume(parser); // consume left
        ParserConsume(parser); // consume operator
        // ParserConsume(parser); // consume right
        NodeBinExpresion* node = arena_alloc(&parser->mem, sizeof(NodeBinExpresion));
        node->left = left;
        node->operator = operator;
        node->right = ParseExpresion(parser);
        return node;
    }else{
        ERROR(left->loc, "Expresion malformed.");
        exit(EXIT_FAILURE);
        return NULL;
    }
}

// the index is pointing to the first literal in the expresion
NodeExpresion* ParseExpresion(Parser* parser){
    Token* t = ParserPeek(parser, 1); // look ahead to see if semicolon or operator follows
    if(t->type == TokenType_SEMICOLON){
        // semicolon
        Token* int_lit = ParserConsume(parser); // consume int_lit
        ParserConsume(parser); // consume semicolon
        NodeExpresion* node = arena_alloc(&parser->mem, sizeof(NodeExpresion));
        node->type = NodeExpresionType_INT_LIT;
        node->intLit = int_lit;
        return node;
    }else if(t->type == TokenType_OPERATOR){
        // binary expresion
        // ParserConsume(parser);
        NodeExpresion* node = arena_alloc(&parser->mem, sizeof(NodeExpresion));
        node->type = NodeExpresionType_BIN_EXP;
        node->binExp = ParseBinExpresion(parser);
        return node;
    }else{
        Token* current = ParserPeek(parser, 0);
        Location loc = current->loc;
        loc.collum += current->value.length; // ; pos should be at the end of the current token, not begining
        ERROR(loc, "Statement needs to end with a ;.");
        exit(EXIT_FAILURE);
        return NULL;
    }
}

NodeKeywordRet* ParseKeywordRet(Parser* parser){
    Token* t = ParserPeek(parser, 0);
    if(t->type == TokenType_INT_LITERAL){
        // dont consume the literal yet, the expresion parsing will consume it
        NodeKeywordRet* node = arena_alloc(&parser->mem, sizeof(NodeKeywordRet));
        node->exp = ParseExpresion(parser);
        return node;
    }else{
        Token* prev = ParserPeek(parser, -1);
        ERROR(prev->loc, "Return keyword needs a value to return.");
        exit(EXIT_FAILURE);
        return NULL;
    }
}

NodeKeywordVar* ParseKeywordVar(Parser* parser){
    Token* t = ParserPeek(parser, 0);
    if(t->type == TokenType_IDENTIFIER){
        NodeKeywordVar* node = arena_alloc(&parser->mem, sizeof(NodeKeywordVar));
        Token* next = ParserPeek(parser, 1);
        if(next->type == TokenType_SEMICOLON){
            // def only
            node->type = NodeKeywordVarType_DEF_ONLY;
            node->defIdentifier = t;
            return node;
        }else if(next->type == TokenType_ASSIGNMENT){
            // def and assign
            NodeKeywordVarDefAssignment* assignment = arena_alloc(&parser->mem, sizeof(NodeKeywordVarDefAssignment));
            assignment->identifier = t;
            ParserConsume(parser); // consume the identifier
            ParserConsume(parser); // consume the equals
            assignment->exp = ParseExpresion(parser);

            node->type = NodeKeywordVarType_DEF_ASSIGNMENT;
            node->assignment = assignment;
            return node;
        }else{
            ERROR(next->loc, "Identifier should be followed by a ; or expresion assignment.");
            exit(EXIT_FAILURE);
            return NULL;
        }
    }else{
        Token* prev = ParserPeek(parser, -1);
        ERROR(prev->loc, "Variable definition needs an identifier.");
        exit(EXIT_FAILURE);
        return NULL;
    }
}

void ParserAddStmt(Parser* parser, void* node, NodeStatementType type){
    if(parser->root.count >= parser->root.capacity){
        size_t newCap = parser->root.capacity * 2;
        if(newCap == 0) newCap = 1;
        parser->root.stmts = arena_realloc(&parser->mem, parser->root.stmts, parser->root.capacity * sizeof(NodeStatement), newCap * sizeof(NodeStatement));
        parser->root.capacity = newCap;
    }

    switch(type){
        case NodeStatementType_KEYWORD: {
            parser->root.stmts[parser->root.count].type = NodeStatementType_KEYWORD;
            parser->root.stmts[parser->root.count].keyword = (NodeKeyword*)node;
            parser->root.count++;
        } break;
        case NodeStatementType_EXPRESION: {
            parser->root.stmts[parser->root.count].type = NodeStatementType_EXPRESION;
            parser->root.stmts[parser->root.count].exp = (NodeExpresion*)node;
            parser->root.count++;
        } break;
        default: {
            printf("[ERROR] Parser cant add statement of type %s\n", NodeStatementTypeStr[type]);
            exit(EXIT_FAILURE);
        } break;
    }
}

NodeRoot Parse(Parser* parser){
    for(Token* t = ParserConsume(parser); t != NULL; t = ParserConsume(parser)){
        switch(t->type){
            case TokenType_RETURN: {
                NodeKeyword* node = arena_alloc(&parser->mem, sizeof(NodeKeyword));
                node->type = NodeKeywordType_RET;
                node->ret = ParseKeywordRet(parser);
 
                ParserAddStmt(parser, node, NodeStatementType_KEYWORD);
            } break;
            // case TokenType_VAR: {
            //     NodeKeyword* node = arena_alloc(&parser->mem, sizeof(NodeKeyword));
            //     node->type = NodeKeywordType_VAR;
            //     node->var = ParseKeywordVar(parser);

            //     ParserAddStmt(parser, node, NodeStatementType_KEYWORD);
            // } break;
            case TokenType_INT_LITERAL: {
                parser->index--; // rewind the parser to point to the first literal in the expresion
                NodeExpresion* node = ParseExpresion(parser);

                ParserAddStmt(parser, node, NodeStatementType_EXPRESION);
            } break;
            default: {
                printf("[ERROR] Unhandled token type by the parser: %s at %.*s:%i:%i\n", TokenTypeStr[t->type], t->loc.filename.length, t->loc.filename.str, t->loc.line, t->loc.collum);
            } break;
        }
    }
    return parser->root;
}

typedef struct Generator{
    Arena mem;
    StringChain outputAsm;
    int stackPointer;
    // NodeRoot root; // NOTE: maybe add ast here for consistency
} Generator;

void GeneratorPushStack(Generator* gen, const char* target){
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    push "));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(target));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
    // TODO: figure out what the stack size is to check for stack overflows
    // if(gen->stackPointer - 1 < 0){
    //     printf("[ERROR] Stack underflow\n");
    //     exit(EXIT_FAILURE);
    // }
    gen->stackPointer++;
}

void GeneratorPopStack(Generator* gen, const char* target){
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    pop "));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(target));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
    if(gen->stackPointer - 1 < 0){
        printf("[ERROR] Stack underflow\n");
        exit(EXIT_FAILURE);
    }
    gen->stackPointer--;
}

// pushes int literal to rax
void GenerateExpresion(Generator* gen, NodeExpresion* node){
    switch(node->type){
        case NodeExpresionType_INT_LIT: {
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    mov rax, "));
            StringChainAppend(&gen->outputAsm, &gen->mem, node->intLit->value);
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
            GeneratorPushStack(gen, "rax"); // TODO: do you need to push here?
        } break;
        case NodeExpresionType_BIN_EXP: {
            // generate subexpresion
            // if literal will end up in rax
            // if binary expresion result will end up in rax
            GenerateExpresion(gen, node->binExp->right);

            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    mov rbx, "));
            StringChainAppend(&gen->outputAsm, &gen->mem, node->binExp->left->value);
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
            // TODO: add other operators
            if(StringEqualsCstr(node->binExp->operator->value, "+")){
                StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    add rax, rbx\n"));
            }else{
                printf("[ERROR] Operator \'%.*s\' not implemented\n", node->binExp->operator->value.length, node->binExp->operator->value.str);
                exit(EXIT_FAILURE);
            }

            GeneratorPushStack(gen, "rax");
        } break;
        default: {
            printf("[ERROR] Unhandled expresion case: %s\n", NodeExpresionTypeStr[node->type]);
        } break;
    }
}

void GenerateReturn(Generator* gen, NodeKeywordRet* node){
    // NOTE: the comment only handles int literals for now
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("; return "));
    StringChainAppend(&gen->outputAsm, &gen->mem, node->exp->intLit->value);
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
    
    GenerateExpresion(gen, node->exp);
    GeneratorPopStack(gen, "rax");
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    ret\n"));
}

void GenerateVar(Generator* gen, NodeKeywordVar* node){
    switch(node->type){
        case NodeKeywordVarType_DEF_ONLY: {
            // node->defIdentifier;
            
            // increase stack pointer
            // register identifier in symbol table
            assert(FALSE && "Unimplemented");
        } break;
        case NodeKeywordVarType_DEF_ASSIGNMENT: {
            assert(FALSE && "Unimplemented");
        } break;
        default: {
            printf("[ERROR] Unhandled var case: %s\n", NodeKeywordVarTypeStr[node->type]);
        } break;
    }
}

void GenerateKeyword(Generator* gen, NodeKeyword* node){
    switch(node->type){
        case NodeKeywordType_RET: {
            GenerateReturn(gen, node->ret);
        } break;
        case NodeKeywordType_VAR: {
            GenerateVar(gen, node->var);
        } break;
        default: {
            printf("[ERROR] Unhandled keyword case: %s\n", NodeKeywordTypeStr[node->type]);
        } break;
    }
}

StringChain Generate(Generator* gen, NodeRoot* root){
    // preamble
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("global _start\n"));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("_start:\n"));

    for(int i = 0; i < root->count; i++){
        switch(root->stmts[i].type){
            case NodeStatementType_KEYWORD: {
                GenerateKeyword(gen, root->stmts[i].keyword);
            } break;
            case NodeStatementType_EXPRESION: {
                GenerateExpresion(gen, root->stmts[i].exp);
            } break;
            default: {
                printf("[ERROR] Unhandled statement case: %s\n", NodeStatementTypeStr[root->stmts[i].type]);
            } break;
        }
    }

    return gen->outputAsm;
}

int main(int argc, char** argv){
    if(argc < 2){
        printf("[ERROR] File not found: %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }
    String sourceRaw = EntireFileRead(argv[1]);
    Tokenizer tokenizer = {.source = sourceRaw, .filename = StringFromCstr(argv[1])};
    TokenArray tokens = Tokenize(&tokenizer);

    TokensPrint(&tokens);

    #if 0
    Parser parser = {.tokens = tokens}; // uses memory arena
    NodeRoot ast = Parse(&parser);

    Generator gen = {}; // uses memory arena
    StringChain outputString = Generate(&gen, &ast);

    bool success = EntireFileWrite("output.asm", outputString);
    if(!success){
        printf("[ERROR] Failed to write to file.\n");
        exit(EXIT_FAILURE);
    }
    #endif

    // TODO: do proper CreateProcess() calls here, but this will do for now
    // system("nasm -fwin64 output.asm");
    // system("ld -o output.exe output.obj");

    // NOTE: for using kernel functions build like this
    // system("nasm -fwin32 output.asm");
    // system("C:\\MinGW\\bin\\gcc.exe -m32 -o output.exe output.obj -lkernel32");

    // arena_free(&parser.mem);
    // arena_free(&gen.mem);
    exit(EXIT_SUCCESS);
}