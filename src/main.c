#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <stdlib.h> // calloc(), realloc(), free(), system()
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()

#define ARENA_IMPLEMENTATION
#include "arena.h"

typedef int bool;
#define TRUE 1
#define FALSE 0

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
            current = current->next; // NOTE: sus compiler warning
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
    TokenType_INT_LITERAL,
    TokenType_SEMICOLON,
    TokenType_OPERATOR,
    TokenType_COUNT,
} TokenType;

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

bool isSpecial(char c){
    return (c == ';');
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
        if(isLetter(c)){
            char* start = (char*)&tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && !isSpecial(*end)) end++;
            int len = end - start;

            String value = StringFromArray(start, len);
            TokenType type = TokenType_NONE;
            // compare keywords
            if(StringEqualsCstr(value, "return")) type = TokenType_RETURN;

            TokenArrayAddToken(&tokens, value, type, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isNumber(c)){
            char* start = (char*)&tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && !isSpecial(*end) && isNumber(*end)) end++;
            int len = end - start;

            TokenArrayAddToken(&tokens, StringFromArray(start, len), TokenType_INT_LITERAL, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(c == ';'){
            TokenArrayAddToken(&tokens, StringFromCstr(";"), TokenType_SEMICOLON, tokenizer->filename, lineNum, collumNum);
        }else if(c == '\n'){
            lineNum++;
            collumNum = 0;
        }else{
            printf("[ERROR] unhandled char by the tokenizer: \'%c\' at %.*s:%i:%i\n", c, tokenizer->filename.length, tokenizer->filename.str, lineNum, collumNum);
        }
    }
    return tokens;
}

void TokensPrint(TokenArray* tokens){
    for(int i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        printf("%.*s:%i:%i \"%.*s\"\n", t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum, t.value.length, t.value.str);
    }
}

typedef struct NodeIntLit{
    Token* value;
} NodeIntLit;

// TODO: move into separate file so forward declarations are a bit nicer
typedef struct NodeExpresion NodeExpresion;

typedef struct NodeBinExpresion{
    Token* left;
    NodeExpresion* right;
    Token* operator;
} NodeBinExpresion;

typedef enum NodeExpresionType{
    NodeExpresionType_NONE,
    NodeExpresionType_INT_LIT,
    NodeExpresionType_BIN_EXP,
    NodeExpresionType_COUNT,
} NodeExpresionType;

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

typedef enum NodeKeywordType{
    NodeKeywordType_NONE,
    NodeKeywordType_RET,
    NodeKeywordType_COUNT,
} NodeKeywordType;

typedef struct NodeKeyword{
    NodeKeywordType type;
    union{
        NodeKeywordRet* ret;
    };
} NodeKeyword;

typedef enum NodeStatementType{
    NodeStatementType_NONE,
    NodeStatementType_KEYWORD,
    NodeStatementType_EXPRESION,
    NodeStatementType_COUNT,
} NodeStatementType;

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
        // printf("[ERROR] %s:%i:%i Expresion malformed.\n", left->loc.filename.str, left->loc.line, left->loc.collum);
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
        // printf("[ERROR] %s:%i:%i Statement needs to end with a ;.\n", t->loc.filename.str, t->loc.line, t->loc.collum);
        ERROR(t->loc, "Statement needs to end with a ;.");
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
        // printf("[ERROR] %s:%i:%i Return keyword needs a value to return.\n", t->loc.filename.str, t->loc.line, t->loc.collum);
        ERROR(t->loc, "Return keyword needs a value to return.");
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
        default: {
            printf("[ERROR] Unknown statement type: %i\n", type);
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
            default: {
                printf("[ERROR] unhandled token type by the parser: \'%i\' at %.*s:%i:%i\n", t->type, t->loc.filename.length, t->loc.filename.str, t->loc.line, t->loc.collum);
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

StringChain Generate(Generator* gen, NodeRoot* root){
    // preamble
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("global _start\n"));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("_start:\n"));

    for(int i = 0; i < root->count; i++){
        switch(root->stmts[i].type){
            case NodeStatementType_KEYWORD: {
                NodeKeyword* node = root->stmts[i].keyword;
                switch(node->type){
                    case NodeKeywordType_RET: {
                        // generate code for expresions, move to own function later
                        NodeExpresion* expNode = node->ret->exp;
                        switch(expNode->type){
                            case NodeExpresionType_NONE: break;
                            case NodeExpresionType_COUNT: break;
                            case NodeExpresionType_INT_LIT: {
                                StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    mov rax, "));
                                StringChainAppend(&gen->outputAsm, &gen->mem, expNode->intLit->value);
                                StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("\n"));
                                GeneratorPushStack(gen, "rax");
                            } break;
                            case NodeExpresionType_BIN_EXP: {
                                // TODO: implement
                                assert(FALSE && "Unimplemented");
                            } break;
                        }

                        // generate code for return keyword
                        GeneratorPopStack(gen, "rax");
                        StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr("    ret\n"));
                    } break;
                    default: {
                        printf("[ERROR] Unknown keyword type: %i\n", node->type);
                    } break;
                }
            } break;
            case NodeStatementType_EXPRESION: {
                // NodeExpresion* node = root->stmts[i].exp;
                // TODO: implement
                assert(FALSE && "Unimplemented");
            } break;
            default: {
                printf("[ERROR] Unknown statement type: %i\n", root->stmts[i].type);
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

    // TokensPrint(&tokens);

    Parser parser = {.tokens = tokens}; // uses memory arena
    NodeRoot ast = Parse(&parser);

    Generator gen = {}; // uses memory arena
    StringChain outputString = Generate(&gen, &ast);

    bool success = EntireFileWrite("output.asm", outputString);
    if(!success){
        printf("[ERROR] Failed to write to file.\n");
        exit(EXIT_FAILURE);
    }

    // TODO: do proper CreateProcess() calls here, but this will do for now
    system("nasm -fwin64 output.asm");
    system("ld output.obj");

    arena_free(&parser.mem);
    arena_free(&gen.mem);
    exit(EXIT_SUCCESS);
}