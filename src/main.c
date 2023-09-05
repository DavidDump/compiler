#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef int bool;
#define TRUE 1
#define FALSE 0

typedef struct{
    char* str;
    int length;
    int capacity;
} String;

// TODO: memory allocation in arena that gets passed in
String StringFromCstr(const char* cstr){
    String result = {};
    int len = strlen(cstr);
    result.str = calloc(len, sizeof(char));
    memcpy(result.str, cstr, len);
    result.length = len;
    result.capacity = len;
    return result;
}

String StringFromArray(const char* arr, int size){
    String result = {};
    result.str = calloc(size, sizeof(char));
    memcpy(result.str, arr, size);
    result.length = size;
    result.capacity = size;
    return result;
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
    TokenType_SEMICOLON, // semicolon
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
    if(tokenizer->index + offset > tokenizer->source.length) return 0;
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

void TokenArrayAddToken(TokenArray* arr, String value, String filename, int line, int collum){
    if(arr->size >= arr->capacity){
        size_t newCap = arr->capacity * 2;
        if(newCap == 0) newCap = 1;
        arr->tokens = realloc(arr->tokens, newCap * sizeof(Token));
        arr->capacity = newCap;
    }

    Token tok = {};
    tok.value = value;
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
            char* start = &tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && !isSpecial(*end)) end++;
            int len = end - start;

            TokenArrayAddToken(&tokens, StringFromArray(start, len), tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isNumber(c)){
            char* start = &tokenizer->source.str[tokenizer->index - 1];
            char* end = start;
            while(*end && !isWhitespace(*end) && !isSpecial(*end) && isNumber(*end)) end++;
            int len = end - start;

            TokenArrayAddToken(&tokens, StringFromArray(start, len), tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(c == ';'){
            TokenArrayAddToken(&tokens, StringFromCstr(";"), tokenizer->filename, lineNum, collumNum);
        }else if(c == '\n'){
            lineNum++;
            collumNum = 0;
        }else{
            printf("[ERROR] unhandled char by the tokenizer: \'%c\'\n", c);
        }
    }
    return tokens;
}

int main(int argc, char** argv){
    if(argc < 2){
        printf("[ERROR] File not found: %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }
    String sourceRaw = EntireFileRead(argv[1]);
    Tokenizer tokenizer = {.source = sourceRaw, .filename = StringFromCstr(argv[1])};
    TokenArray tokens = Tokenize(&tokenizer);

    for(int i = 0; i < tokens.size; i++){
        Token t = tokens.tokens[i];
        printf("%s:%i:%i \"%s\"\n", t.loc.filename.str, t.loc.line, t.loc.collum, t.value.str);
    }

    exit(EXIT_SUCCESS);
}