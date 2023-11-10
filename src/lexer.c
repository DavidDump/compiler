#include "lexer.h"
#include "common.h"

#include <stdio.h> // printf()
#include <string.h> // strlen()

Tokenizer TokenizerInit(String source, String filename, TypeInformation* typeInfo, OperatorInformation* opsInfo){
    Tokenizer tokenizer = {
        .filename = filename,
        .source = source,
        .typeInfo = typeInfo,
        .opsInfo = opsInfo,
    };
    return tokenizer;
}

char TokenizerPeek(Tokenizer* tokenizer, int offset){
    if(tokenizer->index + offset > tokenizer->source.length || tokenizer->index + offset < 0) return 0;
    return tokenizer->source.str[tokenizer->index + offset];
}

char* TokenizerConsume(Tokenizer* tokenizer){
    if(tokenizer->index + 1 > tokenizer->source.length) return 0;
    return (char*)&tokenizer->source.str[tokenizer->index++];
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

bool isOperator(Tokenizer* tokenizer, char* c, int* len){
    for(int i = 0; i < tokenizer->opsInfo->size; i++){
        if(StringContains(tokenizer->opsInfo->ops[i].symbol, c)){
            *len = tokenizer->opsInfo->ops[i].symbol.length;
            return TRUE;
        }
    }
    return FALSE;
}

void TokenArrayAddToken(TokenArray* arr, String value, TokenType type, String filename, int line, int collum){
    if(arr->size >= arr->capacity){
        size_t newCap = arr->capacity * 2;
        if(newCap == 0) newCap = 1;
        arr->tokens = arena_realloc(&arr->mem, arr->tokens, arr->size * sizeof(arr->tokens[0]), newCap * sizeof(arr->tokens[0]));
        arr->capacity = newCap;
    }

    Token tok = {0};
    tok.value = value;
    tok.type = type;
    tok.loc.filename = filename;
    tok.loc.line = line;
    tok.loc.collum = collum;
    arr->tokens[arr->size++] = tok;
}

bool TokenIsType(Tokenizer* tokenizer, String value){
    for(int i = 0; i < tokenizer->typeInfo->size; i++){
        if(StringEquals(tokenizer->typeInfo->types[i].symbol, value)){
            return TRUE;
        }
    }
    return FALSE;
}

TokenArray Tokenize(Tokenizer* tokenizer){
    TokenArray tokens = {0};
    int lineNum = 1;
    int collumNum = 1;
    int operatorLen = 0; // used for determining the lenght on multichar operators
    for(char* c = TokenizerConsume(tokenizer); c != 0; c = TokenizerConsume(tokenizer), collumNum++){
        // special case: line comment
        if(*c == '/'){
            char next = TokenizerPeek(tokenizer, 0);
            if(next == '/'){
                while((c = TokenizerConsume(tokenizer)) != 0 && *c != '\n');
                if(c == 0) break;
            }
        }

        if(isLetter(*c)){
            // keywords and identifiers
            char* start = c;
            char* end = start;
            while(*end && !isWhitespace(*end) && (isLetter(*end) || isNumber(*end))) end++;
            int len = end - start;

            String value = {.str = start, .length = len};
            TokenType type = TokenType_NONE;
            // compare keywords and types
            if(StringEqualsCstr(value, "return"))    type = TokenType_RETURN;
            else if(StringEqualsCstr(value, "if"))   type = TokenType_IF;
            else if(StringEqualsCstr(value, "else")) type = TokenType_ELSE;
            else if(TokenIsType(tokenizer, value))   type = TokenType_TYPE;
            else type = TokenType_IDENTIFIER;

            TokenArrayAddToken(&tokens, value, type, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isNumber(*c)){
            // numbers
            char* start = c;
            char* end = start;
            while(*end && !isWhitespace(*end) && isNumber(*end)) end++;
            int len = end - start;

            String str = {.str = start, .length = len};
            TokenArrayAddToken(&tokens, str, TokenType_INT_LITERAL, tokenizer->filename, lineNum, collumNum);

            collumNum += len - 1;
            tokenizer->index += len - 1;
        }else if(isOperator(tokenizer, c, &operatorLen)){
            // operators

            // special case -> operator
            if(*c == '-'){
                char next = TokenizerPeek(tokenizer, 0);
                if(next == '>'){
                    String str = {.str = c, .length = 2};
                    TokenArrayAddToken(&tokens, str, TokenType_RARROW, tokenizer->filename, lineNum, collumNum);
                    
                    TokenizerConsume(tokenizer);
                    collumNum++;
                    continue;
                }
            }

            for(int i = 0; i < operatorLen - 1; i++) TokenizerConsume(tokenizer);
            String str = {.str = c, .length = operatorLen};
            TokenArrayAddToken(&tokens, str, TokenType_OPERATOR, tokenizer->filename, lineNum, collumNum);
        }else if(*c == ';'){
            // semicolon
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_SEMICOLON, tokenizer->filename, lineNum, collumNum);
        }else if(*c == '='){
            // equals
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_ASSIGNMENT, tokenizer->filename, lineNum, collumNum);
        }else if(*c == ':'){
            // : or :: operator
            char next = TokenizerPeek(tokenizer, 0);
            if(next == ':'){
                // :: operator
                String str = {.str = c, .length = 2};
                TokenArrayAddToken(&tokens, str, TokenType_DOUBLECOLON, tokenizer->filename, lineNum, collumNum);
                
                TokenizerConsume(tokenizer);
                collumNum++;
            }else if(next == '='){
                // := operator
                String str = {.str = c, .length = 2};
                TokenArrayAddToken(&tokens, str, TokenType_INITIALIZER, tokenizer->filename, lineNum, collumNum);
                
                TokenizerConsume(tokenizer);
                collumNum++;
            }else{
                // : operator
                String str = {.str = c, .length = 1};
                TokenArrayAddToken(&tokens, str, TokenType_COLON, tokenizer->filename, lineNum, collumNum);
            }
        }else if(*c == '('){
            // left paren
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_LPAREN, tokenizer->filename, lineNum, collumNum);
        }else if(*c == ')'){
            // right paren
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_RPAREN, tokenizer->filename, lineNum, collumNum);
        }else if(*c == '{'){
            // open scope
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_LSCOPE, tokenizer->filename, lineNum, collumNum);
        }else if(*c == '}'){
            // close scope
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_RSCOPE, tokenizer->filename, lineNum, collumNum);
        }else if(*c == '['){
            // left bracket
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_LBRACKET, tokenizer->filename, lineNum, collumNum);
        }else if(*c == ']'){
            // right bracket
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_RBRACKET, tokenizer->filename, lineNum, collumNum);
        }else if(*c == ','){
            // comma
            String str = {.str = c, .length = 1};
            TokenArrayAddToken(&tokens, str, TokenType_COMMA, tokenizer->filename, lineNum, collumNum);
        }else if(*c == '\n'){
            lineNum++;
            collumNum = 0;
        }
        
        #ifdef COMP_DEBUG
        else if(*c == ' '){
            // NOTE: space is ignored but this case is needed here for debug print
            continue;
        }else if(*c == '\r'){
            // NOTE: carrige return is ignored but this case is needed here for debug print
            continue;
        }else{
            printf("[ERROR] Unhandled char by the tokenizer: \'%c\' at %.*s:%i:%i\n", *c, tokenizer->filename.length, tokenizer->filename.str, lineNum, collumNum);
        }
        #endif // COMP_DEBUG
    }
    return tokens;
}

void TokenPrint(Token t){
    printf("%.*s:%i:%i\t { %-12s %-8.*s }\n", t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum, TokenTypeStr[t.type], t.value.length, t.value.str);
}

void TokenPrintAsTable(Token t, int locWidth, int symbolNameWidth, int symbolWidth){
    int printed = 0;
    int remaining = 0;
    
    printed = printf("%.*s:%i:%i", t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum);
    remaining = locWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printf("{  ");
    printed = printf("%s", TokenTypeStr[t.type]);
    remaining = symbolNameWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printed = printf("%.*s", t.value.length, t.value.str);
    remaining = symbolWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printf(" }\n");
}

void TokensPrint(TokenArray* tokens){
    // collect collum width information
    int locWidth = 0;
    int symbolNameWidth = 0;
    int symbolWidth = 0;
    for(int i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        #define BUFFER_SIZE 1024
        char buffer[BUFFER_SIZE] = {0};

        int len = snprintf(buffer, BUFFER_SIZE, "%.*s:%i:%i", t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum);
        if(len > locWidth) locWidth = len;
        memset(buffer, 0, len);

        len = strlen(TokenTypeStr[t.type]);
        if(len > symbolNameWidth) symbolNameWidth = len;

        len = snprintf(buffer, BUFFER_SIZE, "%.*s", t.value.length, t.value.str);
        if(len > symbolWidth) symbolWidth = len;
    }

    // print the table
    for(int i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        TokenPrintAsTable(t, locWidth, symbolNameWidth, symbolWidth);
    }
}
