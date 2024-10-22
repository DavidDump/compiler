#include "lexer.h"
#include "common.h"

#include <stdio.h> // printf()
#include <string.h> // strlen()

bool isLetter(u8 c) {
    return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'));
}

bool isNumber(u8 c) {
    return ('0' <= c && c <= '9');
}

bool isWhitespace(u8 c) {
    return (c == ' ' || c == '\n' || c == '\t' || c == '\v' || c == '\r');
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

bool isType(String value) {
    return (
        StringEqualsCstr(value, "u8")     ||
        StringEqualsCstr(value, "u16")    ||
        StringEqualsCstr(value, "u32")    ||
        StringEqualsCstr(value, "u64")    ||
        StringEqualsCstr(value, "s8")     ||
        StringEqualsCstr(value, "s16")    ||
        StringEqualsCstr(value, "s32")    ||
        StringEqualsCstr(value, "s64")    ||
        StringEqualsCstr(value, "f32")    ||
        StringEqualsCstr(value, "f64")    ||
        StringEqualsCstr(value, "string") ||
        StringEqualsCstr(value, "bool")   ||
        StringEqualsCstr(value, "void")   ||
        StringEqualsCstr(value, "int")    ||
        StringEqualsCstr(value, "float")
    );
}

TokenArray Tokenize(String source, String filename) {
    TokenArray result = {0};
    u64 lineNum = 1;
    u64 collumNum = 1;
    String value = {0};
    TokenType type = 0;

    for(u64 i = 0; i < source.length; ++i) {
        u8 c = source.str[i];
        
        // special case: line comment
        if(c == '/') {
            u8 next = source.str[i + 1];
            if(next == '/'){
                do c = source.str[++i]; while(c != '\n' && i < source.length);
                if(i >= source.length) break; // in case the file is only one line
            }
        }

        if(isLetter(c)) {
            // keywords and identifiers
            u64 startIndex = i;
            u64 endIndex = i;
            do c = source.str[++endIndex]; while(isLetter(c) || isNumber(c)); // TODO: add special characters that can be in the middle of a identifier
            if(endIndex >= source.length) break;
            u64 len = endIndex - startIndex;
            
            // compare keywords and types
            if(StringEqualsCstr(value, "return"))     type = TokenType_RETURN;
            else if(StringEqualsCstr(value, "if"))    type = TokenType_IF;
            else if(StringEqualsCstr(value, "else"))  type = TokenType_ELSE;
            else if(StringEqualsCstr(value, "loop"))  type = TokenType_LOOP;
            else if(StringEqualsCstr(value, "true"))  type = TokenType_BOOL_LITERAL;
            else if(StringEqualsCstr(value, "false")) type = TokenType_BOOL_LITERAL;
            else if(isType(value))                    type = TokenType_TYPE;
            else type = TokenType_IDENTIFIER;

            value.str = &source.str[startIndex];
            value.length = len;
        } else if(isNumber(c)) {
            // numbers
            u64 startIndex = i;
            u64 endIndex = i;
            do c = source.str[++endIndex]; while(isNumber(c));
            if(endIndex >= source.length) break;
            u64 len = endIndex - startIndex;

            value.str = &source.str[startIndex];
            value.length = len;
            type = TokenType_INT_LITERAL;
        } else if(c == '<') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_LESS;
            if(next == '=') {
                len = 2;
                type = TokenType_LESS_EQ;
            }
            
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '>') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_GREATER;
            if(next == '=') {
                len = 2;
                type = TokenType_GREATER_EQ;
            }
            
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '=') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_ASSIGNMENT;
            if(next == '=') {
                len = 2;
                type = TokenType_COMPARISON;
            }
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '!') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_NONE;
            if(next == '=') {
                len = 2;
                type = TokenType_NOT_EQUALS;
            }
            assert(len == 2 && "! unary operator currently unsupported");
            
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '+') {
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_ADD;
        } else if(c == '-') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_SUB;
            if(next == '>') {
                len = 2;
                type = TokenType_RARROW;
            }
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '*') {
            value.str = &source.str[i];
            value.length = 1;
            type  = TokenType_MUL;
        } else if(c == '/') {
            value.str = &source.str[i];
            value.length = 1;
            type  = TokenType_DIV;
        } else if(c == ';') {
            value.str = &source.str[i];
            value.length = 1;
            type  = TokenType_SEMICOLON;
        } else if(c == ':') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_COLON;
            if(next == ':') {
                // :: operator
                len = 2;
                type = TokenType_DOUBLECOLON;
            } else if(next == '=') {
                // := operator
                len = 2;
                type = TokenType_INITIALIZER;
            }
            value.str = &source.str[i];
            value.length = len;
        } else if(c == '(') {
            // left paren
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_LPAREN;
        } else if(c == ')') {
            // right paren
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_RPAREN;
        } else if(c == '{') {
            // open scope
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_LSCOPE;
        } else if(c == '}') {
            // close scope
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_RSCOPE;
        } else if(c == '[') {
            // left bracket
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_LBRACKET;
        } else if(c == ']') {
            // right bracket
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_RBRACKET;
        } else if(c == ',') {
            // comma
            value.str = &source.str[i];
            value.length = 1;
            type = TokenType_COMMA;
        } else if(c == '.') {
            // dot, double dot, tripple dot
            u8 next = source.str[i + 1];
            u64 len = 1;
            type = TokenType_DOT;
            if(next == '.'){
                next = source.str[i + 2];
                if(next == '.') {
                    // '...' operator
                    len = 3;
                    type = TokenType_TRIPLEDOT;
                } else {
                    // '..' operator
                    len = 2;
                    type = TokenType_DOUBLEDOT;
                }
            }

            value.str = &source.str[i];
            value.length = len;
        } else if(c == '\"') {
            // string literal
            // TODO: propper string literal parsing with escape characters
            u8* start = &source.str[i];
            u8* end = start + 1;
            while(*end && *end != '\"'){
                if(*end == '\n') lineNum++;
                end++;
            }
            end++; // the second "
            u64 len = end - start;

            value.str = start;
            value.length = len;
            type = TokenType_STRING_LIT;
        } else if(c == '\n') {
            lineNum++;
            collumNum = 0;
            continue;
        }

        #ifdef COMP_DEBUG
        else if(isWhitespace(c)) {
            // NOTE: space is ignored but this case is needed here for debug print
            continue;
        } else {
            printf("[ERROR] Unhandled char by the tokenizer: \'%c\' at "STR_FMT":%lli:%lli\n", c, STR_PRINT(filename), lineNum, collumNum);
        }
        #endif // COMP_DEBUG
        
        assert(value.length != 0);
        assert(type != 0);
        TokenArrayAddToken(&result, value, type, filename, lineNum, collumNum);
        
        collumNum += value.length - 1;
        i += value.length - 1;
        
        value.str = 0;
        value.length = 0;
        type = 0;
    }
    return result;
}

#ifdef COMP_DEBUG
void TokenPrint(Token t){
    printf(STR_FMT":%i:%i\t { %-12s %-8.*s }\n", STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum, TokenTypeStr[t.type], (int)t.value.length, t.value.str);
}

void TokenPrintAsTable(Token t, int locWidth, int symbolNameWidth, int symbolWidth){
    int printed = 0;
    int remaining = 0;
    
    printed = printf(STR_FMT":%i:%i", STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum);
    remaining = locWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printf("{  ");
    printed = printf("%s", TokenTypeStr[t.type]);
    remaining = symbolNameWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printed = printf(STR_FMT, STR_PRINT(t.value));
    remaining = symbolWidth - printed;
    for(int i = 0; i < remaining + 1; i++) printf(" ");
    printf(" }\n");
}

void TokensPrint(TokenArray* tokens){
    // collect collum width information
    int locWidth = 0;
    int symbolNameWidth = 0;
    int symbolWidth = 0;
    for(u64 i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        #define BUFFER_SIZE 1024
        char buffer[BUFFER_SIZE] = {0};

        int len = snprintf(buffer, BUFFER_SIZE, STR_FMT":%i:%i", STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum);
        if(len > locWidth) locWidth = len;
        memset(buffer, 0, len);

        len = strlen(TokenTypeStr[t.type]);
        if(len > symbolNameWidth) symbolNameWidth = len;

        len = snprintf(buffer, BUFFER_SIZE, STR_FMT, STR_PRINT(t.value));
        if(len > symbolWidth) symbolWidth = len;
    }

    // print the table
    for(u64 i = 0; i < tokens->size; i++){
        Token t = tokens->tokens[i];
        TokenPrintAsTable(t, locWidth, symbolNameWidth, symbolWidth);
    }
}
#endif // COMP_DEBUG

// TODO: add a mapping table from single char tokens to TokenType's and multi char tokens to TokenType's
