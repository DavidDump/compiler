#include "lexer.h"
#include "common.h"

#include <stdio.h> // printf()
#include <string.h> // strlen()

// Tokenizer TokenizerInit(String source, String filename, TypeMapping* typeMappings, int typeMappingsSize, OperatorInfo* opInfo, int opInfoSize){
//     Tokenizer tokenizer = {
//         .filename = filename,
//         .source = source,
//         .typeMappings = typeMappings,
//         .typeMappingsSize = typeMappingsSize,
//         .opInfo = opInfo,
//         .opInfoSize = opInfoSize,
//     };
//     return tokenizer;
// }

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

TokenArray Tokenize(String source, String filenameCstring) {
    TokenArray result = {0};
    u64 lineNum = 1;
    u64 collumNum = 1;
    
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
            do c = source.str[++i]; while(isLetter(c) || isNumber(c)); // TODO: add special characters that can be in the middle of a identifier
            if(i >= source.length) break;
            u64 len = i - startIndex;
            
            // NOTE: when the loop ends i will point to the char after the keyword/identifier, so back up by one so when the for loop increments it it will not skip a char 
            i--;

            String value = {.str = &source.str[startIndex], .length = len};
            TokenType type = TokenType_NONE;
            // compare keywords and types
            if(StringEqualsCstr(value, "return"))     type = TokenType_RETURN;
            else if(StringEqualsCstr(value, "if"))    type = TokenType_IF;
            else if(StringEqualsCstr(value, "else"))  type = TokenType_ELSE;
            else if(StringEqualsCstr(value, "loop"))  type = TokenType_LOOP;
            else if(StringEqualsCstr(value, "true"))  type = TokenType_BOOL_LITERAL;
            else if(StringEqualsCstr(value, "false")) type = TokenType_BOOL_LITERAL;
            else if(isType(value))                    type = TokenType_TYPE;
            else type = TokenType_IDENTIFIER;

            TokenArrayAddToken(&result, value, type, filenameCstring, lineNum, collumNum);

            collumNum += len - 1;
        } else if(isNumber(c)) {
            // numbers
            u64 startIndex = i;
            do c = source.str[++i]; while(isNumber(c));
            if(i >= source.length) break;
            u64 len = i - startIndex;

            // NOTE: when the loop ends i will point to the char after the number, so back up by one so when the for loop increments it, it will not skip a char 
            i--;

            String str = {.str = &source.str[startIndex], .length = len};
            TokenArrayAddToken(&result, str, TokenType_INT_LITERAL, filenameCstring, lineNum, collumNum);

            collumNum += len - 1;
        } else if(c == '<') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            if(next == '=') {
                len = 2;
                i++;
                collumNum++;
            }
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == '>') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            if(next == '=') {
                len = 2;
                i++;
                collumNum++;
            }
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == '=') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            TokenType type = TokenType_ASSIGNMENT;
            if(next == '=') {
                len = 2;
                type = TokenType_OPERATOR;
                i++;
                collumNum++;
            }
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, type, filenameCstring, lineNum, collumNum);
        } else if(c == '!') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            if(next == '=') {
                len = 2;
                i++;
                collumNum++;
            }
            assert(len == 2 && "! unary operator currently unsupported");
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == '+') {
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == '-') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            TokenType type = TokenType_OPERATOR;
            if(next == '>') {
                len = 2;
                type = TokenType_RARROW;
                i++;
                collumNum++;
            }
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, type, filenameCstring, lineNum, collumNum);
        } else if(c == '*') {
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == '/') {
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_OPERATOR, filenameCstring, lineNum, collumNum);
        } else if(c == ';') {
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_SEMICOLON, filenameCstring, lineNum, collumNum);
        } else if(c == ':') {
            u8 next = source.str[i + 1];
            u64 len = 1;
            TokenType type = TokenType_COLON;
            if(next == ':') {
                // :: operator
                len = 2;
                i++;
                collumNum++;
                type = TokenType_DOUBLECOLON;
            } else if(next == '=') {
                // := operator
                len = 2;
                i++;
                collumNum++;
                type = TokenType_INITIALIZER;
            }
            String str = {.str = &source.str[i], .length = len};
            TokenArrayAddToken(&result, str, type, filenameCstring, lineNum, collumNum);
        } else if(c == '(') {
            // left paren
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_LPAREN, filenameCstring, lineNum, collumNum);
        } else if(c == ')') {
            // right paren
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_RPAREN, filenameCstring, lineNum, collumNum);
        } else if(c == '{') {
            // open scope
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_LSCOPE, filenameCstring, lineNum, collumNum);
        } else if(c == '}') {
            // close scope
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_RSCOPE, filenameCstring, lineNum, collumNum);
        } else if(c == '[') {
            // left bracket
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_LBRACKET, filenameCstring, lineNum, collumNum);
        } else if(c == ']') {
            // right bracket
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_RBRACKET, filenameCstring, lineNum, collumNum);
        } else if(c == ',') {
            // comma
            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_COMMA, filenameCstring, lineNum, collumNum);
        } else if(c == '.') {
            // dot, double dot, tripple dot
            u8 next = &source.str[i + 1];
            u64 len = 1;
            TokenType type = TokenType_DOT;
            if(next == '.'){
                next = next = &source.str[i + 2];
                if(next == '.') {
                    // '...' operator
                    len = 3;
                    type = TokenType_TRIPLEDOT;
                    i += 2;
                    collumNum += 2;
                } else {
                    // '..' operator
                    len = 2;
                    type = TokenType_DOUBLEDOT;
                    i++;
                    collumNum++;
                }
            }

            String str = {.str = &source.str[i], .length = 1};
            TokenArrayAddToken(&result, str, TokenType_DOT, filenameCstring, lineNum, collumNum);
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

            String str = {.str = start, .length = len};
            TokenArrayAddToken(&result, str, TokenType_STRING_LIT, filenameCstring, lineNum, collumNum);

            collumNum += len - 1;
            i += len - 1;
        } else if(c == '\n') {
            lineNum++;
            collumNum = 0;
        }
        
        #ifdef COMP_DEBUG
        else if(isWhitespace(c)) {
            // NOTE: space is ignored but this case is needed here for debug print
            continue;
        } else {
            printf("[ERROR] Unhandled char by the tokenizer: \'%c\' at %.*s:%i:%i\n", *c, filenameCstring.length, filenameCstring.str, lineNum, collumNum);
        }
        #endif // COMP_DEBUG
    }
    return result;
}

#ifdef COMP_DEBUG
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
#endif // COMP_DEBUG
