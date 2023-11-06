#include <stdlib.h>
#include <stdio.h>

#include "parser.h"

Token* ParserPeek(Parser* parser, int offset){
    if(parser->index + offset > parser->tokens.size || parser->index + offset < 0) return NULL;
    return &parser->tokens.tokens[parser->index + offset];
}

Token* ParserConsume(Parser* parser){
    if(parser->index + 1 > parser->tokens.size) return NULL;
    return &parser->tokens.tokens[parser->index++];
}

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
