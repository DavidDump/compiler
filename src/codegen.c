#include "codegen.h"

GenContext GenContextInit(TypeInformation typeInfo, OperatorInformation opInfo){
    GenContext ctx = {0};
    ctx.typeInfo = typeInfo;
    ctx.opInfo = opInfo;
    return ctx;
}

void appendIdLoc(IdentifierLocations* idLoc, String key, int value){
    if(idLoc->size >= idLoc->capacity){
        size_t newCap = idLoc->capacity * 2;
        if(newCap == 0) newCap = 1;
        idLoc->items = arena_realloc(&idLoc->mem, idLoc->items, idLoc->capacity * sizeof(idLoc->items[0]), newCap * sizeof(idLoc->items[0]));
        idLoc->capacity = newCap;
    }

    idLoc->items[idLoc->size].key = key;
    idLoc->items[idLoc->size].value = value;
    idLoc->size++;
}

int findIdLoc(IdentifierLocations* idLoc, String key){
    for(int i = 0; i < idLoc->size; i++){
        if(StringEquals(idLoc->items[i].key, key)){
            return idLoc->items[i].value;
        }
    }

    return -1;
}

// helper for counting the digits of and int
int digitsCount(int value){
    int l = !value;
    while(value){
        l++;
        value/=10;
    }
    return l;
}

// %s in the format string means String type instead of regular cstring
void genChainPrintf(StringChain* result, Arena* mem, const char* format, ...){
    va_list args;
    va_start(args, format);

    String workingStr = {.str = format, .length = 0};
    bool wasPercent = FALSE;
    while(*format != '\0'){
        if(wasPercent == FALSE && *format == '%'){
            wasPercent = TRUE;
        }else if(wasPercent == TRUE && *format == 's'){
            wasPercent = FALSE;
            String arg = va_arg(args, String);
            
            StringChainAppend(result, mem, workingStr);
            StringChainAppend(result, mem, arg);
            
            workingStr.str = format + 1;
            workingStr.length = 0;
        }else if(wasPercent == TRUE && *format == 'i'){
            wasPercent = FALSE;
            int arg = va_arg(args, int);

            StringChainAppend(result, mem, workingStr);

            int intLen = digitsCount(arg) + 1; // NOTE: snprintf only works if the buffer has enough space for a \0 terminator
            char* buffer = arena_alloc(mem, intLen * sizeof(char));
            snprintf(buffer, intLen, "%i", arg);
            StringChainAppend(result, mem, (String){.str = buffer, .length = intLen});

            workingStr.str = format + 1;
            workingStr.length = 0;
        }else{
            wasPercent = FALSE;
            workingStr.length++;
        }
        format++;
    }
    // add the string between the last `%s` and `\0`
    if(workingStr.length > 0) StringChainAppend(result, mem, workingStr);

    va_end(args);
}

void gen_win_x86_64_nasm_push(GenContext* ctx, StringChain* result, const char* reg){
    ctx->stack++;
    genChainPrintf(result, &ctx->mem, "    push %s\n", (String){.str = reg, .length = strlen(reg)});
}

void gen_win_x86_64_nasm_pop(GenContext* ctx, StringChain* result, const char* reg){
    if(ctx->stack - 1 < 0){
        printf("[ERROR] Stack underflow\n");
        exit(EXIT_FAILURE);
    }
    ctx->stack--;
    genChainPrintf(result, &ctx->mem, "    pop %s\n", (String){.str = reg, .length = strlen(reg)});
}

void genSaveStack(GenContext* ctx){
    if(ctx->savedStackPointer + 1 > SAVED_STACK_SIZE){
        printf("[ERROR] Call stack overflow\n");
        exit(EXIT_FAILURE);
    }
    ctx->savedStack[ctx->savedStackPointer++] = ctx->stack;
}

void genRestoreStack(GenContext* ctx){
    if(ctx->savedStackPointer - 1 < 0){
        printf("[ERROR] Call stack underflow\n");
        exit(EXIT_FAILURE);
    }
    ctx->stack = ctx->savedStack[--ctx->savedStackPointer];
}

#if 0
StringChain gen_win_x86_64_nasm_primary(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    
    if(expr->type == ASTNodeType_INT_LIT){
        genChainPrintf(&result, &ctx->mem, "    mov rax, %s\n", expr->node.INT_LIT.value);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        // call function and put the ret value in rax, should be defount behaviour
    }else{
        printf("error in primary\n");
        exit(EXIT_FAILURE);
    }

    return result;
}
#endif

StringChain gen_win_x86_64_nasm_expresion(GenContext* ctx, ASTNode* expr);

StringChain gen_win_x86_64_nasm_func_call(GenContext* ctx, String id, Args args){
    StringChain result = {0};

    for(int i = 0; i < args.size; i++){
        ASTNode* exprNode = args.args[i];

        StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
        StringChainAppendChain(&result, &ctx->mem, expr);
        if(i == 0){
            // first arg
            genChainPrintf(&result, &ctx->mem, "    mov rcx, rax\n");
        }else if(i == 1){
            // second arg
            genChainPrintf(&result, &ctx->mem, "    mov rdx, rax\n");
        }else if(i == 2){
            // third arg
            genChainPrintf(&result, &ctx->mem, "    mov r8, rax\n");
        }else if(i == 3){
            // fourth arg
            genChainPrintf(&result, &ctx->mem, "    mov r9, rax\n");
        }else{
            // fifth+ arg
            gen_win_x86_64_nasm_push(ctx, &result, "rax");
        }
    }

    genChainPrintf(&result, &ctx->mem, "    call %s\n", id);

    return result;
}

StringChain gen_win_x86_64_nasm_expresion(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    // TODO: for now hardcode + operator
    if(expr->type == ASTNodeType_INT_LIT){
        genChainPrintf(&result, &ctx->mem, "    mov rax, %s\n", expr->node.INT_LIT.value);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        String id = expr->node.FUNCTION_CALL.identifier;
        Args args = expr->node.FUNCTION_CALL.args;

        StringChain funcCall = gen_win_x86_64_nasm_func_call(ctx, id, args);
        StringChainAppendChain(&result, &ctx->mem, funcCall);
    }else if(expr->type == ASTNodeType_SYMBOL_RVALUE){
        String id = expr->node.SYMBOL_RVALUE.identifier;
        int loc = findIdLoc(&ctx->idLoc, id);
        if(loc == -1){
            genChainPrintf(&result, &ctx->mem, "    mov rax, [%s]\n", id);
        }else{
            genChainPrintf(&result, &ctx->mem, "    mov rax, [rbp - %i * %i]\n", loc, ctx->intSize);
        }
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "+")){
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, lhs);       // expresion ends up in rax
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, rhs);       // expresion ends up in rax
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    add rax, rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "-")){
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, lhs);
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    sub rax, rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "*")){
        // NOTE: mul rcx means rax = rax * rcx
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, lhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    mul rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "/")){
        // NOTE: http://stackoverflow.com/questions/45506439/ddg#45508617
        // div rcx means rax = rax / rcx remainder is rdx
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, lhs);
        genChainPrintf(&result, &ctx->mem, "    mov rdx, 0\n");
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    div rcx\n");
    }else{
        printf("[ERROR] Unknown operator: %.*s\n", expr->node.EXPRESION.operator.length, expr->node.EXPRESION.operator.str);
        exit(EXIT_FAILURE);
    }

    return result;
}

StringChain generate_win_x86_64_nasm_condition(GenContext* ctx, ASTNode* expr, int label){
    // NOTE: if a else if condition is generated and one of the operands is the same as in the previous condition,
    // it doesnt need to be moved into a register as its already there
    StringChain result = {0};

    StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
    StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

    StringChainAppendChain(&result, &ctx->mem, rhs);
    gen_win_x86_64_nasm_push(ctx, &result, "rax");
    StringChainAppendChain(&result, &ctx->mem, lhs);
    gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
    genChainPrintf(&result, &ctx->mem, "    cmp rax, rcx\n");
    
    if(StringEqualsCstr(expr->node.EXPRESION.operator, "==")){
        genChainPrintf(&result, &ctx->mem, "    je .L%i\n", label);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "!=")){
        genChainPrintf(&result, &ctx->mem, "    jne .L%i\n", label);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "<")){
        genChainPrintf(&result, &ctx->mem, "    jl .L%i\n", label);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, ">")){
        genChainPrintf(&result, &ctx->mem, "    jg .L%i\n", label);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "<=")){
        genChainPrintf(&result, &ctx->mem, "    jle .L%i\n", label);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, ">=")){
        genChainPrintf(&result, &ctx->mem, "    jge .L%i\n", label);
    }

    return result;
}

#define INVALID_IF_LABEL_COUNTER -1
StringChain generate_win_x86_64_nasm_scope(GenContext* ctx, Scope* globalScope, StringChain* dataSection){
    StringChain result = {0};
    StringChain ifConditions = {0};
    StringChain ifBody = {0};
    int ifEndLabel = INVALID_IF_LABEL_COUNTER;
    for(int i = 0; i < globalScope->stmts.size; i++){
        ASTNode* node = globalScope->stmts.statements[i];

        switch(node->type){
            case ASTNodeType_NONE:
            case ASTNodeType_COUNT: {
                printf("[ERROR] ast node none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case ASTNodeType_VAR_DECL_ASSIGN: {
                ASTNode* exprNode = node->node.VAR_DECL_ASSIGN.expresion;
                String id = node->node.VAR_DECL_ASSIGN.identifier;
                ASTNode* type = node->node.VAR_DECL_ASSIGN.type;
                UNUSED(type);
                
                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                // store location in a hashmap with the symbol identifier as a key
                appendIdLoc(&ctx->idLoc, id, ctx->stack);
                // push variable to stack
                gen_win_x86_64_nasm_push(ctx, &result, "rax");
            } break;
            case ASTNodeType_VAR_CONST: {
                String id = node->node.VAR_CONST.identifier;
                String value = node->node.VAR_CONST.value;

                // TODO: dont hardcode dq size but somehow use space more efficiently
                genChainPrintf(dataSection, &ctx->mem, "    %s dq %s\n", id, value);
            } break;
            case ASTNodeType_VAR_DECL: {
                String id = node->node.VAR_DECL.identifier;
                ASTNode* type = node->node.VAR_DECL.type;
                UNUSED(type);

                // store location in a hashmap with the symbol identifier as a key
                appendIdLoc(&ctx->idLoc, id, ctx->stack);
                // increase the stack
                genChainPrintf(&result, &ctx->mem, "    sub rsp, %i\n", ctx->intSize);
                ctx->stack++;
            } break;
            case ASTNodeType_VAR_REASSIGN: {
                String id = node->node.VAR_REASSIGN.identifier;
                ASTNode* exprNode = node->node.VAR_REASSIGN.expresion;

                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                int loc = findIdLoc(&ctx->idLoc, id);
                if(loc == -1){
                    printf("[ERROR] Symbol access not found\n");
                    exit(EXIT_FAILURE);
                }
                int savedStack = ctx->stack;
                genChainPrintf(&result, &ctx->mem, "    lea rsp, [rbp - %i * %i]\n", loc - 1, ctx->intSize);
                gen_win_x86_64_nasm_push(ctx, &result, "rax");
                // TODO: investigate if this has to be `savedStack` or `savedStack - 1`
                genChainPrintf(&result, &ctx->mem, "    lea rsp, [rbp - %i * %i]\n", savedStack, ctx->intSize);
                ctx->stack = savedStack;
                // TODO: reseting the stack to its original position is only neccesary if it didnt alrady get reset by the push
                // ie.: if((savedStack - 1) - (loc - 1) != 1) reset stack pos 
            } break;
            case ASTNodeType_RET: {
                // NOTE: dead code elimination, currently internal stack underflows if there is more than one return in a add
                ASTNode* exprNode = node->node.RET.expresion;
                
                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                genChainPrintf(&result, &ctx->mem, "    mov rsp, rbp\n");
                genRestoreStack(ctx);
                gen_win_x86_64_nasm_pop(ctx, &result, "rbp");
                genChainPrintf(&result, &ctx->mem, "    ret\n");
            } break;
            case ASTNodeType_FUNCTION_DEF: {
                String id = node->node.FUNCTION_DEF.identifier;
                Scope* scope = node->node.FUNCTION_DEF.scope;
                Args args = node->node.FUNCTION_DEF.args;
                ASTNode* type = node->node.FUNCTION_DEF.type;
                UNUSED(type);

                // function header
                genChainPrintf(&result, &ctx->mem, "jmp after_%s\n", id);
                // TODO: temp
                if(StringEqualsCstr(id, "main")){
                    genChainPrintf(&result, &ctx->mem, "_start:\n");
                }else{
                    genChainPrintf(&result, &ctx->mem, "%s:\n", id);
                }
                gen_win_x86_64_nasm_push(ctx, &result, "rbp");
                genChainPrintf(&result, &ctx->mem, "    mov rbp, rsp\n");
                genSaveStack(ctx);
                
                // get args
                for(int i = 0; i < args.size; i++){
                    String argId = args.args[i]->node.VAR_DECL.identifier;
                    ASTNode* argType = args.args[i]->node.VAR_DECL.type;
                    UNUSED(argType);

                    if(i == 0){
                        // first arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "rcx");
                    }else if(i == 1){
                        // second arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "rdx");
                    }else if(i == 2){
                        // third arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "r8");
                    }else if(i == 3){
                        // fourth arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "r9");
                    }else{
                        // fifth+ arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        genChainPrintf(&result, &ctx->mem, "    mov rax, [rbp + %i * %i]\n", i - 3, ctx->intSize);
                        gen_win_x86_64_nasm_push(ctx, &result, "rax");
                        // NOTE: maybe this isnt nessecary and we can just save a negative stack value and store half the args above rbp and the other half below, whould save stack space
                    }
                }

                // generate body
                StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);
                StringChainAppendChain(&result, &ctx->mem, body);

                // function footer
                // NOTE: return is handled by the return keyword,
                // dont know if there is a situation it need to be generated here
                genChainPrintf(&result, &ctx->mem, "after_%s:\n", id);
            } break;
            // TODO: see if some of the StringChainAppendChain calls can be removed adding a string not to the and of the chain but inserting it in the middle
            case ASTNodeType_IF: {
                ASTNode* next = node;
                do{
                    if(next->type != ASTNodeType_IF) i++;
                    if(next->type == ASTNodeType_IF){
                        ASTNode* exprNode = next->node.IF.expresion;
                        Scope* scope = next->node.IF.scope;

                        // conditions
                        int labelCounter = ctx->labelCounter++; // if body label
                        StringChain condition = generate_win_x86_64_nasm_condition(ctx, exprNode, labelCounter);
                        StringChainAppendChain(&ifConditions, &ctx->mem, condition);
                        
                        // body
                        StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);
                        genChainPrintf(&ifBody, &ctx->mem, ".L%i:\n", labelCounter);
                        StringChainAppendChain(&ifBody, &ctx->mem, body);
                    }else if(next->type == ASTNodeType_ELSE){
                        Scope* scope = next->node.ELSE.scope;

                        // else body
                        if(ifEndLabel == INVALID_IF_LABEL_COUNTER) ifEndLabel = ctx->labelCounter++;
                        StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);

                        StringChainAppendChain(&ifConditions, &ctx->mem, body);
                    }else if(next->type == ASTNodeType_ELSE_IF){
                        ASTNode* exprNode = next->node.ELSE_IF.expresion;
                        Scope* scope = next->node.ELSE_IF.scope;

                        // condition
                        int label = ctx->labelCounter++;
                        StringChain condition = generate_win_x86_64_nasm_condition(ctx, exprNode, label);
                        StringChainAppendChain(&ifConditions, &ctx->mem, condition);
                        
                        // body
                        StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);
                        StringChain tmp = {0};
                        genChainPrintf(&tmp, &ctx->mem, ".L%i:\n", label);
                        StringChainAppendChain(&tmp, &ctx->mem, body);

                        // end label
                        if(ifEndLabel == INVALID_IF_LABEL_COUNTER) ifEndLabel = ctx->labelCounter++;
                        genChainPrintf(&tmp, &ctx->mem, "    jmp .L%i\n", ifEndLabel);
                        StringChainPrependChain(&ifBody, &ctx->mem, tmp);
                    }
                    next = globalScope->stmts.statements[i + 1];
                }while(next->type == ASTNodeType_IF || next->type == ASTNodeType_ELSE || next->type == ASTNodeType_ELSE_IF);
                
                // end label
                genChainPrintf(&ifConditions, &ctx->mem, "    jmp .L%i\n", ifEndLabel);
                genChainPrintf(&ifBody, &ctx->mem, ".L%i:\n", ifEndLabel);

                // append everything
                StringChainAppendChain(&result, &ctx->mem, ifConditions);
                StringChainAppendChain(&result, &ctx->mem, ifBody);

                // clean globals
                ifConditions.first = NULL;
                ifConditions.last = NULL;
                ifConditions.nodeCount = 0;
                ifBody.first = NULL;
                ifBody.last = NULL;
                ifBody.nodeCount = 0;
                ifEndLabel = INVALID_IF_LABEL_COUNTER;
            } break;
            case ASTNodeType_FUNCTION_CALL: {
                String id = node->node.FUNCTION_CALL.identifier;
                Args args = node->node.FUNCTION_CALL.args;

                StringChain funcCall = gen_win_x86_64_nasm_func_call(ctx, id, args);
                StringChainAppendChain(&result, &ctx->mem, funcCall);
            } break;
            case ASTNodeType_LOOP: {
                ASTNode* exprNode = node->node.LOOP.expresion;
                Scope* scope = node->node.LOOP.scope;

                StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);

                // TODO: very scuffed way of finding if the expresion evaluates to a bool or an int
                // this is temporary, should get removed when typechecking is added
                OperatorDefinition opDef;
                if(exprNode->type == ASTNodeType_EXPRESION && containsOp(ctx->opInfo, exprNode->node.EXPRESION.operator, &opDef) && StringEqualsCstr(opDef.retType.symbol, "bool")){
                    // expr evaluates to bool
                    // do the loop while condition is true
                    int startLabel = ctx->labelCounter++;
                    int endLabel = ctx->labelCounter++;
                    int conditionLabel = ctx->labelCounter++;
                    
                    StringChain condition = generate_win_x86_64_nasm_condition(ctx, exprNode, startLabel);

                    genChainPrintf(&result, &ctx->mem, ".L%i:\n", conditionLabel);
                    StringChainAppendChain(&result, &ctx->mem, condition);
                    genChainPrintf(&result, &ctx->mem, "    jmp .L%i\n", endLabel);
                    genChainPrintf(&result, &ctx->mem, ".L%i:\n", startLabel);
                    StringChainAppendChain(&result, &ctx->mem, body);
                    genChainPrintf(&result, &ctx->mem, "    jmp .L%i\n", conditionLabel);
                    genChainPrintf(&result, &ctx->mem, ".L%i:\n", endLabel);
                }else{
                    // expr is int litaral or evaluates to int
                    // do the loop n times
                    int startLabel = ctx->labelCounter++;
                    int endLabel = ctx->labelCounter++;

                    StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);

                    int savedStack = ctx->stack;
                    StringChainAppendChain(&result, &ctx->mem, expr);
                    genChainPrintf(&result, &ctx->mem, "    mov rcx, 0\n");
                    genChainPrintf(&result, &ctx->mem, "    cmp rax, rcx\n");
                    genChainPrintf(&result, &ctx->mem, ".L%i:\n", startLabel);
                    gen_win_x86_64_nasm_push(ctx, &result, "rax");
                    genChainPrintf(&result, &ctx->mem, "    jz .L%i\n", endLabel);
                    StringChainAppendChain(&result, &ctx->mem, body);
                    gen_win_x86_64_nasm_pop(ctx, &result, "rax");
                    genChainPrintf(&result, &ctx->mem, "    dec rax\n");
                    genChainPrintf(&result, &ctx->mem, "    jmp .L%i\n", startLabel);
                    genChainPrintf(&result, &ctx->mem, ".L%i:\n", endLabel);
                    genChainPrintf(&result, &ctx->mem, "    add rsp, 8\n");
                    ctx->stack--;
                    ctx->stack = savedStack;
                }
            } break;

            case ASTNodeType_ELSE:
            case ASTNodeType_ELSE_IF:
            case ASTNodeType_EXPRESION:
            case ASTNodeType_INT_LIT:
            case ASTNodeType_FLOAT_LIT:
            case ASTNodeType_STRING_LIT:
            case ASTNodeType_SYMBOL_RVALUE:
            case ASTNodeType_TYPE:
                printf("[ERROR] Unhandled AST Node type: %s\n", ASTNodeTypeStr[node->type]);
                break;
        }
    }

    return result;
}

StringChain Generate(GenContext* ctx, Scope* globalScope){
    ctx->intSize = 8;

    // header
    StringChain result = {0};
    genChainPrintf(&result, &ctx->mem, "bits 64\n");
    genChainPrintf(&result, &ctx->mem, "default rel\n");
    genChainPrintf(&result, &ctx->mem, "section .text\n");

    // TODO: temp
    genChainPrintf(&result, &ctx->mem, "global _start\n");

    // data
    StringChain dataSection = {0};
    genChainPrintf(&dataSection, &ctx->mem, "section .data\n");

    StringChain scope = generate_win_x86_64_nasm_scope(ctx, globalScope, &dataSection);
    StringChainAppendChain(&result, &ctx->mem, scope);

    // data section at the end
    StringChainAppendChain(&result, &ctx->mem, dataSection);

    return result;
}
