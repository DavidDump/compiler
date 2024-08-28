#include "../src/common.h"

typedef enum direction {
    none,
    left,
    right,
} direction;

const char *subtree_prefix = "  |";
const char *space_prefix = "   ";

char *make_prefix(char* prefix, const char* suffix) {
    char* result = (char* )malloc(strlen(prefix) + strlen(suffix) + 1);
    strcpy(result, prefix);
    result = strcat(result, suffix);
    return result;
}

void bst_print_node(Expr* node) {
    if(node->type == ExprType_Number) {
        printf("[%.*s]", node->primary.token.value.length, node->primary.token.value.str);
    } else if(node->type == ExprType_Variable) {
        printf("[%.*s]", node->primary.token.value.length, node->primary.token.value.str);
    }
}

void bst_print_subtree(Expr *tree, char *prefix, direction from) {
    if (tree != NULL) {
        char *current_subtree_prefix = make_prefix(prefix, subtree_prefix);
        char *current_space_prefix = make_prefix(prefix, space_prefix);

        if (from == left) {
            printf("%s\n", current_subtree_prefix);
        }

        if(tree->type == ExprType_Expr) {
            bst_print_subtree(
                tree->expr.lhs,
                from == left ? current_subtree_prefix : current_space_prefix, right
            );
        }

        printf("%s  +-", prefix);
        if(tree->type == ExprType_Expr) {
            printf("[");
            printOp(tree->expr.op);
            printf("]");
        }else{
            bst_print_node(tree);
        }
        printf("\n");

        if(tree->type == ExprType_Expr) {
            bst_print_subtree(
                tree->expr.rhs,
                from == right ? current_subtree_prefix : current_space_prefix, left
            );
        }

        if (from == right) {
            printf("%s\n", current_subtree_prefix);
        }

        free(current_space_prefix);
        free(current_subtree_prefix);
    }
}

void bst_print_tree(Expr *tree) {
    printf("Binary tree structure:\n");
    printf("\n");
    
    if (tree->type != ExprType_NONE) {
        bst_print_subtree(tree, "", none);
    } else {
        printf("Tree is empty\n");
    }
    printf("\n");
}
