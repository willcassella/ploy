// demo.c

#define PLOY_IMPLEMENTATION
#include "../include/ploy.h"

int main()
{
    char buffer[4096];
    ploy_Heap heap = ploy_Heap_new(buffer, sizeof(buffer));
    ploy_ExceptionHandler error_handler = ploy_ExceptionHandler_file(stdout);
    ploy_Context ctx;
    ploy_Context_init(&ctx, &heap, error_handler);
    ploy_Context_Bookmark const bookmark = ploy_Context_bookmark(&ctx);

    printf("Welcome to the ploy REPL");
    while (1)
    {
        // Reset heap
        ploy_Context_restore(&ctx, bookmark);

        printf("\n> ");

        char source_code[1024];
        fgets(source_code, sizeof(source_code), stdin);

        // Compile code
        ploy_Value compiled_code;
        ploy_ExceptionStatus error = ploy_compile_str(ctx, source_code, &compiled_code);
        if (error)
        {
            continue;
        }

        // Run code
        ploy_Value result;
        error = ploy_begin(ctx, compiled_code.list, &result);
        if (error)
        {
            continue;
        }

        ploy_Cell* const cell = ploy_Cell_new(ctx.heap);
        if (!cell)
        {
            continue;
        }
        cell->value = result;

        ploy_Value print_result;
        ploy_print(ctx, cell, &print_result);
    }
}
