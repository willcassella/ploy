// demo.c

#define PLOY_IMPLEMENTATION
#include "../include/ploy.h"

int main()
{
	char buffer[370];
	ploy_Heap heap = ploy_Heap_new(buffer, sizeof(buffer));
	ploy_ErrorHandler error_handler = ploy_ErrorHandler_file(stdout);
	ploy_Context ctx;
	ploy_Context_init(&ctx, &heap, error_handler);

	char const* source = "(print (cons 1 (cons 2 (cons 3 (cons 4 '())))))";
	ploy_Value code;
	ploy_ErrorStatus const error = ploy_compile_str(ctx, source, &code);
	if (error)
	{
		getchar();
		return;
	}

	ploy_Value result;
	ploy_begin(ctx, code.list, &result);
	getchar();
}
