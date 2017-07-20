// demo.c

#define PLOY_IMPLEMENTATION
#include "../include/ploy.h"

int main()
{
	char buffer[4096];
	ploy_Heap heap = ploy_Heap_new(buffer, sizeof(buffer));

	char const* source = "(print (eval (compile \"+ 1 2\" )))";
	ploy_Value code;
	ploy_Error const error = ploy_compile_str(&heap, source, &code);
	if (error)
	{
		printf("An error occured while compiling: %d", error);
		scanf("");
		return error;
	}

	ploy_Value result;
	ploy_begin(&heap, code.list, &result);
	scanf("");
}
