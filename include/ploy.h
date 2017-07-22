// ploy.h
#pragma once

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#ifdef __cplusplus
#   define PLOY_FUNC extern "C"
#else
#   define PLOY_FUNC
#endif

#define PLOY_UNUSED(x) (void)(x)

typedef int32_t ploy_ErrorCode;
typedef enum ploy_ErrorStatus ploy_ErrorStatus;
typedef struct ploy_Value ploy_Value;
typedef enum ploy_ValueType ploy_ValueType;
typedef struct ploy_Cell ploy_Cell;
typedef struct ploy_Function ploy_Function;
typedef struct ploy_Heap ploy_Heap;
typedef enum ploy_BuiltinFunc ploy_BuiltinFunc;
typedef struct ploy_Context ploy_Context;
typedef struct ploy_CallStack ploy_CallStack;
typedef struct ploy_ErrorHandler ploy_ErrorHandler;
typedef size_t ploy_Context_Bookmark;

typedef ploy_ErrorStatus(*ploy_Function_Invoke_fn)(
    void* user_data,
    ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

typedef void(*ploy_ErrorHandler_Handle_fn)(
	void* user_data,
	ploy_Context ctx,
	ploy_ErrorCode error_code,
	char const* error_str_fmt,
	va_list fmt_args
);

enum ploy_ErrorStatus {
	PLOY_OK = 0,
	PLOY_ERROR = 1,
};

enum ploy_CoreErrorCode {
	PLOY_ERROR_CORE_OUT_OF_MEMORY = 1,
	PLOY_ERROR_CORE_UNEXPECTED_NULL,
	PLOY_ERROR_CORE_UNEXPECTED_TYPE,
	PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS,
	PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS,
	PLOY_ERROR_CORE_UNEXPECTED_TOKEN,
	PLOY_ERROR_CORE_INVALID_SYNTAX,
	PLOY_ERROR_FIRST_USER_CODE,
};

enum ploy_ValueType {
    PLOY_VALUE_LIST, // The given value is a list, the address of the first element is stored in the 'list' member.
	PLOY_VALUE_EXPR, // The given value is an expression that may be evaluated (via 'ploy_eval'), the address of which is stored in the 'list' member.
	PLOY_VALUE_BOOL, // The given value is a boolean, the value of which is stored in the 'i32' member (0/1 = true/false).
    PLOY_VALUE_I32, // The given value is an integer (NOT used for mathematical functions), the value of which is stored in the 'i32' member.
    PLOY_VALUE_F32, // The given value is a number, the value of which is stored in the 'f32' member.
    PLOY_VALUE_STR, // The given value is a string, the address of which is stored in the 'str' member.
    PLOY_VALUE_SYMBOL, // The given value is a symbol which requires lookup, the name of the symbol is stored in the 'str' member.
	PLOY_VALUE_QSYMBOL, // The given value is a quoted symbol, indicating the symbol should not be looked up. The name of the symbol is stored in the 'str' member.
	PLOY_VALUE_FUNCTION_USER, // The given value is a user-defined function the address of which is stored in the 'function' member.
	PLOY_VALUE_FUNCTION_BUILTIN, // The given value is a builtin function, the index of which is stored in the 'i32' member.
};

enum ploy_BuiltinFunc {
	/* Basic control functions */
	PLOY_BUILTIN_FUNC_BEGIN,
	PLOY_BUILTIN_FUNC_IF,
	//PLOY_BUILTIN_FUNC_COND,
	//PLOY_BUILTIN_FUNC_TRY,
	//PLOY_BUILTIN_FUNC_CATCH,
	//PLOY_BUILTIN_FUNC_TROW,

	/* Basic list functions */
	PLOY_BUILTIN_FUNC_CAR,
	PLOY_BUILTIN_FUNC_CDR,
	PLOY_BUILTIN_FUNC_CONS,
	//PLOY_BUILTIN_FUNC_MAP,

	/* Basic math functions */
	PLOY_BUILTIN_FUNC_ADD,
	PLOY_BUILTIN_FUNC_SUB,
	//PLOY_BUILTIN_FUNC_MUL,
	//PLOY_BUILTIN_FUNC_DIV,

	/* Basic boolean functions */
	//PLOY_BUILTIN_FUNC_EQ,
	//PLOY_BUILTIN_FUNC_NE,
	//PLOY_BUILTIN_FUNC_GT,
	//PLOY_BUILTIN_FUNC_LT,
	//PLOY_BUILTIN_FUNC_GTE,
	//PLOY_BUILTIN_FUNC_LTE,
	//PLOY_BUILTIN_FUNC_AND,
	//PLOY_BUILTIN_FUNC_OR,
	//PLOY_BUILTIN_FUNC_XOR,
	//PLOY_BUILTIN_FUNC_NOT,

	/* Advanced math functions */
	PLOY_BUILTIN_FUNC_SIN,
	//PLOY_BUILTIN_FUNC_COS,
	//PLOY_BUILTIN_FUNC_TAN,
	//PLOY_BUILTIN_FUNC_ASIN,
	//PLOY_BUILTIN_FUNC_ACOS,
	//PLOY_BUILTIN_FUNC_ATAN,
	//PLOY_BUILTIN_FUNC_POW,

	/* Utility functions */
	PLOY_BUILTIN_FUNC_PRINT,
	PLOY_BUILTIN_FUNC_COMPILE,
	PLOY_BUILTIN_FUNC_EVAL,
};

struct ploy_CallStack {
	char const* func_name;
	ploy_CallStack const* next;
};

PLOY_FUNC void ploy_CallStack_push(
	ploy_Context* ctx,
	ploy_CallStack* callstack_top,
	char const* func_name
);

struct ploy_ErrorHandler {
	void* user_data;
	ploy_ErrorHandler_Handle_fn handle_fn;
};

PLOY_FUNC ploy_ErrorHandler ploy_ErrorHandler_file(
	FILE* out
);

PLOY_FUNC void ploy_throw_error(
	ploy_Context ctx,
	ploy_ErrorCode error_code,
	char const* error_str_fmt,
	...
);

struct ploy_Function {
	void* user_data;
	ploy_Function_Invoke_fn invoke_fn;
};

struct ploy_Value {
	union {
		int32_t i32;
		float f32;
		char const* str;
		ploy_Cell const* list;
		ploy_Function const* function;
	};
	ploy_ValueType type;
};

/**
 * \brief Initializes a default ploy value.
 * \return A default-constructed ploy value.
 */
PLOY_FUNC ploy_Value ploy_Value_new(
);

PLOY_FUNC ploy_ErrorStatus ploy_Value_resolve(
	ploy_Context* ctx,
	ploy_Value value,
	ploy_Value* out_result
);

struct ploy_Heap {
	void* buffer;
	size_t size;
	size_t offset;
};

PLOY_FUNC ploy_Heap ploy_Heap_new(
	void* heap_buffer,
	size_t size
);

PLOY_FUNC ploy_Cell* ploy_Cell_new(
	ploy_Heap* heap
);

PLOY_FUNC char const* ploy_String_copy(
	ploy_Heap* heap,
	char const* str,
	size_t len
);

struct ploy_Context {
	ploy_Heap* heap;
	ploy_CallStack const* callstack;
	ploy_ErrorHandler error_handler;
};

PLOY_FUNC void ploy_Context_init(
	ploy_Context* ctx,
	ploy_Heap* heap,
	ploy_ErrorHandler error_handler
);

PLOY_FUNC ploy_Context_Bookmark ploy_Context_bookmark(
	ploy_Context const* context
);

PLOY_FUNC void ploy_Context_restore(
	ploy_Context* ctx,
	ploy_Context_Bookmark bookmark
);

/*
 *
 * Primary functions.
 *
 */

PLOY_FUNC ploy_ErrorStatus ploy_compile_str(
	ploy_Context ctx,
	char const* source_str,
	ploy_Value* out_result
);

PLOY_FUNC ploy_ErrorStatus ploy_begin(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

PLOY_FUNC ploy_ErrorStatus ploy_eval(
    ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

PLOY_FUNC ploy_ErrorStatus ploy_invoke(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

#ifdef PLOY_IMPLEMENTATION

#ifdef _MSC_VER
#	pragma warning(disable: 4028)
#	pragma warning(disable: 4116)
#endif

#include <math.h>

ploy_Value ploy_Value_new(
) {
	ploy_Value out;
	memset(&out, 0, sizeof(ploy_Value));
	return out;
}

ploy_ErrorStatus ploy_Value_resolve(
	ploy_Context ctx,
	ploy_Value value,
	ploy_Value* const out_result
) {
	ploy_ErrorStatus error = PLOY_OK;
	while (!error)
	{
		// If the argument is an expression
		if (value.type == PLOY_VALUE_EXPR)
		{
			error = ploy_invoke(ctx, value.list, &value);
		}
		else
		{
			break;
		}
		// TODO: Add symbol lookup
	}

	*out_result = value;
	return error;
}

void ploy_CallStack_push(
	ploy_Context* const ctx,
	ploy_CallStack* const callstack,
	char const* const func_name
) {
	callstack->func_name = func_name;
	callstack->next = ctx->callstack;
	ctx->callstack = callstack;
}

static void ploy_file_error_handler(
	void* const user_data,
	ploy_Context const ctx,
	ploy_ErrorCode const error_code,
	char const* const error_str_fmt,
	va_list fmt_args
) {
	FILE* const out = (FILE*)user_data;

	char const* error_code_str = NULL;
	switch ((enum ploy_CoreErrorCode)error_code)
	{
	case PLOY_ERROR_CORE_OUT_OF_MEMORY:
		error_code_str = "PLOY_ERROR_CORE_OUT_OF_MEMORY";
		break;

	case PLOY_ERROR_CORE_UNEXPECTED_NULL:
		error_code_str = "PLOY_ERROR_CORE_UNEXPECTED_NULL";
		break;

	case PLOY_ERROR_CORE_UNEXPECTED_TYPE:
		error_code_str = "PLOY_ERROR_CORE_UNEXPECTED_TYPE";
		break;

	case PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS:
		error_code_str = "PLOY_ERROR_CORE_UNEXPECTED_TYPE";
		break;

	case PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS:
		error_code_str = "PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS";
		break;

	case PLOY_ERROR_CORE_UNEXPECTED_TOKEN:
		error_code_str = "PLOY_ERROR_CORE_UNEXPECTED_TOKEN";
		break;

	case PLOY_ERROR_CORE_INVALID_SYNTAX:
		error_code_str = "PLOY_ERROR_CORE_INVALID_SYNTAX";
		break;

	default:
		error_code_str = "USER ERROR CODE";
		break;
	}

	// Print out error code, name, and description
	fprintf(out, "An exception was thrown, error code %d [%s]:\n", error_code, error_code_str);
	vfprintf(out, error_str_fmt, fmt_args);

	// Print out callstack
	fprintf(out, "\nCallstack (most recent call first):\n");
	for (ploy_CallStack const* callstack = ctx.callstack; callstack != NULL; callstack = callstack->next)
	{
		fprintf(out, "(%s)\n", callstack->func_name);
	}
}

ploy_ErrorHandler ploy_ErrorHandler_file(
	FILE* const file
) {
	ploy_ErrorHandler handler;
	handler.user_data = file;
	handler.handle_fn = &ploy_file_error_handler;

	return handler;
}

void ploy_throw_error(
	ploy_Context const ctx,
	ploy_ErrorCode const error_code,
	char const* const error_str_fmt,
	...
) {
	va_list fmt_args;
	va_start(fmt_args, error_str_fmt);
	ctx.error_handler.handle_fn(ctx.error_handler.user_data, ctx, error_code, error_str_fmt, fmt_args);
	va_end(fmt_args);
}

void ploy_Context_init(
	ploy_Context* const ctx,
	ploy_Heap* const heap,
	ploy_ErrorHandler const error_handler
) {
	ctx->heap = heap;
	ctx->callstack = NULL;
	ctx->error_handler = error_handler;
}

ploy_Context_Bookmark ploy_Context_bookmark(
	ploy_Context const* const ctx
) {
	return ctx->heap->offset;
}

void ploy_Context_restore(
	ploy_Context* const ctx,
	ploy_Context_Bookmark const bookmark
) {
	ctx->heap->offset = bookmark;
}

struct ploy_Cell {
	ploy_Value value;
	ploy_Cell const* next;
};

/* Ploy cells are at least at aligned as a float, and at most as aligned as a pointer. */
static size_t const PLOY_CELL_ALIGNMENT = sizeof(float) > sizeof(void*) ? sizeof(float) : sizeof(void*);

ploy_Heap ploy_Heap_new(
	void* const buffer,
	size_t const size
) {
	ploy_Heap out;
	out.buffer = buffer;
	out.size = size;
	out.offset = 0;

	return out;
}

ploy_Cell* ploy_Cell_new(
	ploy_Heap* const heap
) {
	// Ensure there's enough space on the heap
	if (heap->offset + sizeof(ploy_Cell) > heap->size)
	{
		return NULL;
	}

	// Allocate space and increment offset
	ploy_Cell* const cell = (ploy_Cell*)((char*)heap->buffer + heap->offset);
	heap->offset += sizeof(ploy_Cell);

	// Initialize cell
	memset(cell, 0, sizeof(ploy_Cell));

	return cell;
}

char const* ploy_String_copy(
	ploy_Heap* heap,
	char const* const str,
	size_t const len
) {
	// Ensure there's enough space on the heap
	size_t const total_size = len + 1;
	if (heap->size < total_size)
	{
		return NULL;
	}

	// Allocate space, and increment offset (accounting for alignment)
	char* const result = (char*)heap->buffer + heap->offset;
	size_t const alignment_diff = PLOY_CELL_ALIGNMENT - total_size % PLOY_CELL_ALIGNMENT;
	if (alignment_diff == PLOY_CELL_ALIGNMENT)
	{
		heap->offset += total_size;
	}
	else if (heap->offset + total_size + alignment_diff > heap->size)
	{
		heap->offset = heap->size;
	}
	else
	{
		heap->offset += total_size + alignment_diff;
	}

	// Copy the string
	memcpy(result, str, len);
	result[len] = 0;

	return result;
}

/*
*
* Basic control functions
*
*/

ploy_ErrorStatus ploy_begin(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "begin");

	*out_result = ploy_Value_new();
	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, out_result);
		if (error)
		{
			return PLOY_ERROR;
		}
	}

	return PLOY_OK;
}

static ploy_ErrorStatus ploy_if(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "if");

	if (!arg_list || !arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "if: too few arguments (expected 2-3)");
		return PLOY_ERROR;
	}
	if (arg_list->next->next && arg_list->next->next->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "if: too many arguments (expected 2-3)");
		return PLOY_ERROR;
	}

	// Evaluate the first argument to a boolean
	ploy_Value predicate;
	ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &predicate);
	if (error)
	{
		return PLOY_ERROR;
	}
	arg_list = arg_list->next;

	if (predicate.type != PLOY_VALUE_BOOL)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "if: predicate argument was not a boolean");
		return PLOY_ERROR;
	}

	if (predicate.i32)
	{
		// If the expression is true, run the true branch
		return ploy_Value_resolve(ctx, arg_list->value, out_result);
	}
	else if (arg_list->next)
	{
		// If a false branch was given, run that
		return ploy_Value_resolve(ctx, arg_list->next->value, out_result);
	}

	// Otherwise, just return default value
	*out_result = ploy_Value_new();
	return PLOY_OK;
}

/*
 *
 * Basic list functions
 *
 */

static ploy_ErrorStatus ploy_car(
	ploy_Context ctx,
	ploy_Cell const* const arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "car");

	// Check argument count (only supports 1 argument)
	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "car: too few arguments (expected 1, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "car: too many arguments (expected 1, got more)");
		return PLOY_ERROR;
	}

	// Resolve argument
	ploy_Value arg;
	ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &arg);
	if (error)
	{
		return PLOY_ERROR;
	}

	if (arg.type != PLOY_VALUE_LIST)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "car: argument must be a list");
		return PLOY_ERROR;
	}

	// Make sure list isn't empty
	if (!arg.list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_NULL, "car: received empty list");
		return PLOY_ERROR;
	}

	return ploy_Value_resolve(ctx, arg.list->value, out_result);
}

static ploy_ErrorStatus ploy_cdr(
	ploy_Context ctx,
	ploy_Cell const* const arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "cdr");

	// Check argument count (only supports 1 argument)
	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "cdr: too few arguments (expected 1, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "cdr: too many arguments (expected 1, got more)");
		return PLOY_ERROR;
	}

	// Resolve arguments
	ploy_Value arg;
	ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &arg);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Check arg type (must be a list)
	if (arg.type != PLOY_VALUE_LIST)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "cdr: argument must be a list");
		return PLOY_ERROR;
	}

	// Make sure list isn't empty
	if (!arg.list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_NULL, "cdr: received empty list");
		return PLOY_ERROR;
	}

	out_result->type = PLOY_VALUE_LIST;
	out_result->list = arg.list->next;
	return PLOY_OK;
}

static ploy_ErrorStatus ploy_cons(
	ploy_Context ctx,
	ploy_Cell const* const arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "cons");

	// Check argument count (only suports 1-2 arguments)
	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "cons: too few arguments (expected 2, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next && arg_list->next->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "cons: too many arguments (expected 2, got more)");
		return PLOY_ERROR;
	}

	// Get value for start of list
	ploy_Value first;
	ploy_ErrorStatus error = ploy_Value_resolve(ctx, arg_list->value, &first);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Get value for rest of list
	ploy_Cell const* list = NULL;
	if (arg_list->next)
	{
		ploy_Value list_val;
		error = ploy_Value_resolve(ctx, arg_list->next->value, &list_val);
		if (error)
		{
			return PLOY_ERROR;
		}

		if (list_val.type != PLOY_VALUE_LIST)
		{
			ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "cons: second argument must be a list");
			return PLOY_ERROR;
		}

		list = list_val.list;
	}

	// Create list
	ploy_Cell* const list_start = ploy_Cell_new(ctx.heap);
	if (!list_start)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_OUT_OF_MEMORY, "cons: failed to allocate Cell");
		return PLOY_ERROR;
	}
	list_start->value = first;
	list_start->next = list;

	out_result->type = PLOY_VALUE_LIST;
	out_result->list = list_start;
	return PLOY_OK;
}

/*
 *
 * Basic math functions
 *
 */

static ploy_ErrorStatus ploy_add(
    ploy_Context ctx,
    ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "+");

	out_result->type = PLOY_VALUE_I32;
	out_result->i32 = 0;

    // Do the addition
	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_Value value;
		ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &value);
		if (error)
		{
			return PLOY_ERROR;
		}

		switch (value.type)
		{
		case PLOY_VALUE_I32:
			if (out_result->type == PLOY_VALUE_I32)
			{
				out_result->i32 += value.i32;
			}
			else
			{
				out_result->f32 += (float)value.i32;
			}
			break;

		case PLOY_VALUE_F32:
			if (out_result->type == PLOY_VALUE_I32)
			{
				out_result->type = PLOY_VALUE_F32;
				out_result->f32 = (float)out_result->i32;
			}

			out_result->f32 += value.f32;
			break;

		default:
			ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "+: arguments must be of integral or floating-point type");
			return PLOY_ERROR;
		}
	}

	return PLOY_OK;
}

static ploy_ErrorStatus ploy_sub(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "-");

	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "-: too few arguments (expected at least 1, got 0)");
		return PLOY_ERROR;
	}

	ploy_Value value;
	ploy_ErrorStatus error = ploy_Value_resolve(ctx, arg_list->value, &value);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Check first argument type
	if (value.type != PLOY_VALUE_I32 && value.type != PLOY_VALUE_F32)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "-: arguments must be of integral or floating-point type");
		return PLOY_ERROR;
	}

	// If only a single value was given, just negate that
	if (!arg_list->next)
	{
		if (value.type == PLOY_VALUE_I32)
		{
			out_result->type = PLOY_VALUE_I32;
			out_result->i32 = -value.i32;
		}
		else
		{
			out_result->type = PLOY_VALUE_F32;
			out_result->f32 = -value.f32;
		}

		return PLOY_OK;
	}

	// Do the subtraction
	for (arg_list = arg_list->next; arg_list != NULL; arg_list = arg_list->next)
	{
		// Evaluate expression, if given
		error = ploy_Value_resolve(ctx, arg_list->value, &value);
		if (error)
		{
			return PLOY_ERROR;
		}

		switch (value.type)
		{
		case PLOY_VALUE_I32:
			if (out_result->type == PLOY_VALUE_I32)
			{
				out_result->i32 -= value.i32;
			}
			else if (out_result->type == PLOY_VALUE_F32)
			{
				out_result->f32 -= (float)value.f32;
			}
			break;

		case PLOY_VALUE_F32:
			if (out_result->type == PLOY_VALUE_I32)
			{
				out_result->type = PLOY_VALUE_F32;
				out_result->f32 = (float)out_result->i32;
			}

			out_result->f32 -= value.f32;
			break;

		default:
			ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "-: arguments must be of integral or floating-point type");
			return PLOY_ERROR;
		}
	}

	return PLOY_OK;
}

/*
 *
 * Advanced math functions
 *
 */

static ploy_ErrorStatus ploy_sin(
	ploy_Context ctx,
	ploy_Cell const* const arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "sin");

	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "sin: too few arguments (expected 1, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "sin: too many arguments (expected 1, got more)");
		return PLOY_ERROR;
	}

	// Get first argument
	ploy_Value arg;
	ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &arg);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Check type
	float arg_value = 0.f;
	if (arg.type == PLOY_VALUE_F32)
	{
		arg_value = arg.f32;
	}
	else if (arg.type == PLOY_VALUE_I32)
	{
		arg_value = (float)arg.i32;
	}
	else
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "sin: argument must be of integral or floating-point type");
		return PLOY_ERROR;
	}

	out_result->type = PLOY_VALUE_F32;
	out_result->f32 = sinf(arg_value);
	return PLOY_OK;
}

/*
 *
 * Utility functions.
 *
 */

static ploy_ErrorStatus ploy_print(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "print");

	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_Value value;
		ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &value);
		if (error)
		{
			return PLOY_ERROR;
		}

		switch (value.type)
		{
		case PLOY_VALUE_LIST:
			printf("'( ");
			ploy_print(ctx, value.list, out_result);
			printf(") ");
			break;

		case PLOY_VALUE_BOOL:
			if (value.i32)
			{
				printf("true ");
			}
			else
			{
				printf("false ");
			}
			break;

		case PLOY_VALUE_I32:
			printf("%d ", value.i32);
			break;

		case PLOY_VALUE_F32:
			printf("%f ", value.f32);
			break;

		case PLOY_VALUE_STR:
			printf("\"%s\" ", value.str);
			break;
		}
	}

	*out_result = ploy_Value_new();
	return PLOY_OK;
}

static ploy_ErrorStatus ploy_compile(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "compile");

	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "compile: too few arguments (expected 1, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "compile: too many arguments (expected 1, got more)");
		return PLOY_ERROR;
	}

	ploy_Value source;
	ploy_ErrorStatus const error = ploy_Value_resolve(ctx, arg_list->value, &source);
	if (error)
	{
		return PLOY_ERROR;
	}

	if (source.type != PLOY_VALUE_STR)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "compile: argument must be a string");
		return PLOY_ERROR;
	}

	return ploy_compile_str(ctx, source.str, out_result);
}

ploy_ErrorStatus ploy_eval(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "eval");

	if (!arg_list)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_FEW_ARGUMENTS, "eval: too few arguments (expected 1, got 0)");
		return PLOY_ERROR;
	}
	if (arg_list->next)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_TOO_MANY_ARGUMENTS, "eval: too many arguments (expected 1, got more)");
		return PLOY_ERROR;
	}

	// Evaluate the first argument to a list
	ploy_Value list;
	ploy_ErrorStatus error = ploy_Value_resolve(ctx, arg_list->value, &list);
	if (error)
	{
		return PLOY_ERROR;
	}
	if (list.type != PLOY_VALUE_LIST)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "eval: argument must be a list");
		return PLOY_ERROR;
	}

	return ploy_invoke(ctx, list.list, out_result);
}

ploy_ErrorStatus ploy_invoke(
	ploy_Context ctx,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	if (!arg_list)
	{
		*out_result = ploy_Value_new();
		return PLOY_OK;
	}

	// Evaluate the first argument to a procedure-ish thing
	ploy_Value proc;
	ploy_ErrorStatus error = ploy_Value_resolve(ctx, arg_list->value, &proc);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Procedure thing may be a builtin function or user function
	if (proc.type == PLOY_VALUE_FUNCTION_BUILTIN)
	{
		switch ((ploy_BuiltinFunc)proc.i32)
		{
		/* Basic control functions */
		case PLOY_BUILTIN_FUNC_BEGIN:
			return ploy_begin(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_IF:
			return ploy_if(ctx, arg_list->next, out_result);

			/* Basic list functions */
		case PLOY_BUILTIN_FUNC_CAR:
			return ploy_car(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_CDR:
			return ploy_cdr(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_CONS:
			return ploy_cons(ctx, arg_list->next, out_result);

			/* Basic math functions */
		case PLOY_BUILTIN_FUNC_ADD:
			return ploy_add(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_SUB:
			return ploy_sub(ctx, arg_list->next, out_result);

			/* Advanced math functions */
		case PLOY_BUILTIN_FUNC_SIN:
			return ploy_sin(ctx, arg_list->next, out_result);

			/* Utility functions */
		case PLOY_BUILTIN_FUNC_PRINT:
			return ploy_print(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_COMPILE:
			return ploy_compile(ctx, arg_list->next, out_result);

		case PLOY_BUILTIN_FUNC_EVAL:
			return ploy_eval(ctx, arg_list->next, out_result);
		}
	}

	if (proc.type == PLOY_VALUE_FUNCTION_USER)
	{
		return proc.function->invoke_fn(proc.function->user_data, ctx, arg_list->next, out_result);
	}

	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "invoke");
	ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TYPE, "invoke: given function object was not a function type. Type code %d", proc.type);
	return PLOY_ERROR;
}

/*
 *
 * Compiler code.
 *
 */

typedef enum ploy_TokenType {
	PLOY_TOKEN_NONE,
	PLOY_TOKEN_BEGIN,
	PLOY_TOKEN_END,
	PLOY_TOKEN_QUOTE,
	PLOY_TOKEN_I32,
	PLOY_TOKEN_F32,
	PLOY_TOKEN_STRING,
	PLOY_TOKEN_SYMBOL,
} ploy_TokenType;

static char const* ploy_pass_whitespace(
	char const* source_str
) {
	while (*source_str && isspace(*source_str))
	{
		source_str += 1;
	}
	return source_str;
}

static int ploy_is_symbol_char(
	char const c
) {
	return isalpha(c) || isdigit(c)
		|| c == '~' || c == '!' || c == '$' || c == '%' || c == '^' || c == '&' || c == '*' || c == '-' || c == '_' || c == '=' || c == '+' || c == ':' || c == '<' || c == '.' || c == '>' || c == '/' || c == '?';
}

static ploy_ErrorStatus ploy_next_token(
	ploy_Context ctx,
	char const** const io_source_str,
	size_t* out_token_len,
	ploy_TokenType* out_token_type
) {
	char const* source_str = *io_source_str;

	// Skip whitespace
	source_str = ploy_pass_whitespace(source_str);
	*io_source_str = source_str;

	// If we've reached the end of the string, no token to return
	if (!*source_str)
	{
		*out_token_len = 0;
		*out_token_type = PLOY_TOKEN_NONE;
		return PLOY_OK;
	}

	// Check for begin
	if (*source_str == '(')
	{
		*out_token_len = 1;
		*out_token_type = PLOY_TOKEN_BEGIN;
		return PLOY_OK;
	}

	// Check for end
	if (*source_str == ')')
	{
		*out_token_len = 1;
		*out_token_type = PLOY_TOKEN_END;
		return PLOY_OK;
	}

	// Check for quote
	if (*source_str == '\'')
	{
		*out_token_len = 1;
		*out_token_type = PLOY_TOKEN_QUOTE;
		return PLOY_OK;
	}

	// Check for numbers
	if (isdigit(*source_str) || (*source_str == '+' || *source_str == '-' || *source_str == '.') && isdigit(source_str[1]))
	{
		*out_token_type = PLOY_TOKEN_I32;

		// Pass following digits
		char const* token_end = source_str + 1;
		while (isdigit(*token_end))
		{
			token_end += 1;
		}

		// If a decimal point is reached, make sure it's a correctly formed float
		if (*token_end == '.')
		{
			token_end += 1;

			if (!isdigit(*token_end))
			{
				*out_token_len = 0;
				*out_token_type = PLOY_TOKEN_NONE;
				ploy_throw_error(ctx, PLOY_ERROR_CORE_INVALID_SYNTAX, "A decimal point must be followed by at least one digit");
				return PLOY_ERROR;
			}

			// Skip remaining digits
			while (isdigit(*token_end))
			{
				token_end += 1;
			}

			*out_token_type = PLOY_TOKEN_F32;
		}

		if (!isspace(*token_end) && *token_end != '(' && *token_end != ')' && *token_end != 0)
		{
			*out_token_len = 0;
			*out_token_type = PLOY_TOKEN_NONE;
			ploy_throw_error(ctx, PLOY_ERROR_CORE_INVALID_SYNTAX, "Only whitespace, '(', ')', or EOF may follow a digit");
			return PLOY_ERROR;
		}

		*out_token_len = token_end - source_str;
		return PLOY_OK;
	}

	// Check for string
	if (*source_str == '"')
	{
		char const* token_end = source_str;
		for (token_end += 1; *token_end != 0; token_end += 1)
		{
			// Skip escaped quotes
			if (token_end[0] == '\\' && token_end[1] == '"')
			{
				token_end += 1;
				continue;
			}

			if (*token_end == '"')
			{
				break;
			}
		}

		// Make sure we ended with a closing quote
		if (*token_end != '"')
		{
			*out_token_len = 0;
			*out_token_type = PLOY_TOKEN_NONE;
			ploy_throw_error(ctx, PLOY_ERROR_CORE_INVALID_SYNTAX, "EOF reached before closing quotation mark");
			return PLOY_ERROR;
		}

		*out_token_len = token_end - source_str + 1;
		*out_token_type = PLOY_TOKEN_STRING;
		return PLOY_OK;
	}

	// Must be a symbol (don't need to check for invalid starting characters, since those have already been covered in previous cases)
	if (ploy_is_symbol_char(*source_str))
	{
		char const* token_end = source_str + 1;
		while (ploy_is_symbol_char(*token_end))
		{
			token_end += 1;
		}

		*out_token_len = token_end - source_str;
		*out_token_type = PLOY_TOKEN_SYMBOL;
		return PLOY_OK;
	}

	// An error must have happened somewhere
	*out_token_len = 0;
	*out_token_type = PLOY_TOKEN_NONE;
	ploy_throw_error(ctx, PLOY_ERROR_CORE_INVALID_SYNTAX, "An unknown parsing error occurred");
	return PLOY_ERROR;
}

static ploy_ErrorStatus ploy_parse_symbol(
	ploy_Context ctx,
	char const* const symbol_str,
	size_t const symbol_len,
	ploy_Value* const out_result
) {
	// Check builtin functions
	out_result->type = PLOY_VALUE_FUNCTION_BUILTIN;

	/* Basic control functions */
	if (symbol_len == 5 && !strncmp(symbol_str, "begin", 5))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_BEGIN;
		return PLOY_OK;
	}
	if (symbol_len == 2 && !strncmp(symbol_str, "if", 2))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_IF;
		return PLOY_OK;
	}

	/* Basic list functions */
	if (symbol_len == 3 && !strncmp(symbol_str, "car", 3))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_CAR;
		return PLOY_OK;
	}
	if (symbol_len == 3 && !strncmp(symbol_str, "cdr", 3))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_CDR;
		return PLOY_OK;
	}
	if (symbol_len == 4 && !strncmp(symbol_str, "cons", 4))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_CONS;
		return PLOY_OK;
	}

	/* Basic math functions */
	if (symbol_len == 1 && *symbol_str == '+')
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_ADD;
		return PLOY_OK;
	}
	if (symbol_len == 1 && *symbol_str == '-')
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_SUB;
		return PLOY_OK;
	}

	/* Advanced math functions */
	if (symbol_len == 3 && !strncmp(symbol_str, "sin", 3))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_SIN;
		return PLOY_OK;
	}

	/* Utility functions */
	if (symbol_len == 5 && !strncmp(symbol_str, "print", 5))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_PRINT;
		return PLOY_OK;
	}
	if (symbol_len == 7 && !strncmp(symbol_str, "compile", 7))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_COMPILE;
		return PLOY_OK;
	}
	if (symbol_len == 4 && !strncmp(symbol_str, "eval", 4))
	{
		out_result->i32 = PLOY_BUILTIN_FUNC_EVAL;
		return PLOY_OK;
	}

	// Must be a user symbol, copy the string
	out_result->type = PLOY_VALUE_SYMBOL;
	out_result->str = ploy_String_copy(ctx.heap, symbol_str, symbol_len);
	if (!out_result->str)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_OUT_OF_MEMORY, "compile_str: failed to allocate string");
		return PLOY_ERROR;
	}

	return PLOY_OK;
}

static ploy_ErrorStatus ploy_compile_recursive(
	ploy_Context ctx,
	char const** io_source_str,
	size_t* out_token_len,
	ploy_TokenType* out_token_type,
	ploy_Cell const** out_list
) {
	// Get the next token
	ploy_ErrorStatus error = ploy_next_token(ctx, io_source_str, out_token_len, out_token_type);
	if (error)
	{
		return PLOY_ERROR;
	}

	while (*out_token_type != PLOY_TOKEN_NONE && *out_token_type != PLOY_TOKEN_END)
	{
		// Allocate a new cell for whatever we're looking at
		ploy_Cell* const cell = ploy_Cell_new(ctx.heap);
		if (!cell)
		{
			ploy_throw_error(ctx, PLOY_ERROR_CORE_OUT_OF_MEMORY, "compile_str: failed to allocate Cell");
			return PLOY_ERROR;
		}

		*out_list = cell;
		out_list = &cell->next;

		switch (*out_token_type)
		{
		case PLOY_TOKEN_I32:
			cell->value.type = PLOY_VALUE_I32;
			cell->value.i32 = strtol(*io_source_str, NULL, 10);
			*io_source_str += *out_token_len;
			break;

		case PLOY_TOKEN_F32:
			cell->value.type = PLOY_VALUE_F32;
			cell->value.f32 = strtof(*io_source_str, NULL);
			*io_source_str += *out_token_len;
			break;

		case PLOY_TOKEN_STRING:
			cell->value.type = PLOY_VALUE_STR;
			cell->value.str = ploy_String_copy(ctx.heap, *io_source_str + 1, *out_token_len - 2);
			*io_source_str += *out_token_len;

			// Make sure the string was sucessfully allocated
			if (!cell->value.str)
			{
				ploy_throw_error(ctx, PLOY_ERROR_CORE_OUT_OF_MEMORY, "compile_str: failed to allocate string");
				return PLOY_ERROR;
			}
			break;

		case PLOY_TOKEN_BEGIN:
			*io_source_str += *out_token_len;
			cell->value.type = PLOY_VALUE_EXPR;
			error = ploy_compile_recursive(ctx, io_source_str, out_token_len, out_token_type, &cell->value.list);
			*io_source_str += *out_token_len;

			if (error)
			{
				return PLOY_ERROR;
			}

			// A 'begin' token MUST be ulitmately terminated by an 'end' token
			if (*out_token_type != PLOY_TOKEN_END)
			{
				ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TOKEN, "compile_str: expected ')' token. Got token code %d", *out_token_type);
				return PLOY_ERROR;
			}
			break;

		case PLOY_TOKEN_QUOTE:
			// Get the next token
			*io_source_str += *out_token_len;
			error = ploy_next_token(ctx, io_source_str, out_token_len, out_token_type);
			if (error)
			{
				return PLOY_ERROR;
			}

			if (*out_token_type == PLOY_TOKEN_SYMBOL)
			{
				cell->value.type = PLOY_VALUE_QSYMBOL;
				cell->value.str = ploy_String_copy(ctx.heap, *io_source_str, *out_token_len);
				*io_source_str += *out_token_len;

				// Make sure the string was successfully allocated
				if (!cell->value.str)
				{
					ploy_throw_error(ctx, PLOY_ERROR_CORE_OUT_OF_MEMORY, "compile_str: failed to allocate string");
					return PLOY_ERROR;
				}
			}
			else if (*out_token_type == PLOY_TOKEN_BEGIN)
			{
				*io_source_str += *out_token_len;
				cell->value.type = PLOY_VALUE_LIST;
				error = ploy_compile_recursive(ctx, io_source_str, out_token_len, out_token_type, &cell->value.list);
				if (error)
				{
					return PLOY_ERROR;
				}

				// Make sure the begin was matched with an end
				if (*out_token_type != PLOY_TOKEN_END)
				{
					ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TOKEN, "compile_str: expected ')' token. Got token code %d", *out_token_type);
					return PLOY_ERROR;
				}
				*io_source_str += *out_token_len;
			}
			break;

		case PLOY_TOKEN_SYMBOL:
			error = ploy_parse_symbol(ctx, *io_source_str, *out_token_len, &cell->value);
			*io_source_str += *out_token_len;
			if (error)
			{
				return PLOY_ERROR;
			}
			break;
		}

		error = ploy_next_token(ctx, io_source_str, out_token_len, out_token_type);
		if (error)
		{
			return PLOY_ERROR;
		}
	}

	*out_list = NULL;
	return PLOY_OK;
}

ploy_ErrorStatus ploy_compile_str(
	ploy_Context ctx,
	char const* source_str,
	ploy_Value* const out_result
) {
	ploy_CallStack callstack;
	ploy_CallStack_push(&ctx, &callstack, "compile_str");

	out_result->type = PLOY_VALUE_LIST;
	size_t token_len;
	ploy_TokenType token_type;

	// Recursively compile the string
	ploy_ErrorStatus const error = ploy_compile_recursive(ctx, &source_str, &token_len, &token_type, &out_result->list);
	if (error)
	{
		return PLOY_ERROR;
	}

	// Last token must have been NONE
	if (token_type != PLOY_TOKEN_NONE)
	{
		ploy_throw_error(ctx, PLOY_ERROR_CORE_UNEXPECTED_TOKEN, "compile_str: expected EOF. Got token code %d", token_type);
		return PLOY_ERROR;
	}

	return PLOY_OK;
}

#endif
