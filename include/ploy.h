// ploy.h
#pragma once

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef __cplusplus
#   define PLOY_FUNC extern "C"
#else
#   define PLOY_FUNC
#endif

#define PLOY_UNUSED(x) (void)(x)

/*
 *
 * Common typedefs
 *
 */

typedef int32_t ploy_Error;
typedef struct ploy_Value ploy_Value;
typedef enum ploy_ValueType ploy_ValueType;
typedef struct ploy_Cell ploy_Cell;
typedef struct ploy_Function ploy_Function;
typedef struct ploy_Heap ploy_Heap;

typedef ploy_Error(*ploy_Function_Invoke_fn)(
    void* user_data,
	ploy_Heap* heap,
    ploy_Cell const* arg_list,
	ploy_Value* out_value
);

#define PLOY_ERROR_NONE 0
#define PLOY_ERROR_UNKNOWN 1
#define PLOY_ERROR_OUT_OF_MEMORY 2
#define PLOY_ERROR_INVALID_SYNTAX 3
#define PLOY_ERROR_INVALID_TYPE 4
#define PLOY_ERROR_UNDEFINED_SYMBOL 5
#define PLOY_ERROR_TOO_MANY_ARGUMENTS 6
#define PLOY_ERROR_TOO_FEW_ARGUMENTS 7

enum ploy_ValueType {
    PLOY_VALUE_LIST, // The given value is a list, the address of the first element is stored in the 'list' member.
	PLOY_VALUE_EXPR, // The given value is an expression that may be evaluated (via 'ploy_eval'), the address of which is stored in the 'list' member.
	PLOY_VALUE_BOOL, // The given value is a boolean, the value of which is stored in the 'i32' member (0/1 = true/false).
    PLOY_VALUE_I32, // The given value is an integer (NOT used for mathematical functions), the value of which is stored in the 'i32' member.
    PLOY_VALUE_F32, // The given value is a number, the value of which is stored in the 'f32' member.
    PLOY_VALUE_STR, // The given value is a string, the address of which is stored in the 'str' member.
    PLOY_VALUE_SYMBOL, // The given value is a symbol which requires lookup, the name of the symbol is stored in the 'str' member.
	PLOY_VALUE_QSYMBOL, // The given value is a quoted symbol, indicating the symbol should not be looked up. The name of the symbol is stored in the 'str' member.
	PLOY_VALUE_SYNTAX, // The given value is a syntax object, the index of which is stored in the 'i32' member.
	PLOY_VALUE_FUNCTION_USER, // The given value is a user-defined function the address of which is stored in the 'function' member.
	PLOY_VALUE_FUNCTION_BUILTIN, // The given value is a builtin function, the index of which is stored in the 'i32' member.
};

/* Syntax objects. */
#define PLOY_SYNTAX_BEGIN 1
#define PLOY_SYNTAX_IF 2
#define PLOY_SYNTAX_COND 3
#define PLOY_SYNTAX_ELSE 4

/* Basic math functions. */
#define PLOY_BUILTIN_ADD 1
#define PLOY_BUILTIN_SUB 2
#define PLOY_BUILTIN_MUL 3
#define PLOY_BUILTIN_DIV 4

/* Basic boolean functions. */
#define PLOY_BUILTIN_EQ 5
#define PLOY_BUILTIN_NE 6
#define PLOY_BUILTIN_GT 7
#define PLOY_BUILTIN_LT 8
#define PLOY_BUILTIN_GTE 9
#define PLOY_BUILTIN_LTE 10
#define PLOY_BUILTIN_AND 11
#define PLOY_BUILTIN_OR 12
#define PLOY_BUILTIN_NOT 13

/* Advanced math functions. */
#define PLOY_BUILTIN_SIN 14
#define PLOY_BUILTIN_COS 15
#define PLOY_BUILTIN_TAN 16
#define PLOY_BUILTIN_ASIN 17
#define PLOY_BUILTIN_ACOS 18
#define PLOY_BUILTIN_ATAN 19
#define PLOY_BUILTIN_POW 20

/* Utility functions. */
#define PLOY_BUILTIN_PRINT 21
#define PLOY_BUILTIN_COMPILE 22
#define PLOY_BUILTIN_EVAL 23

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

/**
 * \brief Resolves the given value (which may be an expression or symbol) into an actual value.
 * \param heap Heap to allocate to when evaluating expressions.
 * \param value Value to resolve.
 * \param out_result Value to store the result in.
 * \return An error code, if one occurs.
 */
PLOY_FUNC ploy_Error ploy_Value_resolve(
	ploy_Heap* heap,
	ploy_Value value,
	ploy_Value* out_result
);

struct ploy_Heap {
	void* buffer;
	size_t size;
	size_t offset;
};

/**
 * \brief Initializes a heap object.
 * \param buffer The buffer to associate with the heap.
 * \param size The size of the given buffer.
 */
PLOY_FUNC ploy_Heap ploy_Heap_new(
	void* buffer,
	size_t size
);

/**
 * \brief Bookmarks the current location in the given heap, so that it may be restored later.
 * \param heap The heap object to bookmark.
 */
PLOY_FUNC size_t ploy_Heap_bookmark(
	ploy_Heap const* heap
);

/**
 * \brief Restores the heap to the given bookmark.
 * \param heap The heap object to bookmark.
 * \param bookmark The boorkmark to restore to.
 */
PLOY_FUNC void ploy_Heap_restore(
	ploy_Heap* heap,
	size_t bookmark
);

/**
 * \brief Allocates a default-initialized cell on the given heap.
 * \param heap The heap to allocate from.
 * \return A pointer to the allocated cell.
 */
PLOY_FUNC ploy_Cell* ploy_Cell_new(
	ploy_Heap* heap
);

/**
 * \brief Copies a string onto the ploy heap.
 * \param heap The heap to allocate the string on.
 * \param str The address of the start of the string to copy. This string is not required to be null-terminated.
 * \param len The number of characters in the string to copy.
 * \return The address of the copied (and null-terminated) string.
 */
PLOY_FUNC char const* ploy_String_copy(
	ploy_Heap* heap,
	char const* str,
	size_t len
);

/*
 *
 * Primary functions.
 *
 */

PLOY_FUNC ploy_Error ploy_compile_str(
	ploy_Heap* heap,
	char const* source_str,
	ploy_Value* out_result
);

PLOY_FUNC ploy_Error ploy_begin(
	ploy_Heap* heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

PLOY_FUNC ploy_Error ploy_eval(
    ploy_Heap* heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

PLOY_FUNC ploy_Error ploy_invoke(
	ploy_Heap* heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
);

#ifdef PLOY_IMPLEMENTATION

#ifdef _MSC_VER
#	pragma warning(disable: 4028)
#	pragma warning(disable: 4116)
#endif

ploy_Value ploy_Value_new(
) {
	ploy_Value out;
	memset(&out, 0, sizeof(ploy_Value));
	return out;
}

ploy_Error ploy_Value_resolve(
	ploy_Heap* const heap,
	ploy_Value value,
	ploy_Value* const out_result
) {
	ploy_Error error = PLOY_ERROR_NONE;
	while (!error)
	{
		// If the argument is an expression
		if (value.type == PLOY_VALUE_EXPR)
		{
			error = ploy_invoke(heap, value.list, &value);
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

size_t ploy_Heap_bookmark(
	ploy_Heap const* const heap
) {
	return heap->offset;
}

void ploy_Heap_restore(
	ploy_Heap* const heap,
	size_t const bookmark
) {
	heap->offset = bookmark;
}

struct ploy_Cell {
	ploy_Value value;
	ploy_Cell const* next;
};

/* Ploy cells are at least at aligned as a float, and at most as aligned as a pointer. */
static size_t const PLOY_CELL_ALIGNMENT = sizeof(float) > sizeof(void*) ? sizeof(float) : sizeof(void*);

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
	ploy_Heap* const heap,
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
* Syntax functions
*
*/

ploy_Error ploy_begin(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	if (!arg_list)
	{
		*out_result = ploy_Value_new();
		return PLOY_ERROR_NONE;
	}

	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_Error const error = ploy_Value_resolve(heap, arg_list->value, out_result);
		if (error)
		{
			return error;
		}
	}

	return PLOY_ERROR_NONE;
}

static ploy_Error ploy_if(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	if (!arg_list || !arg_list->next)
	{
		return PLOY_ERROR_TOO_FEW_ARGUMENTS;
	}
	if (arg_list->next->next && arg_list->next->next->next)
	{
		return PLOY_ERROR_TOO_MANY_ARGUMENTS;
	}

	// Evaluate the first argument to a boolean
	ploy_Value predicate;
	ploy_Error const error = ploy_Value_resolve(heap, arg_list->value, &predicate);
	if (error)
	{
		return error;
	}
	arg_list = arg_list->next;

	if (predicate.type != PLOY_VALUE_BOOL)
	{
		return PLOY_ERROR_INVALID_TYPE;
	}

	if (predicate.i32)
	{
		// If the expression is true, run the true branch
		return ploy_Value_resolve(heap, arg_list->value, out_result);
	}
	else if (arg_list->next)
	{
		// If a false branch was given, run that
		return ploy_Value_resolve(heap, arg_list->next->value, out_result);
	}

	// Otherwise, just return default value
	*out_result = ploy_Value_new();
	return PLOY_ERROR_NONE;
}

/*
 *
 * Basic math functions
 *
 */

static ploy_Error ploy_add(
    ploy_Heap* const heap,
    ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	out_result->type = PLOY_VALUE_I32;
	out_result->i32 = 0;

    // Do the addition
	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_Value value;
		ploy_Error const error = ploy_Value_resolve(heap, arg_list->value, &value);
		if (error)
		{
			return error;
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
			return PLOY_ERROR_INVALID_TYPE;
		}
	}

	return PLOY_ERROR_NONE;
}

static ploy_Error ploy_sub(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	if (!arg_list)
	{
		return PLOY_ERROR_TOO_FEW_ARGUMENTS;
	}

	ploy_Value value;
	ploy_Error error = ploy_Value_resolve(heap, arg_list->value, &value);
	if (error)
	{
		return error;
	}

	// Check first argument type
	if (value.type != PLOY_VALUE_I32 && value.type != PLOY_VALUE_F32)
	{
		return PLOY_ERROR_INVALID_TYPE;
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

		return PLOY_ERROR_NONE;
	}

	// Do the subtraction
	for (arg_list = arg_list->next; arg_list != NULL; arg_list = arg_list->next)
	{
		// Evaluate expression, if given
		error = ploy_Value_resolve(heap, arg_list->value, &value);
		if (error)
		{
			return error;
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
			return PLOY_ERROR_INVALID_TYPE;
		}
	}

	return PLOY_ERROR_NONE;
}

/*
 *
 * Utility functions.
 *
 */

static ploy_Error ploy_print(
	ploy_Heap* heap,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	for (; arg_list != NULL; arg_list = arg_list->next)
	{
		ploy_Value value;
		ploy_Error const error = ploy_Value_resolve(heap, arg_list->value, &value);
		if (error)
		{
			return error;
		}

		switch (value.type)
		{
		case PLOY_VALUE_LIST:
			printf("'( ");
			ploy_print(heap, value.list, out_result);
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
	return PLOY_ERROR_NONE;
}

static ploy_Error ploy_compile(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* const out_result
) {
	if (!arg_list)
	{
		return PLOY_ERROR_TOO_FEW_ARGUMENTS;
	}
	if (arg_list->next)
	{
		return PLOY_ERROR_TOO_MANY_ARGUMENTS;
	}

	ploy_Value source;
	ploy_Error const error = ploy_Value_resolve(heap, arg_list->value, &source);
	if (error)
	{
		return error;
	}

	if (source.type != PLOY_VALUE_STR)
	{
		return PLOY_ERROR_INVALID_TYPE;
	}

	return ploy_compile_str(heap, source.str, out_result);
}

ploy_Error ploy_eval(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	if (!arg_list)
	{
		return PLOY_ERROR_TOO_FEW_ARGUMENTS;
	}
	if (arg_list->next)
	{
		return PLOY_ERROR_TOO_MANY_ARGUMENTS;
	}

	// Evaluate the first argument to a list
	ploy_Value list;
	ploy_Error error = ploy_Value_resolve(heap, arg_list->value, &list);
	if (error)
	{
		return error;
	}
	if (list.type != PLOY_VALUE_LIST)
	{
		return PLOY_ERROR_INVALID_TYPE;
	}

	return ploy_invoke(heap, list.list, out_result);
}

ploy_Error ploy_invoke(
	ploy_Heap* const heap,
	ploy_Cell const* arg_list,
	ploy_Value* out_result
) {
	if (!arg_list)
	{
		*out_result = ploy_Value_new();
		return PLOY_ERROR_NONE;
	}

	// Evaluate the first argument to a procedure-ish thing
	ploy_Value proc;
	ploy_Error error = ploy_Value_resolve(heap, arg_list->value, &proc);
	if (error)
	{
		return error;
	}

	// Procedure thing may be a syntax object, builtin function, or user function
	if (proc.type == PLOY_VALUE_SYNTAX)
	{
		switch (proc.i32)
		{
		case PLOY_SYNTAX_BEGIN:
			return ploy_begin(heap, arg_list->next, out_result);

		case PLOY_SYNTAX_IF:
			return ploy_if(heap, arg_list->next, out_result);
		}
	}

	if (proc.type == PLOY_VALUE_FUNCTION_BUILTIN)
	{
		switch (proc.i32)
		{
			/* Basic math functions */
		case PLOY_BUILTIN_ADD:
			return ploy_add(heap, arg_list->next, out_result);

		case PLOY_BUILTIN_SUB:
			return ploy_sub(heap, arg_list->next, out_result);

			/* Utility functions */
		case PLOY_BUILTIN_PRINT:
			return ploy_print(heap, arg_list->next, out_result);

		case PLOY_BUILTIN_COMPILE:
			return ploy_compile(heap, arg_list->next, out_result);

		case PLOY_BUILTIN_EVAL:
			return ploy_eval(heap, arg_list->next, out_result);
		}
	}

	if (proc.type == PLOY_VALUE_FUNCTION_USER)
	{
		// TODO: This should resolve remaining arguments first
		return proc.function->invoke_fn(proc.function->user_data, heap, arg_list->next, out_result);
	}

	return PLOY_ERROR_INVALID_TYPE;
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

static ploy_Error ploy_next_token(
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
		return PLOY_ERROR_NONE;
	}

	// Check for begin
	if (*source_str == '(')
	{
		*out_token_len = 1;
		*out_token_type = PLOY_TOKEN_BEGIN;
		return PLOY_ERROR_NONE;
	}

	// Check for end
	if (*source_str == ')')
	{
		*out_token_len = 1;
		*out_token_type = PLOY_TOKEN_END;
		return PLOY_ERROR_NONE;
	}

	// Check for quote
	if (*source_str == '\'')
	{
		*out_token_len = 1;
		*out_token_len = PLOY_TOKEN_QUOTE;
		return PLOY_ERROR_NONE;
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

			// Decimal point must be followed by at least one more digit
			if (!isdigit(*token_end))
			{
				*out_token_len = 0;
				*out_token_type = PLOY_TOKEN_NONE;
				return PLOY_ERROR_INVALID_SYNTAX;
			}

			// Skip remaining digits
			while (isdigit(*token_end))
			{
				token_end += 1;
			}

			*out_token_type = PLOY_TOKEN_F32;
		}

		// Only whitespace, opening parenthesis, closing parenthesis, or the null terminator may follow a number
		if (!isspace(*token_end) && *token_end != '(' && *token_end != ')' && *token_end != 0)
		{
			*out_token_len = 0;
			*out_token_type = PLOY_TOKEN_NONE;
			return PLOY_ERROR_INVALID_SYNTAX;
		}

		*out_token_len = token_end - source_str;
		return PLOY_ERROR_NONE;
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
			return PLOY_ERROR_INVALID_SYNTAX;
		}

		*out_token_len = token_end - source_str + 1;
		*out_token_type = PLOY_TOKEN_STRING;
		return PLOY_ERROR_NONE;
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
		return PLOY_ERROR_NONE;
	}

	// An error must have happened somewhere
	*out_token_len = 0;
	*out_token_type = PLOY_TOKEN_NONE;
	return PLOY_ERROR_INVALID_SYNTAX;
}

static ploy_Error ploy_parse_symbol(
	ploy_Heap* const heap,
	char const* const symbol_str,
	size_t const symbol_len,
	ploy_Value* const out_result
) {
	// Check syntax identifiers
	out_result->type = PLOY_VALUE_SYNTAX;
	if (symbol_len == 5 && !strncmp(symbol_str, "begin", 5))
	{
		out_result->i32 = PLOY_SYNTAX_BEGIN;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 2 && !strncmp(symbol_str, "if", 2))
	{
		out_result->i32 = PLOY_SYNTAX_IF;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 4 && !strncmp(symbol_str, "cond", 4))
	{
		out_result->i32 = PLOY_SYNTAX_COND;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 4 && !strncmp(symbol_str, "else", 4))
	{
		out_result->i32 = PLOY_SYNTAX_ELSE;
		return PLOY_ERROR_NONE;
	}

	// Check builtin functions
	out_result->type = PLOY_VALUE_FUNCTION_BUILTIN;
	if (symbol_len == 1 && *symbol_str == '+')
	{
		out_result->i32 = PLOY_BUILTIN_ADD;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 1 && *symbol_str == '-')
	{
		out_result->i32 = PLOY_BUILTIN_SUB;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 5 && !strncmp(symbol_str, "print", 5))
	{
		out_result->i32 = PLOY_BUILTIN_PRINT;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 7 && !strncmp(symbol_str, "compile", 7))
	{
		out_result->i32 = PLOY_BUILTIN_COMPILE;
		return PLOY_ERROR_NONE;
	}
	if (symbol_len == 4 && !strncmp(symbol_str, "eval", 4))
	{
		out_result->i32 = PLOY_BUILTIN_EVAL;
		return PLOY_ERROR_NONE;
	}

	// Must be a user symbol, copy the string
	out_result->type = PLOY_VALUE_SYMBOL;
	out_result->str = ploy_String_copy(heap, symbol_str, symbol_len);
	if (!out_result->str)
	{
		return PLOY_ERROR_OUT_OF_MEMORY;
	}

	return PLOY_ERROR_NONE;
}

static ploy_Error ploy_compile_recursive(
	ploy_Heap* const heap,
	char const** io_source_str,
	size_t* out_token_len,
	ploy_TokenType* out_token_type,
	ploy_Cell const** out_list
) {
	// Get the next token
	ploy_Error error = ploy_next_token(io_source_str, out_token_len, out_token_type);
	if (error)
	{
		return error;
	}

	while (*out_token_type != PLOY_TOKEN_NONE && *out_token_type != PLOY_TOKEN_END)
	{
		// Allocate a new cell for whatever we're looking at
		ploy_Cell* const cell = ploy_Cell_new(heap);
		if (!cell)
		{
			return PLOY_ERROR_OUT_OF_MEMORY;
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
			cell->value.str = ploy_String_copy(heap, *io_source_str + 1, *out_token_len - 2);
			*io_source_str += *out_token_len;

			// Make sure the string was sucessfully allocated
			if (!cell->value.str)
			{
				return PLOY_ERROR_OUT_OF_MEMORY;
			}
			break;

		case PLOY_TOKEN_BEGIN:
			*io_source_str += *out_token_len;
			cell->value.type = PLOY_VALUE_EXPR;
			error = ploy_compile_recursive(heap, io_source_str, out_token_len, out_token_type, &cell->value.list);
			*io_source_str += *out_token_len;

			if (error)
			{
				return error;
			}

			// A 'begin' token MUST be ulitmately terminated by an 'end' token
			if (*out_token_type != PLOY_TOKEN_END)
			{
				return PLOY_ERROR_INVALID_SYNTAX;
			}
			break;

		case PLOY_TOKEN_QUOTE:
			// Get the next token
			*io_source_str += *out_token_len;
			error = ploy_next_token(io_source_str, out_token_len, out_token_type);
			if (error)
			{
				return error;
			}

			if (*out_token_type == PLOY_TOKEN_SYMBOL)
			{
				cell->value.type = PLOY_VALUE_QSYMBOL;
				cell->value.str = ploy_String_copy(heap, *io_source_str, *out_token_len);
				*io_source_str += *out_token_len;

				// Make sure the string was successfully allocated
				if (!cell->value.str)
				{
					return PLOY_ERROR_OUT_OF_MEMORY;
				}
			}
			else if (*out_token_type == PLOY_TOKEN_BEGIN)
			{
				*io_source_str += *out_token_len;
				cell->value.type = PLOY_VALUE_LIST;
				error = ploy_compile_recursive(heap, io_source_str, out_token_len, out_token_type, &cell->value.list);
				if (error)
				{
					return error;
				}

				// Make sure the begin was matched with an end
				if (*out_token_type != PLOY_TOKEN_END)
				{
					return PLOY_ERROR_INVALID_SYNTAX;
				}
			}
			break;

		case PLOY_TOKEN_SYMBOL:
			error = ploy_parse_symbol(heap, *io_source_str, *out_token_len, &cell->value);
			*io_source_str += *out_token_len;
			if (error)
			{
				return error;
			}
			break;

		default:
			return PLOY_ERROR_UNKNOWN;
		}

		error = ploy_next_token(io_source_str, out_token_len, out_token_type);
		if (error)
		{
			return error;
		}
	}

	*out_list = NULL;
	return PLOY_ERROR_NONE;
}

ploy_Error ploy_compile_str(
	ploy_Heap* const heap,
	char const* source_str,
	ploy_Value* const out_result
) {
	out_result->type = PLOY_VALUE_LIST;
	size_t token_len;
	ploy_TokenType token_type;

	// Recursively compile the string
	ploy_Error const error = ploy_compile_recursive(heap, &source_str, &token_len, &token_type, &out_result->list);
	if (error)
	{
		return error;
	}

	// Last token must have been NONE
	if (token_type != PLOY_TOKEN_NONE)
	{
		return PLOY_ERROR_INVALID_SYNTAX;
	}

	return PLOY_ERROR_NONE;
}

#endif
