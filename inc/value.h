#ifndef clox_value_h
#define clox_value_h

#include <string.h> // memcpy for NAN_BOXING
#include <math.h> // floorf

#include "common.h"

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t) 0x8000000000000000)
#define QNAN 		((uint64_t) 0x7ffc000000000000)
// Use last 2 bits of the mantissa to store the tags
#define TAG_EMPTY 0	// 00.
#define TAG_NIL 1 	// 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE 3 	// 11.

/* Use a single 64 bit space to store any type of value (Value either is float, bool, nil or pointer to Obj) */
typedef uint64_t Value;

#define FALSE_VAL ((Value) (uint64_t) (QNAN | TAG_FALSE))
#define TRUE_VAL ((Value) (uint64_t) (QNAN | TAG_TRUE))
#define NIL_VAL ((Value) (uint64_t) (QNAN | TAG_NIL)) // Add tag to QNAN
#define EMPTY_VAL ((Value) (uint64_t) (QNAN | TAG_EMPTY))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) \
	(Value) (SIGN_BIT | QNAN | (uint64_t) (uintptr_t) (obj))

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)

#define AS_BOOL(value) ((value) == TRUE_VAL) // Of all Values, only true is true
#define AS_NUMBER(value) valueToNum(value)
#define AS_INTEGER(value) ((int) valueToNum(value))
#define AS_OBJ(value) \
	((Obj*) (uintptr_t) ((value) & ~(SIGN_BIT | QNAN))) // Removes sign and qnan bits from value to leave pointer

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL) // Set last bit to 1: false -> true, true -> true, num -> ?, obj -> ?
#define IS_NIL(value) ((value) == NIL_VAL)
#define IS_EMPTY(value) ((value) == EMPTY_VAL)
// All non-numbers will have the quient nan form (all exponent bits + first mantissa bit set), so we check for that
#define IS_NUMBER(value) (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
	(((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)) // Need to check qnan bits too b/c negative numbers will have sign bit set

typedef struct _Obj Obj;
typedef struct _ObjString ObjString;

static inline Value numToValue(double num) {
	Value value;
	memcpy(&value, &num, sizeof(double));
	return value;
}

static inline double valueToNum(Value value) {
	double num;
	memcpy(&num, &value, sizeof(Value));
	return num;
}

#else

/* The data types which Value can take within the COMPILER (user types aren't included). */
typedef enum {
	VAL_BOOL,
	VAL_NIL,
	VAL_NUMBER,
	VAL_OBJ,
	VAL_EMPTY,
} ValueType;

/* The type what lox uses to represent literals and values in bytecode. */
typedef struct {
	ValueType type; 	//* Type of the current value.
	union {
		bool boolean; 	//* Value's Boolean value. Only valid when type is VAL_BOOL.
		double number;	//* Value's double value. Only valid when type is VAL_NUMBER.
		Obj* obj;				//* Pointer to a heap allocated value. Only valid when type is VAL_OBJ.
	} as;							//* Value's union type.
} Value;

// Convert a statically typed C value to a dynamically typed lox value.
#define BOOL_VAL(value) 	((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL 					((Value){VAL_NIL, {.number = 0}})
#define EMPTY_VAL 				((Value){VAL_EMPTY, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) 	((Value){VAL_OBJ, {.obj = (Obj*) object}})

// Shorthands for boolean constants
#define TRUE_VAL 					BOOL_VAL(true)
#define FALSE_VAL 				BOOL_VAL(false)

// Convert a dynamically typed lox value to a statically typed C value. Only safe when the lox type is known.
#define AS_BOOL(value) 		((value).as.boolean)
#define AS_NUMBER(value) 	((value).as.number)
#define AS_INTEGER(value) ((int) (value).as.number)
#define AS_OBJ(value) 		((value).as.obj)

// Checks whether a lox value has a specific type.
#define IS_BOOL(value) 		((value).type == VAL_BOOL)
#define IS_NIL(value) 		((value).type == VAL_NIL)
#define IS_EMPTY(value) 	((value).type == VAL_EMPTY)
#define IS_NUMBER(value) 	((value).type == VAL_NUMBER)
#define IS_OBJ(value) 		((value).type == VAL_OBJ)

// #define TO_NUMBER(value)	(Value) toNumber(value);
// #define TO_BOOL(value)		(Value) toBoolean(value);

#endif

#define IS_INTEGER(value) (isInt(value))

static inline bool isInt(Value value) {
	return IS_NUMBER(value) && floorf(AS_NUMBER(value)) == AS_NUMBER(value);
}

/* Dynamic array to hold values. Used by chunks to store literals which appear in the bytecode. */
typedef struct {
	int count; 				//* Number of elements within the ValueArray.
	int capacity; 		//* The maximum capacity of the ValueArray.
	Value* values; 		//* Pointer to an array of Values.
} ValueArray;

/** Computes the hash value of a Value.
 * @param[in] value The value to take in.
 * @return uint32_t The hash value of the input.
 */
uint32_t hashValue(Value value);

/** Initialize an empty ValueArray pointer and its corresponding metadata. Allocates memory.
 * @param[out] array The pointer to be initialized.
 */
void initValueArray(ValueArray *array);

/** Writes a value to the last element in the input ValueArray pointer. May enlarge @p array.
 * @param[in,out] array The ValueArray to be written to.
 * @param[in] value The value to write into @p array.
 */
void writeValueArray(VM* vm, ValueArray *array, Value value);

/** Releases the memory held by a ValueArray and all corresponding elements. Also resets metadata and nullifies pointer.
 * @param[out] array The ValueArray to be freed.
 */
void freeValueArray(VM* vm, ValueArray *array);



//~ Lox Semantics

/** Checks whether two values are equal. If the types are not equal, returns false. Otherwise, check the value contents.
 * @param[in] a The first value to check.
 * @param[in] b The second value to check
 * @return true if the two inputs are the same.
 * @return false if the two outputs are not the same.
 */
bool valuesEqual(Value a, Value b);

/** Displays a Value to stdout.
 * @param[in] value The value to be displayed.
 */
void printValue(Value value);



#endif /* clox_value_h */
