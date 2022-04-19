/* How lox values are represented. */

#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

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
#define AS_OBJ(value) 		((value).as.obj)

// Checks whether a lox value has a specific type.
#define IS_BOOL(value) 		((value).type == VAL_BOOL)
#define IS_NIL(value) 		((value).type == VAL_NIL)
#define IS_EMPTY(value) 	((value).type == VAL_EMPTY)
#define IS_NUMBER(value) 	((value).type == VAL_NUMBER)
#define IS_OBJ(value) 		((value).type == VAL_OBJ)

#define TO_NUMBER(value)	(Value) toNumber(value);
#define TO_BOOL(value)		(Value) toBoolean(value);


/* Dynamic array to hold values. Used by chunks to store literals which appear in the bytecode. */
typedef struct {
	int count; 			//* Number of elements within the ValueArray.
	int capacity; 	//* The maximum capacity of the ValueArray.
	Value *values; 	//* Pointer to an array of Values.
} ValueArray;

/** Computes the hash value of a Value.
 * 
 * @param[in] value The value to take in.
 * @return uint32_t The hash value of the input.
 */
uint32_t hashValue(Value value);

/** Initialize an empty ValueArray pointer and its corresponding metadata. Allocates memory.
 * 
 * @param[out] array The pointer to be initialized.
 */
void initValueArray(ValueArray *array);

/** Writes a value to the last element in the input ValueArray pointer. May enlarge @p array.
 * 
 * @param[in,out] array The ValueArray to be written to.
 * @param[in] value The value to write into @p array.
 */
void writeValueArray(ValueArray *array, Value value);

/** Releases the memory held by a ValueArray and all corresponding elements. Also resets metadata and nullifies pointer.
 *
 * @param[out] array The ValueArray to be freed.
 */
void freeValueArray(ValueArray *array);

/** Displays a Value to stdout.
 * 
 * @param[in] value The value to be displayed.
 */
void printValue(Value value);

//~ Lox Semantics

/** Checks whether two values are equal. If the types are not equal, returns false. Otherwise, check the value contents.
 * 
 * @param[in] a The first value to check.
 * @param[in] b The second value to check
 * @return true if the two inputs are the same.
 * @return false if the two outputs are not the same.
 */
bool valuesEqual(Value a, Value b);



#endif /* clox_value_h */
