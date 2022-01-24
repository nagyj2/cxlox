/* How lox values are represented. */

#ifndef clox_value_h
#define clox_value_h

#include "common.h"

/* The type what lox uses to represent literals and values in bytecode. */
typedef double Value;

/* Dynamic array to hold values. Used by chunks to store literals which appear in the bytecode. */
typedef struct {
	int count; //* Number of elements within the ValueArray.
	int capacity; //* The maximum capacity of the ValueArray.
	Value *values; //* Pointer to an array of Values.
} ValueArray;

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
 * 
 */
void printValue(Value value);

#endif /* clox_value_h */
