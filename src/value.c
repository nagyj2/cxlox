#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "value.h"
#include "object.h"

/** Computes a hash of a double value by modifiying the the input value and then adding the two 32 bit halves.
 * 
 * @param[in] d The double to compute the hash of.
 * @return uint32_t The hash value for the input double.
 */
static uint32_t hashDouble(double d) {
	union BitCast { // Place a 64 bit double in and retrieve 2 uint32_t out
		double d;
		uint32_t part[2];
	};

	union BitCast cast;
	cast.d = (d) +1.3;
	return cast.part[0] + cast.part[1];
}

uint32_t hashValue(Value value) {
	switch (value.type) {
		case VAL_NUMBER:
			return hashDouble(value.as.boolean);
		case VAL_BOOL:
			return AS_BOOL(value) ? 3 : 5;
		case VAL_NIL:
			return 7;
		case VAL_EMPTY:
			return 0;
		case VAL_OBJ:
			return AS_STRING(value)->hash;
	}
}

void initValueArray(ValueArray *array) {
	array->count = 0;
	array->capacity = 0;
	array->values = NULL;
}

void writeValueArray(ValueArray *array, Value value) {
	if (array->capacity < array->count + 1) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

void freeValueArray(ValueArray *array) {
	FREE_ARRAY(Value, array->values, array->capacity);
	initValueArray(array);
}

void printValue(Value value) {
	switch (value.type) {
		case VAL_BOOL :
			printf(AS_BOOL(value) ? "true" : "false");
			break;
		case VAL_NIL:
			printf("nil");
			break;
		case VAL_EMPTY:
			printf("<empty>");
			break;
		case VAL_NUMBER:
			printf("%g", AS_NUMBER(value));
			break;
		case VAL_OBJ:
			printObject(value);
			break;
	}
}

//~ Lox Semantics

bool valuesEqual(Value a, Value b) {
	if (a.type != b.type)
		return false;

	switch (a.type) {
		case VAL_BOOL:
			return AS_BOOL(a) == AS_BOOL(b);
		case VAL_NIL:
		case VAL_EMPTY:
			return true;
		case VAL_NUMBER:
			return AS_NUMBER(a) == AS_NUMBER(b);
		case VAL_OBJ:
			return AS_OBJ(a) == AS_OBJ(b); // b/c of string interning, we can simply see if the references are identical
		default: // IMPOSSIBLE
			return false;
	}
}
