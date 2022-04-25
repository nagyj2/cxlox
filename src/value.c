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
#ifdef NAN_BOXING
	if (IS_NUMBER(value)) {
		return hashDouble(AS_NUMBER(value));
	} else if (IS_BOOL(value)) {
		return AS_BOOL(value) ? 3 : 5;
	} else if (IS_NIL(value)) {
		return 7;
	} else if (IS_EMPTY(value)) {
		return 0;
	} else if (IS_OBJ(value)) {
		// todo: add other hash types for other obj types
		return AS_STRING(value)->hash;
	}
#else
	switch (value.type) {
		case VAL_NUMBER:
			return hashDouble(value.as.number);
		case VAL_BOOL:
			return AS_BOOL(value) ? 3 : 5;
		case VAL_NIL:
			return 7;
		case VAL_EMPTY:
			return 0;
		case VAL_OBJ:
			return AS_STRING(value)->hash;
	}
#endif
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
#ifdef NAN_BOXING
	if (IS_BOOL(value)) {
    printf(AS_BOOL(value) ? "true" : "false");
  } else if (IS_NIL(value)) {
    printf("nil");
  } else if (IS_NUMBER(value)) {
    printf("%g", AS_NUMBER(value));
  } else if (IS_OBJ(value)) {
    printObject(value);
  }
#else
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
#endif
}

//~ Lox Semantics

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
	// singleton true, false and nil have 1 representation, so we check if they are the same
	// objs check for pointer equality and these will be singletons with regards to where they point (in sign_bit | qnan form with the rest of the mantissa rep. the pointer -> same obj, same pointer)
	// For numbers, we need to ensure nan != nan (`var nan = 0/0; print nan == nan;`). To avoid, simply check for both equal to numbers and then convert as needed
	if (IS_NUMBER(a) && IS_NUMBER(b)) {
		return AS_NUMBER(a) == AS_NUMBER(b);
	}
	return a == b;
#else
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
#endif
}
