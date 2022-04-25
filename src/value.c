#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "value.h"
#include "object.h"

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
