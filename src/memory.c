#include <stdlib.h>

#include "memory.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
	// If newSize is 0, handle free case.
	if (newSize == 0) {
		free(pointer);
		return NULL;
	}

	// Otherwise, reallocate. realloc will deal with other 3 cases.
	// really is same as malloc for oldSize = 0. It even does the data copy over for us.
	void *result = realloc(pointer, newSize);
	if (result == NULL) exit(1); // Handle allocation failure.
	return result;
}

/** Frees a specific object's memory.
 * 
 * @param[in,out] object The object to free.
 */
static void freeObject(Obj* object) {
	switch (object->type) {
		case OBJ_STRING: {
			ObjString* string = (ObjString*) object;
			FREE_ARRAY(char, string->chars, string->length + 1);
			FREE(ObjString, object);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* function = (ObjFunction*) object;
			freeChunk(&function->chunk);
			FREE(ObjFunction, object);
			break;
		}
		case OBJ_NATIVE: {
			FREE(ObjNative, object);
			break;
		}
	}
}

void freeObjects() {
	Obj* object = vm.objects;
	while (object != NULL) {
		Obj* next = object->next;
		freeObject(object);
		object = next;
	}
}
