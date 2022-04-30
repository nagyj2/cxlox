#include <stdlib.h>
#include <stdio.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include "debug.h"
#endif

#define GC_HEAP_GROWTH_FACTOR 2

/* 
 ~ When adding new objects, modify:
 ~	blackenObject()
 ~	freeObject()
 ~	allocateObject()
 */

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize) {
	vm->bytesAllocated += newSize - oldSize; // Shift allocated bytes by the difference
	// printf("%zu allocated, %zu next\n", vm->bytesAllocated, vm->nextGC);
	if (newSize > oldSize) {
		if (vm->bytesAllocated > vm->nextGC) {
			collectGarbage(vm);
		} else {
#ifdef DEBUG_STRESS_GC
		// Always trigger  on allocation if forcing stress GC
		collectGarbage(vm);
#endif
		}
	}
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
static void freeObject(VM* vm, Obj* object) {
#ifdef DEBUG_LOG_GC
	printf("%p freed ", (void*) object);
	switch (object->type) {
		case OBJ_STRING: 				printf("string '"); break;
		case OBJ_FUNCTION: 			printf("function '"); break;
		case OBJ_CLOSURE: 			printf("closure '"); break;
		case OBJ_NATIVE: 				printf("native '"); break;
		case OBJ_UPVALUE: 			printf("upvalue '"); break;
		case OBJ_CLASS: 				printf("class '"); break;
		case OBJ_INSTANCE: 			printf("instance '"); break;
		case OBJ_BOUND_METHOD:	printf("bound method '"); break;
		case OBJ_LIST:					printf("list '"); break;
		case OBJ_MODULE:				printf("module '"); break;
		default: 								printf("unknown '"); break;
	}
	printObject(OBJ_VAL(object));
	printf("'\n");
#endif
	
	switch (object->type) {
		case OBJ_STRING: {
			ObjString* string = (ObjString*) object;
			FREE_ARRAY(vm, char, string->chars, string->length + 1);
			FREE(vm, ObjString, object);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* function = (ObjFunction*) object;
			freeChunk(vm, &function->chunk);
			FREE(vm, ObjFunction, object);
			break;
		}
		case OBJ_NATIVE: {
			FREE(vm, ObjNative, object);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*) object;
			FREE_ARRAY(vm, ObjUpvalue*, closure->upvalues, closure->upvalueCount);
			// doesnt own the function, so it isnt released
			FREE(vm, ObjClosure, object);
			break;
		}
		case OBJ_UPVALUE: {
			// doesnt own the variable, so it isnt released
			FREE(vm, ObjUpvalue, object);
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*) object;
			freeTable(vm, &klass->methods);
			FREE(vm, ObjClass, object);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*) object;
			freeTable(vm, &instance->fields);
			FREE(vm, ObjInstance, object);
			break;
		}
		case OBJ_BOUND_METHOD: {
			// ObjBoundMethod* boundMethod = (ObjBoundMethod*) object;
			// Does not own the method or instance, so it doesn't free them
			FREE(vm, ObjBoundMethod, object);
			break;
		}
		case OBJ_LIST: {
			ObjList* list = (ObjList*) object;
			freeValueArray(vm, &list->entries); // fix owns entries? -> No aliasing
			FREE(vm, ObjList, object);
			break;
		}
		case OBJ_MODULE: {
			ObjModule* module = (ObjModule*) object;
			freeTable(vm, &module->values); // module owns its values
			break;
		}
	}
}

void freeObjects(VM* vm) {
	Obj* object = vm->objects;
	while (object != NULL) {
		Obj* next = object->next;
		freeObject(vm, object);
		object = next;
	}
	
	// Free the GC's stack manually
	free(vm->grayStack);
}

void markObject(VM* vm, Obj* object) {
	if (object == NULL)
		return;
	if (object->isMarked)
		return;

#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void*) object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	object->isMarked = true;

	if (vm->grayCapacity < vm->grayCount + 1) {
		vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
		// Use system realloc to prevent GC from managing the memory
		vm->grayStack = (Obj**) realloc(vm->grayStack, sizeof(Obj*) * vm->grayCapacity);

		if (vm->grayStack == NULL) {
			free(vm->rainyDay);
			vm->usedRainyDay = true;
			vm->rainyDay = NULL;
			vm->grayStack = (Obj**) realloc(vm->grayStack, sizeof(Obj*) * vm->grayCapacity);

			if (vm->grayStack == NULL) {
				printf("Could not allocate memory for gray stack.\n");
				exit(1);
			}
		}
	}

	vm->grayStack[vm->grayCount++] = object;
}

void markValue(VM* vm, Value value) {
	if (IS_OBJ(value))
		markObject(vm, AS_OBJ(value));
}

/** Mark all elements in an array as reachable.
 * @param[out] array The array to mark.
 */
static void markArray(VM* vm, ValueArray* array) {
	for (int i = 0; i < array->count; i++) {
		markValue(vm, array->values[i]);
	}
}

/** Marks all root objects as reachable.
 */
static void markRoots(VM* vm) {
	// Mark elements of the stack
	for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
		markValue(vm, *slot);
	}

	// Mark call frame stack
	for (int i = 0; i < vm->frameCount; i++) {
		markObject(vm, (Obj*) vm->frames[i].closure);
	}

	// Mark open upvalues
	for (ObjUpvalue* upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
		markObject(vm, (Obj*) upvalue);
	}

	// Mark globals
	markTable(vm, &vm->globals);
	markTable(vm, &vm->constants);
	markTable(vm, &vm->modules);

	// Mark compiling functions
	markCompilerRoots(vm);

	// Mark special VM strings
	markObject(vm, (Obj*) vm->initString);
}

/** Append all reachable objects from a given object to the gray stack. 
 * @param[in] object The object to mark.
 */
static void blackenObject(VM* vm, Obj* object) {
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void*) object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif
	
	switch (object->type) {
		case OBJ_UPVALUE:
			markValue(vm, ((ObjUpvalue*) object)->closed); // Element is either on the stack or hoisted. Ensure we find the hoisted value
			break;
		case OBJ_FUNCTION: {
			ObjFunction* function = (ObjFunction*) object;
			markObject(vm, (Obj*) function->name); // Mark the name
			markArray(vm, &function->chunk.constants); // Mark all stored constants
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*) object;
			markObject(vm, (Obj*) closure->function); // Mark the function
			for (int i = 0; i < closure->upvalueCount; i++) {
				markObject(vm, (Obj*) closure->upvalues[i]); // Mark each upvalue it maintains
			}
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*) object;
			markObject(vm, (Obj*) klass->name); // Mark the name of the class
			markTable(vm, &klass->methods); // Mark all methods
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*) object;
			markObject(vm, (Obj*) instance->klass); // Mark the class
			markTable(vm, &instance->fields); // Mark all fields
			break;
		}
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod* bound = (ObjBoundMethod*) object;
			markValue(vm, bound->receiver);
			markObject(vm, (Obj*) bound->method);
			break;
		}
		case OBJ_LIST: {
			ObjList* list = (ObjList*) object;
			markArray(vm, &list->entries);
			break;
		}
		case OBJ_MODULE: {
			ObjModule* module = (ObjModule*) object;
			markObject(vm, (Obj*) module->name);
			markObject(vm, (Obj*) module->path);
			markTable(vm, &module->values);
			break;
		}
		case OBJ_NATIVE:
		case OBJ_STRING:
			break;
	}
}

/** Travel through all gray objects and mark all reachable objects from them.
 */
static void traceReferences(VM* vm) {
	while (vm->grayCount > 0) {
		Obj* object = vm->grayStack[--vm->grayCount];
		blackenObject(vm, object);
	}
}

/** Removes all white objects from the heap.
 * @details
 * The objects referenced in Lox can be accessed via a linked list, so we can simply traverse it and remove
 * all elements which have not been marked.
 */
static void sweep(VM* vm) {
	Obj* previous = NULL;
	Obj* object = vm->objects;
	while (object != NULL) {
		if (object->isMarked) {
			object->isMarked = false; // Reset mark for next GC pass
			previous = object;
			object = object->next;
		} else {
			Obj* unreached = object;
			object = object->next;
			if (previous != NULL) {
				previous->next = object;
			} else {
				vm->objects = object;
			}
			freeObject(vm, unreached);
		}
	}
}

void collectGarbage(VM* vm) {
#ifdef DEBUG_LOG_GC
	printf("-- GC begin --\n");
	size_t before = vm->bytesAllocated;
#endif

	// Mark inital gray objects
	markRoots(vm);
	// Traverse the memory graph, marking elements until all possible references have been visited
	traceReferences(vm);
	// We cannot treat the table as roots b/c then all strings would ALWAYS stay around
	// While all objects have been marked as black or white, remove all white strings
	tableRemoveWhite(vm, &vm->strings);
	// Sweep + remove unmarked objects
	sweep(vm);

	vm->nextGC = vm->bytesAllocated * GC_HEAP_GROWTH_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- GC end --\n");
	printf("   Freed %zu bytes (from %zu to %zu) next at %zu\n", before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
	
	// Reallocate rainy day if used
	if (vm->usedRainyDay) {
		vm->rainyDay = malloc(RAINY_DAY_MEMORY);
		vm->usedRainyDay = false;
	#ifdef DEBUG_LOG_GC
			printf("   Rainy day reallocated: %p\n", vm->rainyDay);
	#endif
		if (vm->rainyDay == NULL) {
			printf("Failed to allocate backup memory.\n");
			exit(1);
		}
	}
}
