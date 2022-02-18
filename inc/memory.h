#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"
//* Memory allocated at the beginning of the program and used during GC if the graystack cannot be expanded. Is ~0.5MB.
#define RAINY_DAY_MEMORY (1024 * 512)

/** Allocate memory for a new number of objects. Wrapped reallocate call with proper arguments */
#define ALLOCATE(type, count) \
	(type*) reallocate(NULL, 0, sizeof(type) * (count))

/** Free a pointer of a specific type. Wrapped reallocate call with proper arguments */
#define FREE(type, pointer) \
	reallocate(pointer, sizeof(type), 0)

/* Grows the capacity of a dynamic array. Return 8 if input is lower than 8, else double the input. */
#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

/** Reallocate an array pointer of to a new size. Wrapped reallocate call with proper arguments. */
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
	(type*) reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

/* Free an array pointer and its contents. Wrapped reallocate call with proper arguments. */
#define FREE_ARRAY(type, pointer, oldCount) \
	reallocate(pointer, sizeof(type) * (oldCount), 0)

/** Main memory management, capable of allocating, freeing and reallocating. Operation depends on input.
 *
 * @details
 * If @p oldSize ==0, @p newSize !=0, allocate new memory.
 * If @p oldSize !=0, @p newSize ==0, free pointed to memory. 
 * If @p oldSize ==0, @p newSize < @p oldSize, shrink the pointed to memory. 
 * If @p oldSize ==0, @p newSize > @p oldSize, grow pointed to memory. 
 *
 * @param[in,out] pointer The pointer to be modified.
 * @param[in] oldSize The current size of the pointer.
 * @param[in] newSize The new size of the pointer.
 * @return void* A pointer to the newly allocated memory.
 */
void *reallocate(void *pointer, size_t oldSize, size_t newSize);

/** Frees all allocated object memory.
 * @details
 * Iterates through the object list and frees the allocated memory. Called when the VM is freed.
 */
void freeObjects();

/** Marks an object to be kept by the GC.
 * @param[out] object The object to be marked.
 */
void markObject(Obj* object);

/** Mark an object as reachable to prevent memory deallocation.
 * @details
 * Ignores all values which are stored inside the value struct itself. Only objects are freed.
 * @param[in,out] value The value to mark.
 */
void markValue(Value value);

/** Performs garbage collection on all Lox allocated objects.
 */
void collectGarbage();

#endif /* clox_memory_h */
