#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

/* Return 8 if input is lower than 8, else double the input. */
#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

/** Reallocate an array pointer of to a new size. Pretties up our call to reallocate. */
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
	(type*) reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

/* Free an array pointer and its contents. Wrapped reallocate call with proper arguments. */
#define FREE_ARRAY(type, pointer, oldCount) \
	reallocate(pointer, sizeof(type) * (oldCount), 0)

/* Main memory management function for lox. Used for all memory management tasks. */
/* oldSize      newSize      Operation
 * 0            !=0          Allocate new block
 * !=0          0						 Free block
 * !=0          <oldSize     Shrink block
 * !=0          >oldSize     Grow block
 */


/** Main memory management. Operation depends on input. 
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

#endif /* clox_memory_h */
