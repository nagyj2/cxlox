#include <stdlib.h>

#include "memory.h"

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
