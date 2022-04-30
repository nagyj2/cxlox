#ifndef clox_util_h
#define clox_util_h

#include "common.h"

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#define DIR_SEPARATOR '/'

/** Reads the text file contents and returns the raw character text.
 * 
 * @param[in] path 
 * @return char pointer to the source code text.
 */
char* readFile(VM* vm, const char* path);

ObjString* getDirectory(VM* vm, const char* path);

bool resolvePath(char* directory, const char* path, char* ret);

ObjString* dirname(VM* vm, char* path, int len);

#endif /* clox_util_h */
