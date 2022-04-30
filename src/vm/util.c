#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "util.h"
#include "vm.h"
#include "memory.h"

char* readFile(VM* vm, const char* path) {
	FILE* file = fopen(path, "rb"); // Open path in read binary mode
	if (file == NULL) {
		fprintf(stderr, "Could not open file '%s'.", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END); // Go to the end of the file
	size_t fileSize = ftell(file); // Get file size in bytes
	rewind(file);

	char* buffer = (char*) malloc(fileSize + 1); // Allocate space for source + 1
	if (buffer == NULL) {
		fprintf(stderr, "Not enough memory to read '%s'.", path);
		exit(74);
	}
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file); // Place source into memory
	if (bytesRead < fileSize) {
		fprintf(stderr, "Could not read file '%s'.", path);
		exit(74);
	}
	buffer[bytesRead] = '\0'; // Append end marker

	fclose(file);
	return buffer;
}

ObjString *dirname(VM* vm, char* path, int len) {
    if (!len) {
        return copyString(vm, ".", 1);
    }

    char *sep = path + len;

    /* trailing slashes */
    while (sep != path) {
        if (0 == (*sep == DIR_SEPARATOR))
            break;
        sep--;
    }

    /* first found */
    while (sep != path) {
        if ((*sep) == DIR_SEPARATOR)
            break;
        sep--;
    }

    /* trim again */
    while (sep != path) {
        if (0 == (*sep == DIR_SEPARATOR))
            break;
        sep--;
    }

    if (sep == path && !(*sep == DIR_SEPARATOR)) {
        return copyString(vm, ".", 1);
    }

    len = sep - path + 1;

    return copyString(vm, path, len);
}

bool resolvePath(char *directory, const char *path, char *ret) {
    char buf[PATH_MAX];
    if (*path == DIR_SEPARATOR)
        snprintf (buf, PATH_MAX, "%s", path);
    else
        snprintf(buf, PATH_MAX, "%s%c%s", directory, DIR_SEPARATOR, path);

    if (realpath(buf, ret) == NULL) {
        return false;
    }

    return true;
}

ObjString *getDirectory(VM* vm, const char* source) {
    // Slight workaround to ensure only .du files are the ones
    // attempted to be found.
    int len = strlen(source);
    if (vm->isREPL || len < 4 || source[len - 3] != '.') {
        source = "";
    }

    char res[PATH_MAX];
    if (!resolvePath(".", source, res)) {
        runtimeError(vm, "Unable to resolve path '%s'", source);
        exit(1);
    }

    if (vm->isREPL) {
        return copyString(vm, res, strlen(res));
    }

    return dirname(vm, res, strlen(res));
}
