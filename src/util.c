#include <stdlib.h>
#include <stdio.h>

#include "util.h"

char* readFile(const char* path) {
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
	