#include <stdio.h> // fprintf
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

/** Reads the text file contents and returns the raw character text.
 * 
 * @param[in] path 
 * @return char pointer to the source code text.
 */
static char* readFile(const char* path) {
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

/** Start the REPL mode of the interpreter.
 * 
 */
static void repl() {
	char line[1024]; // Stores the input line
	for (;;) {
		printf("> ");

		// If the input is null, return.
		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		interpret(line);
	}
}

/** Runs a lox source file. Dynamically allocates memory.
 * 
 * @param[in] path The path to the source file to be interpreted.
 */
static void runFile(const char* path) {
	char* source = readFile(path);
	InterpretResult result = interpret(source);
	free(source); // b/c readFile allocated memory AND the tokens point to the source, so they all need to be deleted before the source text.

	if (result == INTERPRET_COMPILE_ERROR) exit(65);
	if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}
	

int main(int argc, const char* argv[]) {
	initVM();
	
	if (argc == 1) {
		repl();
	} else if (argc == 2) {
		runFile(argv[1]);
	} else {
		fprintf(stderr, "Usage: clox [path]\n");
		exit(64);
	}
	
	freeVM();

	return 0;
}
