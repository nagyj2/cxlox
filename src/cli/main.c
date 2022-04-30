#include <stdio.h> // fprintf
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"
#include "util.h"

/** Start the REPL mode of the interpreter.
 * 
 */
static void repl(VM* vm) {
	char line[1024]; // Stores the input line
	for (;;) {
		printf("> ");

		// If the input is null, return.
		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		interpret(vm, "repl", line);
	}
}

static char* readFileMain(const char *path) {
	FILE *file = fopen(path, "rb");
	if (file == NULL) {
			return NULL;
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	char *buffer = malloc(sizeof(char) * (fileSize + 1));
	if (buffer == NULL) {
			fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
			exit(74);
	}

	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	if (bytesRead < fileSize) {
			fprintf(stderr, "Could not read file \"%s\".\n", path);
			exit(74);
	}

	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

/** Runs a lox source file. Dynamically allocates memory.
 * 
 * @param[in] path The path to the source file to be interpreted.
 */
static void runFileMain(VM* vm, const char* filename) {
	char* source = readFileMain(filename);

	if (source == NULL) {
		fprintf(stderr, "Could not read file \"%s\".\n", filename);
		exit(74);
	}

	InterpretResult result = interpret(vm, filename, source);
	free(source); // b/c readFile allocated memory AND the tokens point to the source, so they all need to be deleted before the source text.

	if (result == INTERPRET_COMPILE_ERROR) exit(65);
	if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}
	
int main(int argc, const char* argv[]) {

	VM* vm = initVM();

	if (argc == 1) {
		vm->isREPL = true;
		repl(vm);
	} else if (argc == 2) {
		vm->isREPL = false;
		runFileMain(vm, argv[1]);
	} else {
		fprintf(stderr, "Usage: clox [path]\n");
		exit(64);
	}
	
	freeVM(vm);
	return 0;
}
