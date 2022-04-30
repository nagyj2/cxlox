#include "optionals/optionals.h"

BuiltinModules modules[] = {
	{"stdlib", &createMathModule, true},
	{NULL, NULL, true}
};

Value importBuiltinModule(VM* vm, int index) {
	return modules[index].module(vm);
}

int findBuiltinModule(char* name, int length, bool* CSource) {
	// Iterate through the modules array and see if a match is found, returning the index if so
	for (int i = 0; modules[i].module != NULL; ++i) {
		if (strncmp(modules[i].name, name, length) == 0) {
			return i;
		}
	}
	return -1;
}
