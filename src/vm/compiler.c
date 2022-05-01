#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "vm.h"
#include "object.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

// Unamed var counter
char unamedVarCounter = '0';

//~ Writing to chunks

/** The chunk which is currently getting bytecode emitted to it.
 * @return Chunk* pointer to the chunk currently being emitted to.
 */
static Chunk* currentChunk(Compiler* compiler) {
	return &compiler->function->chunk;
}

//~ Error functions

/** Prints an error to stderr. Includes line number information.
 * @param[in] token The token which represents the error.
 * @param[in] message The error message to display.
 */
static void errorAt(Parser *parser, Token* token, const char* message) {
	// Ignore if already in panic mode
	if (parser->panicMode) return;
	parser->hadError = true;
	
	fprintf(stderr, "File '%s' [line %d] Error", parser->module->name->chars, token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else { // Show error's lexeme
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
}

/** Reports an error at the token which was just parsed.
 * @param[in] message The error to display.
 */
static void error(Parser* parser, const char* message) {
	errorAt(parser, &parser->previous, message);
}

/** Reports an error at the token which is currently being parsed.
 * @param[in] message The error to display.
 */
static void errorAtCurrent(Parser* parser, const char* message) {
	errorAt(parser, &parser->current, message);
}

//~ Parsing functions

/** Advances the parser forward to the next token. 
 */
static void advance(Parser* parser) {
	parser->previous = parser->current;

	// B/c errors are through special tokens, wrap in a for loop so multiple error tokens in a row are shown and discarded
	for (;;) {
		parser->current = scanToken(&parser->scanner);
		if (parser->current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser, parser->current.start);
	}
}

/** Consumes the next token if it matches the input type and causes an error if it doesn't.
 * @param[in] type The token type to compare the current token against.
 * @param[in] message Message to display if the token doesn't match.
 */
static void consume(Compiler* compiler, TokenType type, const char* message) {
	if (compiler->parser->current.type == type) {
		advance(compiler->parser);
		return;
	}

	errorAtCurrent(compiler->parser, message);
}

/** Returns whether the current token matches the input token type.
 * @param[in] type The token type to compare the current token against.
 * @return true if the token matches.
 * @return false if the token doesnt match.
 */
static bool check(Compiler* compiler, TokenType type) {
	return compiler->parser->current.type == type;
}

/** Returns whether or not the current token matches the input type.
 * @details
 * If a match is encountered, the token is consumed.
 * @param[in] type The token type to check for
 * @return true if the token was found.
 * @return false if the token was not found.
 */
static bool match(Compiler* compiler, TokenType type) {
	if (!check(compiler, type))
		return false;

	advance(compiler->parser);
	return true;
}

/** Consume input until a synchronization token is found.
 */
static void synchronize(Parser* parser) {
	parser->panicMode = false;

	while (parser->current.type != TOKEN_EOF) {
		if (parser->previous.type == TOKEN_SEMICOLON)
			return;
		switch (parser->current.type) {
			case TOKEN_CLASS:
			case TOKEN_FUN:
			case TOKEN_VAR:
			case TOKEN_FOR:
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_PRINT:
			case TOKEN_RETURN:
				return;
			default:
				; // Nothing
		}

		advance(parser);
	}
}

//~ Bytecode emission

/** Appends a byte to the current chunk.
 * @param[in] byte The byte to append.
 */
static void emitByte(Compiler* compiler, uint8_t byte) {
	writeChunk(compiler->parser->vm, currentChunk(compiler), byte, compiler->parser->previous.line);
}

/** Emits 2 sequential bytes to the current chunk.
 * @param[in] byte1 The first byte to emit.
 * @param[in] byte2 The second byte to emit.
 */
static void emitBytes(Compiler* compiler, uint8_t byte1, uint8_t byte2) {
	emitByte(compiler, byte1);
	emitByte(compiler, byte2);
}

/** Emits an opcode and a 3 byte address in a big endian encoding.
 * @param[in] opcode_long The opcode to emit.
 * @param[in] index The index to emit as 3 bytes
 */
static void emitLongBytes(Compiler* compiler, uint8_t opcode_long, index_t index) {
	emitBytes(compiler, opcode_long, (uint8_t) ((index >> 16) & 0xff));
	emitBytes(compiler, (uint8_t) ((index >> 8) & 0xff), (uint8_t) (index & 0xff));
}

/** Emits 2-4 bytes depending on the size of the index.
 * @param[in] opcode The opcode to emit in case of a short index.
 * @param[in] opcode_long The opcode to emit in case of a long index.
 * @param[in] index The index to emit.
 */
static void emitLongable(Compiler* compiler, opcode_t opcode, opcode_t opcode_long, index_t index) {
	if (index >= CONST_TO_LONG_CONST) {
		emitLongBytes(compiler, opcode_long, index);
	} else {
		emitBytes(compiler, opcode, (uint8_t) index);
	}
}

/** Emit a 3 byte jump stub. Must be patched with a later call to emitJump from the destination position.
 * @param[in] instruction The jump opcode to emit.
 * @return int The position of the jump offset bytes.
 */
static int emitJump(Compiler* compiler, opcode_t instruction) {
	emitByte(compiler, instruction);
	emitBytes(compiler, 0xff, 0xff); // Jump offset placeholder.
	return currentChunk(compiler)->count - 2;
}

/** Emit 3 bytes corresponding to a unconditional jump backwards.
 * @param[in] loopStart The byte offset for the loop to jump back to.
 */
static void emitLoop(Compiler* compiler, int loopStart) {
	emitByte(compiler, OP_LOOP);

	// +2 for the size of the jump address
	int offset = currentChunk(compiler)->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error(compiler->parser, "Loop body too large.");

	emitByte(compiler, (offset >> 8) & 0xff);
	emitByte(compiler, offset & 0xff);
}

/** Replaces the jump stub at given offset bytes back with the current bytecode position.
 * @details
 * Assumes a 16 bit jump stub starts at the code chunk at index @p offset.
 * The jump is written as big endian.
 * @param[in] offset The position of the jump placeholder to replace.
 */
static void patchJump(Compiler* compiler, int offset) {
	// -2 to account for the size of the jump placeholder.
	int jump = currentChunk(compiler)->count - offset - 2; // find the number of bytes to skip

	if (jump > UINT16_MAX) {
		error(compiler->parser, "Too much code to jump over.");
	}

	currentChunk(compiler)->code[offset] = (jump >> 8) & 0xFF; // MSB
	currentChunk(compiler)->code[offset + 1] = jump & 0xFF;
}

/** Patch all going-out-of-scope break statements
 * @param[in] oldBreak The old number of break statements.
 */
static void patchBreaks(Compiler* compiler, int oldBreak) {
	while (compiler->numBreak > oldBreak) {
		// Index jumps from the back of the list
		patchJump(compiler, compiler->recentBreak[--compiler->numBreak]);
	}
}


/** Emits an implied nil constant and a opcode for function return.
 */
static void emitReturn(Compiler* compiler) {
	if (compiler->type == TYPE_INITIALIZER) {
		emitBytes(compiler, OP_GET_LOCAL, 0); // we reserved slot 0 for 'this'. Return this if initializer
	} else {
		emitByte(compiler, OP_NIL); // Implicitly places nil on the stack
	}
	
	// If a return expression is present, it will be the stack top and get returned. Otherwise the just emitted nil will
	emitByte(compiler, OP_RETURN);
}

/** Places a constant into the current chunk's constant pool and returns its index.
 * Used to inline constants indicies into the bytecode
 * @details
 * Causes an error if the number of constants in the chunk exceeds the maximum.
 * @param[in] value The value to write to the constant pool.
 * @return index_t byte representing the index of the constant in the constant pool.
 */
static index_t makeConstant(Compiler* compiler, Value value) {
	index_t constant = addConstant(compiler->parser->vm, currentChunk(compiler), value);
	if (constant > MAX_CONSTANTS_PER_CHUNK) {
		error(compiler->parser, "Too many constants in one chunk.");
		return 0;
	}
	return constant;
}

/** Emits a constant value which is placed on the stack.
 * @param[in] value The value to write.
 */
static void emitConstant(Compiler* compiler, Value value) {
	index_t index = makeConstant(compiler, value);
	emitLongable(compiler, OP_CONSTANT, OP_CONSTANT_LONG, index);
}

//~ Compiler utilities

/** Initializes the state of the compiler and sets it to be the current compiler.
 * @pre Assumes the name of the function has just been consumed if parsing a function body.
 * @param[out] compiler The compiler to initialize.
 * @param[in] type The type of function being compiled.
 */
static void initCompiler(Parser *parser, Compiler* compiler, Compiler* parent, FunctionType type) {
	compiler->parser = parser;
	compiler->enclosing = parent;
	
	compiler->function = NULL; // allocate to prevent GC from seeing uninitialized memory in newFunction() below
	compiler->class = NULL;

	if (parent != NULL) {
		compiler->class = parent->class; // Set the inner most class compiler
	}

	compiler->type = type;
	compiler->localCount = 0; 	// Number of locals in the current scope
	compiler->scopeDepth = 0; 	// Current scope depth for locals
	compiler->recentLoop = -1; 	// The most recent loop position for break/continue
	compiler->numBreak = 0; 		// Number of breaks in the current scope
	compiler->inListing = false; 	// Whether the compiler is currently in a function call. Controls lambda functions

	parser->vm->compiler = compiler; // Link compiler to the parser
	compiler->function = newFunction(parser->vm, parser->module, type);

	switch (type) {
		case TYPE_METHOD:
		case TYPE_INITIALIZER:
		case TYPE_FUNCTION:
			compiler->function->name = copyString(parser->vm, parser->previous.start, parser->previous.length);
			break;
		case TYPE_LAMBDA:
			compiler->function->name = copyString(parser->vm, "lambda", 6);
			break;
		case TYPE_SCRIPT:
			compiler->function->name = NULL;
			break;
	}

	// Reserve a position in the stack for the compiler's use
	Local* local = &compiler->locals[compiler->localCount++];
	local->depth = compiler->scopeDepth; // Save as a top level scoped local. Prevents it from getting removed from endScope
	local->isCaptured = false;
	local->constant = false;
	if (type == TYPE_METHOD || type == TYPE_INITIALIZER) {
		// In a method, "this" refers to the compiling entity
		local->name.start = "this";
		local->name.length = 4;
	} else {
		// Hold the function being compiler. Cannot be referenced so no name
		local->name.start = "";
		local->name.length = 0;
	}
}

/** Marks the end of a compilation.
 * @param[in] appendReturn Whether the parsed function should have a return expression appended to it
 * @return ObjFunction* The completely compiled function.
 */
static ObjFunction* endCompiler(Compiler* compiler, bool appendReturn) {
	if (appendReturn)
		emitReturn(compiler);
	ObjFunction* function = compiler->function; // Return the function we just made

	// If debug is enabled, print the completed bytecode if there was no error
#ifdef DEBUG_PRINT_CODE
	if (!compiler->parser->hadError) {
		disassembleChunk(currentChunk(compiler), function->name != NULL ? function->name->chars : function->module->name->chars);
	}
#endif

	// Capture upvalues in new closure object
	if (compiler->enclosing != NULL) {
		// Emit the constant onto the stack OF THE PARENT COMPILER
		emitLongable(compiler->enclosing, OP_CLOSURE, OP_CLOSURE_LONG, makeConstant(compiler->enclosing, OBJ_VAL(function)));
		// For every upvalue captured, emit its locality and index
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(compiler->enclosing, compiler->upvalues[i].isLocal ? 1 : 0); // Flag on whether the variable is a local or not
			emitByte(compiler->enclosing, compiler->upvalues[i].index); // index of the variable in the stack
		}
		
	}
	
	// todo if string constants, free here
	compiler->parser->vm->compiler = compiler->enclosing; // Restore the previous compiler
	return function;
}

/** Signals the start of a new scope.
 */
static void beginScope(Compiler* compiler) {
	compiler->scopeDepth++;
}

/** Emits bytes to pop a number of of values from the stack. Used for removing locals at the end of a scope.
 * @param[in] locals The number of locals to pop.
 */
static void popLocals(Compiler* compiler, int locals) {
	if (locals == 0)
		return;
	else if (locals == 1)
		emitByte(compiler, OP_POP);
	else
		emitBytes(compiler, OP_POPN, (uint8_t) locals);
}

/** Signals the end of the current scope to the compiler singleton.
 * @details
 * Removes all local variables stored at the current scope.
 */
static void endScope(Compiler* compiler) {
	compiler->scopeDepth--;
	int passedLocals = 0;
	while (compiler->localCount > 0 && compiler->locals[compiler->localCount - 1].depth > compiler->scopeDepth) {
		// If local is captured, emit a special pop instruction
		if (compiler->locals[compiler->localCount - 1].isCaptured) {
			// First remove all locals before the upvalue
			popLocals(compiler, passedLocals);
			passedLocals = 0;

			emitByte(compiler, OP_CLOSE_UPVALUE);
		} else {
			passedLocals++;
		}
		compiler->localCount--;
	}

	// Pop all remaining locals
	popLocals(compiler, passedLocals);
}

/** Triggers end of scope code to discard locals but does change the scope level.
 * @details
 * Used for break statements.
 */
static void prematureEndScope(Compiler* compiler) {
	endScope(compiler);
	compiler->scopeDepth++;
}

//~ Grammar visitor methods

// Forward declares to allow references.
static void declaration(Compiler* compiler);
static void varDeclaration(Compiler* compiler);
static void funDeclaration(Compiler* compiler);
static void classDeclaration(Compiler* compiler);
static void importDeclaration(Compiler* compiler);

static void statement(Compiler* compiler);
static void expressionStatement(Compiler* compiler);
static void printStatement(Compiler* compiler);
static void ifStatement(Compiler* compiler);
static void whileStatement(Compiler* compiler);
static void forStatement(Compiler* compiler);
static void continueStatement(Compiler* compiler);
static void breakStatement(Compiler* compiler);
static void switchStatement(Compiler* compiler);
static void delStatement(Compiler* compiler);
static void returnStatement(Compiler* compiler);
static void block(Compiler* compiler);

static void expression(Compiler* compiler);
static int argumentList(Compiler* compiler);
static void lambda(Compiler* compiler);
static void function(Compiler* compiler, FunctionType type);
static void method(Compiler* compiler);

static ParseRule* getRule(TokenType type);
static void parsePrecidence(Compiler* compiler, Precidence precidence);

//~ Identifier resolution

/** Creates a string constant for an identifier, places it in the constant pool, and returns the index of the constant.
 * @param[in] name The name of the variable to declare.
 * @return index_t 
 */
static index_t identifierConstant(Compiler* compiler, Token* name) {
	// todo If using string constants, add to the table here
	return makeConstant(compiler, OBJ_VAL(copyString(compiler->parser->vm, name->start, name->length)));
}

/** Test if two identifier (string) tokens are equal.
 * @param[in] a The first token to compare.
 * @param[in] b The second token to compare.
 * @return true if the tokens refer to the same string.
 * @return false if the tokens are not the same string.
 */
static bool identifiersEqual(Token* a, Token* b) {
	// Short circuit based on length
	if (a->length != b->length)
		return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

/** Creates a fake token struct from a string.
 * @param[in] text The token to base the token off of.
 * @return Token representing a string.
 */
static Token syntheticToken(Compiler* compiler, const char* text) {
	Token token;
	token.type = TOKEN_IDENTIFIER;
	token.start = text;
	token.length = (int) strlen(text);
	token.line = compiler->parser->previous.line;
	return token;
}

/** Resolves a variable as a local or global.
 * @details
 * If the variable is a local, an integer corresponding to how far down it is on the stack is returned.
 * If the variable is a global, a corresponding value is not found and a sentinel value -1 is returned.
 * @param[in] compiler The compiler to resolve the variable from.
 * @param[in] name The variable to try and resolve.
 * @return int representing the index of the variable in the locals array or a sentinel value of -1.
 */
static int resolveLocal(Compiler* compiler, Token* name, bool inFunction) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local* local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (!inFunction && local->depth == -1) {
				error(compiler->parser, "Cannot read local variable in its own initializer.");
			}
			return i;
		}
	}

	return -1;
}

/** Create a new upvalue in the array and return its index in the upvalue array.
 * @details
 * To prevent the compiler from creating multiple upvalues for the same variable, we iterate through the existing upvalues looking for ours. 
 * If it is there, we return it.
 * @param[in] compiler The compiler which the upvalue will be placed in.
 * @param[in] index The local function offset positiion of the variable.
 * @param[in] isLocal Whether the variable is a local or global variable.
 * @return int representing the index of the position of the upvalue in the compiler.
 */
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal, bool isConstant) {
	int upvalueCount = compiler->function->upvalueCount; // Find the current number of upvalues

	// Check for an existing upvalue. B/c they map by index, we want to verify that the index is the same and the locality.
	// Locality matters because the origin of the upvalue call can refer to different variables
	for (int i = 0; i < upvalueCount; i++) {
		Upvalue* upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) { // ensure we find what we are looking for
			return i; // Return the index in the array
		}
	}

	// Verify there isnt too many upvalues
	if (upvalueCount == UINT8_COUNT) {
		error(compiler->parser, "Too many closure variables in function.");
		return 0;
	}

	// Fill in the new values for (read: create) a new upvalue
	compiler->upvalues[upvalueCount].index = index;
	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].constant = isConstant;
	return compiler->function->upvalueCount++;
}

/** Updates upvalues if the token being resolved is a local or global variable already declared.
 * @details
 * If a variable cannot be resolved as a local variable, we need to check if it is a local variable of an outer scope. If so,
 * we want to identify it as an upvalue so we can track it separately at runtime b/c it may not always be on the stack. To do
 * this, we want to recursively check outer, non global scopes for the variable. This involves checking the local scope and if
 * not found, check the enclosing scope's locals.
 * @param[in] compiler The compiler which is currently being worked on.
 * @param[in] name The variable which is being resolved.
 * @return int representing the 'upvalue index' of the variable. Returns -1 if the variable could not be resolved as an upvalue.
 */
static int resolveUpvalue(Compiler* compiler, Token* name) {
	// If global scope, we cannot possibly have an upvalue (by definition), so return not found
	if (compiler->enclosing == NULL)
		return -1;

	// See if the variable can be resolved locally.
	// Note: we check for locality of the 'current' compiler before calling this function, so this 'resolveLocal' is first called for the enclosing compiler
	int local = resolveLocal(compiler->enclosing, name, true);
	if (local != -1) { // If found as a local, create a local upvalue in the below compiler.
		compiler->enclosing->locals[local].isCaptured = true; // Mark the local as captured. Used to instruct VM how to dispose of it
		// Variable was found in the enclosing compiler, so mark it as local.
		return addUpvalue(compiler, (uint8_t) local, true, compiler->enclosing->locals[local].constant);
	}

	// If it is not local, resolve the upvalue recursively to the top of the call stack
	int upvalue = resolveUpvalue(compiler->enclosing, name);
	if (upvalue != -1) { // If found, create a non-local upvalue to permit an upvalue chain
		// Creates an upvalue. Mark it as non-local because it is NOT in in the closure immediately above current
		// This allows for a upvalue to point to an upvalue
		return addUpvalue(compiler, (uint8_t) upvalue, false, compiler->enclosing->upvalues[upvalue].constant);
	}

	return -1; // Not found in parent's chain
}

/** Retrieves the next available local variable slot and assigns a name and depth.
 * @param[in] name The name to assign to the local variable.
 */
static void addLocal(Compiler* compiler, Token name, bool isConstant) {
	if (compiler->localCount == UINT8_COUNT) {
		error(compiler->parser, "Too many local variables in function.");
		return;
	}
	
	Local* local = &compiler->locals[compiler->localCount++];
	local->name = name;
	local->depth = -1;
	local->isCaptured = false;
	local->constant = isConstant;

	// compiler->localCount++;
}

/** Converts the (simulated) top stack element into a local variable if it isn't global.
 * Uses the last read token as the name. Will cause compile error if name is reused.
 * Adds variable to scope.
 */
static void declareNamedVariable(Compiler* compiler, Token* name, bool isConstant) {
	// Globals are placed into the constant pool, so we don't need to do anything special here
	if (compiler->scopeDepth == 0) {
		return;
	}

	// Token* name = &parser.previous;
	// Ensure local isn't already defined. Start at back and decrease index b/c current compiler scoped items are there
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local* local = &compiler->locals[i]; // Get next local
		// If we walk to a different scope than the one we are interested in, break b/c we are out of the current scope
		if (local->depth != -1 && local->depth < compiler->scopeDepth) {
			break;
		}
		// Currently scoped var. Check names
		if (identifiersEqual(name, &local->name)) {
			error(compiler->parser, "Already a variable with this name in scope.");
		}
	}
	addLocal(compiler, *name, isConstant);
}

/** Declare the last parsed token to be a variable. Uses previous token as the name.
 * @param[in] isConstant Whether the variable should be marked as a constant.
 */
static void declareVariable(Compiler* compiler, bool isConstant) {
	Token* name = &compiler->parser->previous;
	declareNamedVariable(compiler, name, isConstant);
}

/** Declare the top most local variable available for use by removing the sentinel -1 depth.
 * @details
 * If the variable is not global to the current function, its depth is updated.
 */
static void markInitialized(Compiler* compiler) {
	// Do not mark initialization for global functions/ variables
	if (compiler->localCount == 0)
		return;
	compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

/** Emits 2-4 bytes to define a new variable for use. After this point the variable can be referenced.
 * If defining a local variable, it will be marked as initialized by the compiler.
 * If defining a global variable, code will be emitted to define the variable globally but it will need to
 * be initialized separately.
 * @param[in] global The index to the constant pool location of the global variable's name.
 */
static void defineVariable(Compiler* compiler, index_t global, bool isConstant) {
	// If a local, don't put into constant pool
	if (compiler->scopeDepth > 0) {
		markInitialized(compiler);
		return; // locals dont need any new bytes b/c their value is already on top of the stack
	}

	if (isConstant)	emitLongable(compiler, OP_DEFINE_CONST,  OP_DEFINE_CONST_LONG,  global);
	else						emitLongable(compiler, OP_DEFINE_GLOBAL, OP_DEFINE_GLOBAL_LONG, global);
}

/** Parses a variable, creates a local or global and then and returns the index of the variable in the constant pool or stack.
 * @param[in] errorMessage The message to show if the variable name is missing.
 * @return index_t index of the variable in the constant pool
 */
static index_t parseVariable(Compiler* compiler, const char* errorMessage, bool isConstant) {
	consume(compiler, TOKEN_IDENTIFIER, errorMessage);

	// declare the var. If a local, the locals array must be updated. If a global, nothing needs to be done
	declareVariable(compiler, isConstant);
	// If local, var will be placed on stack, so dont give the global location
	if (compiler->scopeDepth > 0)
		return 0;

	return identifierConstant(compiler, &compiler->parser->previous);
}

/** Parses a variable which has just been passed over.
 * @return index_t index of the variable in the constant pool
 */
static index_t parseVariablePast(Compiler* compiler, bool isConstant) {
	// declare the var. If a local, the locals array must be updated. If a global, nothing needs to be done
	declareVariable(compiler, isConstant);
	// If local, var will be placed on stack, so dont give the global location
	if (compiler->scopeDepth > 0)
		return 0;

	return identifierConstant(compiler, &compiler->parser->previous);
}

//~ Grammar evaluation functions

/** Parses a number of arguments and returns the number of arguments parsed.
 * @details
 * Parsed arguments are left on the stack
 */
static int argumentList(Compiler* compiler) {
	int argCount = 0;
	bool oldCallStatus = compiler->inListing; // Track the old status so we can restore it. Used for nested calls
	compiler->inListing = true;
	if (!check(compiler, TOKEN_RIGHT_PAREN)) {
		do {
			// Disallow comma expressions
			parsePrecidence(compiler, PREC_COMMA+1);
			// expression(compiler);
			argCount++;
			if (argCount >= 255) { // b/c using uint8_t exclusively
				error(compiler->parser, "Cannot have more than 255 arguments.");
			}
		} while (match(compiler, TOKEN_COMMA));
	}
	consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')'.");
	compiler->inListing = oldCallStatus;
	return argCount;
}

/** Emits a constant literal to the current chunk.
 * @pre The literal token is in the parser.previous position.
 * @param[in] canAssign unused.
 */
static void literal(Compiler* compiler, bool canAssign) {
	switch (compiler->parser->previous.type) {
		case TOKEN_FALSE:
			emitByte(compiler, OP_FALSE);
			break;
		case TOKEN_TRUE:
			emitByte(compiler, OP_TRUE);
			break;
		case TOKEN_NIL:
			emitByte(compiler, OP_NIL);
			break;
		default:
			error(compiler->parser, "Unknown literal.");
			return;
	}
}

/** Emits a constant number value to the current chunk. 
 *  @pre The number token is in the parser.previous position.
 * @param[in] canAssign unused.
 */
static void number(Compiler* compiler, bool canAssign) {
	// Convert the lexeme from the last token to a number.
	//? How is the end of the lexeme marked
	double value = strtod(compiler->parser->previous.start, NULL);
	emitConstant(compiler, NUMBER_VAL(value));
}

/** Emits a string constant copied from the input source. The string is stored in the constant pool as all Values are.
 * @details
 * The allocated Value is stored in the constant pool, but the actual char array behind the string is heap allocated by C. The constant pool value contains a pointer to 
 * the heap allocated char array. The value in the constant pool is indexed in the same way as numerical and boolean constants.
 * @param[in] canAssign unused.
 */
static void singleString(Compiler* compiler, bool canAssign) {
	emitConstant(compiler, OBJ_VAL(copyString(compiler->parser->vm, compiler->parser->previous.start + 1, compiler->parser->previous.length - 2)));
}

/** Emit an interpolated string constant.
 * @param[in] canAssign unused.
 */
static void doubleString(Compiler* compiler, bool canAssign) {
	// parser.previous.start was redefined by the double string tokenization process. Cast to get rid of const
	// emitConstant(compiler, OBJ_VAL(takeString(compiler->parser->vm, (char*) compiler->parser->previous.start, compiler->parser->previous.length)));
	emitConstant(compiler, OBJ_VAL(copyString(compiler->parser->vm, (char*) compiler->parser->previous.start, compiler->parser->previous.length)));
}

/** Emits bytes to retrieve or set a global variable's value.
 * @details
 * Parses the 'assignment' production in the grammar.
 * @param[in] name The name of the variable to retrieve.
 */
static void namedVariable(Compiler* compiler, Token name, bool canAssign) {
	opcode_t getOp, setOp, getOpLong, setOpLong;
	// Attempt to find local with name. 
	index_t index = resolveLocal(compiler, &name, false); // RETURNS INTEGER; POSITION OF VAR IN LOCALS. IF -1, then it will be converted to index_t below
	
	if (index != -1) {
		getOp = OP_GET_LOCAL; getOpLong = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL; setOpLong = OP_SET_LOCAL;
		if (compiler->locals[index].constant)
			error(compiler->parser, "Cannot assign to a constant.");
	} else if ((index = resolveUpvalue(compiler, &name)) != -1) { // Captured Upvalue
		getOp = OP_GET_UPVALUE; getOpLong = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE; setOpLong = OP_SET_UPVALUE;
		if (compiler->upvalues[index].constant)
			error(compiler->parser, "Cannot assign to a constant.");
	} else {
		index = identifierConstant(compiler, &name);
		getOp = OP_GET_GLOBAL; getOpLong = OP_GET_GLOBAL_LONG;
		setOp = OP_SET_GLOBAL; setOpLong = OP_SET_GLOBAL_LONG;
	}

	// Check if it should be a setter right before we emit the opcode
	if (canAssign && (match(compiler, TOKEN_EQUAL)
									|| match(compiler, TOKEN_MINUS_EQUAL)
									|| match(compiler, TOKEN_PLUS_EQUAL)
									|| match(compiler, TOKEN_STAR_EQUAL)
									|| match(compiler, TOKEN_SLASH_EQUAL))) {
		switch (compiler->parser->previous.type) {
			case TOKEN_MINUS_EQUAL: {
				emitLongable(compiler, getOp, getOpLong, index);
				expression(compiler);
				emitByte(compiler, OP_SUBTRACT);
				break;
			}
			case TOKEN_PLUS_EQUAL: {
				emitLongable(compiler, getOp, getOpLong, index);
				expression(compiler);
				emitByte(compiler, OP_ADD);
				break;
			}
			case TOKEN_STAR_EQUAL: {
				emitLongable(compiler, getOp, getOpLong, index);
				expression(compiler);
				emitByte(compiler, OP_MULTIPLY);
				break;
			}
			case TOKEN_SLASH_EQUAL: {
				emitLongable(compiler, getOp, getOpLong, index);
				expression(compiler);
				emitByte(compiler, OP_DIVIDE);
				break;
			}
			default:
				expression(compiler);
				break;
		}
		emitLongable(compiler, setOp, setOpLong, index);
		
	} else {
		emitLongable(compiler, getOp, getOpLong, index);
	}
}

/** Parses a property get or set.
 * @param[in] canAssign If true, dot indicates an assignment. Otherwise a get is parsed
 */
static void dot(Compiler* compiler, bool canAssign) {
	consume(compiler, TOKEN_IDENTIFIER, "Expected property name after '.'.");
	index_t nameIndex = identifierConstant(compiler, &compiler->parser->previous); // property name

	// Check if an assignment is being parsed and if an assignment can even occur
	if (canAssign && match(compiler, TOKEN_EQUAL)) {
		expression(compiler);
		emitLongable(compiler, OP_SET_PROPERTY, OP_SET_PROPERTY_LONG, nameIndex);
		// emitBytes(OP_SET_PROPERTY, name);
	} else if (match(compiler, TOKEN_LEFT_PAREN)) { // Optimization for method calls: 'x.y(z)'
		int argCount = argumentList(compiler);
		emitLongable(compiler, OP_INVOKE, OP_INVOKE_LONG, nameIndex);
		emitByte(compiler, argCount);
	} else {
		emitLongable(compiler, OP_GET_PROPERTY, OP_GET_PROPERTY_LONG, nameIndex);
	}

	// todo again, copy namedVariable to allow `class.pop += value`
}

/** Parses an infix indexing operation.
 * @pre The initial '[' has already been consumed.
 * @param[in] canAssign unused.
 */
static void subscript(Compiler* compiler, bool canAssign) {
	parsePrecidence(compiler, PREC_COMMA + 1);
	consume(compiler, TOKEN_RIGHT_BRACE, "Expected ']' after index.");
	if (canAssign && match(compiler, TOKEN_EQUAL)) {
		parsePrecidence(compiler, PREC_COMMA + 1);
		emitByte(compiler, OP_SET_LIST);
	} else {
		emitByte(compiler, OP_GET_LIST);
	}
	
	// todo copy namedVariable's structure to allow `lst[i] += value`
}

/** Parses a variable or lambda declaration.
 * @details
 * Assumes the variable has already been consumed and is in `parser.previous`.
 * @param[in] canAssign unused.
 */
static void variable(Compiler* compiler, bool canAssign) {
	if (!compiler->inListing && (check(compiler, TOKEN_COMMA) || check(compiler, TOKEN_EQUAL_GREATER))) {
		lambda(compiler);
	} else {
		namedVariable(compiler, compiler->parser->previous, canAssign);
	}
}

/** Emits instructions/ alters state to implement the 'this' keyword.
 * @pre The 'this' keyword has already been consumed.
 * @param[in] canAssign
 */
static void this_(Compiler* compiler, bool canAssign) {
	if (compiler->class == NULL) {
		error(compiler->parser, "Cannot use 'this' outside of a class.");
		return;
	}
	variable(compiler, false); // simply create a variable called 'this'
}

static void super_(Compiler* compiler, bool canAssign) {
	if (compiler->class == NULL) {
		error(compiler->parser, "Cannot use 'super' outside of a class.");
	} else if (!compiler->class->hasSuperclass) {
		error(compiler->parser, "Cannot use 'super' in a class with no superclass.");
	}
	
	consume(compiler, TOKEN_DOT, "Expected '.' after 'super'.");
	consume(compiler, TOKEN_IDENTIFIER, "Expected superclass method name.");
	index_t name = identifierConstant(compiler, &compiler->parser->previous);

	namedVariable(compiler, syntheticToken(compiler, "this"), false); // Get 'this' variable on the stack
	if (match(compiler, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(compiler);
		namedVariable(compiler, syntheticToken(compiler, "super"), false); // Get 'super' variable on the stack. Required b/c we want to match 'this' class to the 'super' class
		emitLongable(compiler, OP_SUPER_INVOKE, OP_SUPER_INVOKE_LONG, name);
		emitByte(compiler, argCount);
	} else {
		namedVariable(compiler, syntheticToken(compiler, "super"), false);
		emitLongable(compiler, OP_GET_SUPER, OP_GET_SUPER_LONG, name);
	}
}

//~ Operators

/** Parses and emits a simple binary expression.
 * @pre The left operand has already been emitted and the operand has just been consumed.
 * @param[in] canAssign unused.
 */
static void binary(Compiler* compiler, bool canAssign) {
	// Save type of operation
	TokenType operatorType = compiler->parser->previous.type;
	// Find the precidence of the operator
	ParseRule* rule = getRule(operatorType);
	// Parse an expression at the precidence level of one above the operator for left associative oerators.
	// This will place the next operation on the stack
	parsePrecidence(compiler, (Precidence) (rule->precidence + 1));

	// Emit actual binary operation
	switch (operatorType) {
		case TOKEN_PLUS:
			emitByte(compiler, OP_ADD);
			break;
	 	case TOKEN_MINUS:
			emitByte(compiler, OP_SUBTRACT);
			break;
	 	case TOKEN_STAR:
			emitByte(compiler, OP_MULTIPLY);
			break;
	 	case TOKEN_SLASH:
			emitByte(compiler, OP_DIVIDE);
			break;
	 	case TOKEN_EQUAL_EQUAL:
			emitByte(compiler, OP_EQUAL);
			break;
	 	case TOKEN_BANG_EQUAL:
			emitBytes(compiler, OP_EQUAL, OP_NOT);
			break;
	 	case TOKEN_COMMA:
			emitByte(compiler, OP_POP);
			break;
	 	case TOKEN_QUESTION:
			emitByte(compiler, OP_CONDITIONAL);
			break;
	 	case TOKEN_COLON:
			emitByte(compiler, OP_OPTIONAL);
			break;
		case TOKEN_GREATER:
			emitByte(compiler, OP_GREATER);
			break;
	 	case TOKEN_GREATER_EQUAL:
			emitBytes(compiler, OP_LESSER, OP_NOT);
			break;
	 	case TOKEN_LESSER:
			emitByte(compiler, OP_LESSER);
			break;
	 	case TOKEN_LESSER_EQUAL:
			emitBytes(compiler, OP_GREATER, OP_NOT);
			break;
		default: // Impossible
			return;
	}
}

/** Parses a unary expression.
 * @pre The operator has already been consumed.
 */
static void unary(Compiler* compiler, bool canAssign) {
	TokenType operatorType = compiler->parser->previous.type;

	// Compile the operand at unary precidence level or higher. Allowing the same precidence level allows nesting of operations.
	parsePrecidence(compiler, PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType) {
		case TOKEN_MINUS:
			emitByte(compiler, OP_NEGATE);
			break;
		case TOKEN_BANG:
			emitByte(compiler, OP_NOT);
			break;
		default: // Impossible
			return;
	}
}

/** Parses a function call and corresponding arguments.
 * @pre Assumes '(' has already been consumed.
 * @param[in] canAssign unused.
 */
static void call(Compiler* compiler, bool canAssign) {
	int argCount = argumentList(compiler); // The number of elements on the stack to take as input
	emitBytes(compiler, OP_CALL, (uint8_t) argCount);
}

/** Parses a property get or set, but defaults a nil output on error.
 * @param[in] canAssign If true, dot indicates an assignment. Otherwise a get is parsed
 */
static void chain(Compiler* compiler, bool canAssign) {
	// Stop if a nil is found. Leave it on the stack.
	int endJump = emitJump(compiler, OP_JUMP_IF_NIL);
	dot(compiler, canAssign);
	patchJump(compiler, endJump);
}

/** Parse a disjunction expression.
 * @details
 * Performes short circuit evaluation. If the left operand is false, the right operand is not evaluated.
 * Assumes the left operand is already on the stack and 'and' has been consumed.
 * @param[in] canAssign unused.
 */
static void and_(Compiler* compiler, bool canAssign) {
	int endJump = emitJump(compiler, OP_JUMP_IF_FALSE); // If left is false, skip the right
	emitByte(compiler, OP_POP); // Pop the left operand. Ensures entire evaluation only has 1 operand left on stack
	parsePrecidence(compiler, PREC_AND); // Parse the right operand
	patchJump(compiler, endJump); // Jump to here if left is false
}

/** Parse a conjunction expression.
 * @details
 * Performes short circuit evaluation. If the left operand is true, the right operand is not evaluated.
 * Assumes the left operand is already on the stack and 'or' has been consumed.
 * @param[in] canAssign unused.
 */
static void or_(Compiler* compiler, bool canAssign) {
	int elseJump = emitJump(compiler, OP_JUMP_IF_FALSE); // If left is false, jump to the right operand
	int endJump = emitJump(compiler, OP_JUMP); // Jump to the end if left is true
	
	patchJump(compiler, elseJump); // Jump to here if left is false
	emitByte(compiler, OP_POP); // Pop the left operand. Ensures entire evaluation only has 1 operand left on stack
	
	parsePrecidence(compiler, PREC_OR); // Parse the right operand
	patchJump(compiler, endJump); // Jump to here if left is true
}

static void beginFunction(Compiler* compiler, Compiler* fnCompiler, FunctionType type) {
	// Keep compiling from the current parser but set fnCompiler to the 'current' and 'current' to the parent
	initCompiler(compiler->parser, fnCompiler, compiler, type);
	beginScope(fnCompiler); // Begin scope for the function compiler
	const bool constParams = false; // set parameters as consts for now

	// Parse the parameter list
	consume(fnCompiler, TOKEN_LEFT_PAREN, "Expected '(' after function name.");
	
	if (!check(fnCompiler, TOKEN_RIGHT_PAREN)) {
		// to implement optional args, track arity + optional arity with a bool 'topional args started' flag
		// each optiona should take an expression as well as a default value
		do {

			index_t constant = parseVariable(fnCompiler, "Expected parameter name.", constParams);
			// Type comment
#ifdef USE_CODE_COMMENT
			if (match(fnCompiler, TOKEN_COLON))
				consume(fnCompiler, TOKEN_IDENTIFIER, "Expected a type name.");
#endif
			defineVariable(fnCompiler, constant, constParams); // Do not initialize. Initialization will occur when passing functions

			fnCompiler->function->arity++;
			if (fnCompiler->function->arity > 255) {
				error(fnCompiler->parser, "Cannot have more than 255 parameters.");
			}
			
		} while (match(fnCompiler, TOKEN_COMMA));
	}
	
	consume(fnCompiler, TOKEN_RIGHT_PAREN, "Expected ')' after parameters.");
}

/** Parse a function (argument list and body) and emit bytecode to place a closure on the stack.
 * @param[in] type The type of function to parse.
 */
static void function(Compiler* compiler, FunctionType type) {
	Compiler fnCompiler;

	beginFunction(compiler, &fnCompiler, type);

	// Type comment
#ifdef USE_CODE_COMMENT
	if (match(fnCompiler, TOKEN_MINUS_GREATER)) {
		consume(fnCompiler, TOKEN_IDENTIFIER, "Expected a return type identifier.");
	}
#endif

	consume(&fnCompiler, TOKEN_LEFT_CURLY, "Expected '{' before function body.");

	// Compile the function body
	block(&fnCompiler);

	// Finish compiling and create the function object constant
	// Note: Because we end the compiler, there is no corresponding endScope(). Placing an endScope() would simply add more bytecode to pop locals with no benefit
	endCompiler(&fnCompiler, true); // sets the current compiler to the enclosing
	//! Old version took the ObjFunction that was ejected and did the last half of endCompiler
}

static void beginLambda1Parsed(Compiler* compiler, Compiler* fnCompiler, FunctionType type) {
	// Keep compiling from the current parser but set fnCompiler to the 'current' and 'current' to the parent
	initCompiler(compiler->parser, fnCompiler, compiler, type);
	beginScope(fnCompiler); // Begin scope for the function compiler
	const bool constParams = false; // set parameters as consts for now

	index_t constant = parseVariablePast(fnCompiler, constParams);
	defineVariable(fnCompiler, constant, constParams);
	fnCompiler->function->arity = 1;

	match(fnCompiler, TOKEN_COMMA); // Eat comma if it exists

	if (!check(fnCompiler, TOKEN_EQUAL_GREATER) && !check(compiler, TOKEN_EOF)) {
		// to implement optional args, track arity + optional arity with a bool 'topional args started' flag
		// each optiona should take an expression as well as a default value
		do {
			index_t constant = parseVariable(fnCompiler, "Expected parameter name.", constParams);
			defineVariable(fnCompiler, constant, constParams);
			// Type comment
#ifdef USE_CODE_COMMENT
			if (match(fnCompiler, TOKEN_COLON))
				consume(fnCompiler, TOKEN_IDENTIFIER, "Expected a type name.");
#endif

			fnCompiler->function->arity++;
			if (fnCompiler->function->arity > 255) {
				error(fnCompiler->parser, "Cannot have more than 255 parameters.");
			}
		} while (match(fnCompiler, TOKEN_COMMA));
	}
	
	consume(fnCompiler, TOKEN_EQUAL_GREATER, "Expected '=>' after parameters.");
}

/** Parses an anonymous function.
 * @pre Assumes the initial argument has been parsed and new scope has been initialized. parser.current = ','
 */
static void lambda(Compiler* compiler) {
	Compiler fnCompiler;
	beginLambda1Parsed(compiler, &fnCompiler, TYPE_LAMBDA);
	
	if (match(&fnCompiler, TOKEN_LEFT_CURLY)) {
		//~ Parse Traditional Functions
		block(&fnCompiler);
		emitByte(&fnCompiler, OP_NIL); // B/c lambdas come in expr or stmt varieties, we handle returning here instead of endCompiler()
		emitByte(&fnCompiler, OP_RETURN); // Dont use emitReturn b/c we dont want the possibility to allow 'this'
	} else {
		//~ Parse implied return
		emitByte(&fnCompiler, OP_NIL); // Implicit return value
		expression(&fnCompiler);
		emitByte(&fnCompiler, OP_RETURN);
	}
	
	// Finish compiling and create the function object constant
	endCompiler(&fnCompiler, false);
	// Emit the constant onto the stack

	// This is the old way to do it, in case needed
	//// ObjFunction* function = endCompiler(false);
	//// emitLongable(OP_CLOSURE, OP_CLOSURE_LONG, makeConstant(OBJ_VAL(function)));
	//// // For every upvalue captured, emit its locality and index
	//// for (int i = 0; i < function->upvalueCount; i++) {
	//// 	emitByte(compiler.upvalues[i].isLocal ? 1 : 0); // Flag on whether the variable is a local or not
	//// 	emitByte(compiler.upvalues[i].index); // index of the variable in the stack
	//// }
}

/** Parses an expression which culminates in a right parentheses.
 * @pre The left parentheses has already been consumed.
 * @param[in] canAssign unused.
 */
static void grouping(Compiler* compiler, bool canAssign) {
	bool oldCallStatus = compiler->inListing;
	compiler->inListing = false; // Reset in call status b/c it allows lambdas to be directly put in as args
	expression(compiler);
	compiler->inListing = oldCallStatus;
	consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')' after expression.");
}

/** Parses an array literal.
 * @pre The initial '[' has already been consumed.
 * @param[in] canAssign unused.
 */
static void list(Compiler* compiler, bool canAssign) {
	
	bool oldCallStatus = compiler->inListing;
	compiler->inListing = true; // Disallow lambdas in array literals

	int elements = 0;
	if (!match(compiler, TOKEN_RIGHT_BRACE)) {
		do {
			// Pile expressions on the stack and count how many there are
			// expression(compiler);
			parsePrecidence(compiler, PREC_COMMA + 1);
			elements++;
		} while (match(compiler, TOKEN_COMMA));

		consume(compiler, TOKEN_RIGHT_BRACE, "Expected ']' after list.");
	}
	compiler->inListing = oldCallStatus;

	emitConstant(compiler, NUMBER_VAL(elements)); // so it works like OP_NIL_LIST
	emitByte(compiler, OP_CREATE_LIST);
}

// Declared after all function declarations so they can be placed into the table.
/** Singleton representing the functions to call when a token is encountered when parsing an expression and the precidence level to parse for binary expressions. 
 * Literals are included in this table with the 'unary' slot representing the function to parse the literal.
 * Precidence column is used for the infix precedence of the operator. If some prefix (or postfix) operators had different precidence levels, their precidences would
 * also need to be stored.
 */
ParseRule rules[] = {  // 	PREFIX				INFIX					PRECIDENCE (INFIX) */
	[TOKEN_LEFT_PAREN]			= {grouping,		call,					PREC_CALL},
  [TOKEN_LEFT_CURLY]			= {NULL,				NULL,					PREC_NONE},
  [TOKEN_LEFT_BRACE]			= {list,				subscript,		PREC_CALL},
	[TOKEN_DOT]           	= {NULL,     		dot,    			PREC_CALL},
  [TOKEN_MINUS]						= {unary,				binary,				PREC_TERM},
  [TOKEN_PLUS]						= {NULL,				binary,				PREC_TERM},
  [TOKEN_SEMICOLON]				= {NULL,				NULL,					PREC_NONE},
  [TOKEN_SLASH]						= {NULL,				binary,				PREC_FACTOR},
  [TOKEN_STAR]						= {NULL,				binary,				PREC_FACTOR},
  [TOKEN_QUESTION]				= {NULL,				binary,				PREC_CONDITIONAL},	// For conditional expressions
  [TOKEN_COLON]						= {NULL,				binary,				PREC_OPTIONAL},			// For defaulted values 
	[TOKEN_COMMA]						= {NULL,				binary,				PREC_COMMA},				// For comma operator
	[TOKEN_QUESTION_DOT]    = {NULL,     		chain,    		PREC_CALL},					// For safe access
  [TOKEN_BANG]						= {unary,				NULL,					PREC_NONE},
  [TOKEN_BANG_EQUAL]			= {NULL,				binary,				PREC_EQUALITY},
  [TOKEN_EQUAL]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_EQUAL_EQUAL]			= {NULL,				binary,				PREC_EQUALITY},
  [TOKEN_GREATER]					= {NULL,				binary,				PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL]		= {NULL,				binary,				PREC_COMPARISON},
  [TOKEN_LESSER]					= {NULL,				binary,				PREC_COMPARISON},
  [TOKEN_LESSER_EQUAL]		= {NULL,				binary,				PREC_COMPARISON},
  [TOKEN_IDENTIFIER]			= {variable,		NULL,					PREC_NONE},
	[TOKEN_STRING]					= {singleString,NULL,					PREC_NONE},
	[TOKEN_STRING_INTERP]		= {doubleString,NULL,					PREC_NONE},
  [TOKEN_NUMBER]					= {number,			NULL,					PREC_NONE}, // Literals also appear as an 'operator
  [TOKEN_AND]							= {NULL,				and_,					PREC_AND},
  [TOKEN_CLASS]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_CONTINUE]				= {NULL,				NULL,					PREC_NONE},
  [TOKEN_ELSE]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_FALSE]						= {literal,			NULL,					PREC_NONE},
  [TOKEN_FOR]							= {NULL,				NULL,					PREC_NONE},
  [TOKEN_FUN]							= {NULL,				NULL,					PREC_NONE},
  [TOKEN_IF]							= {NULL,				NULL,					PREC_NONE},
  [TOKEN_IMPORT]					= {NULL,				NULL,					PREC_NONE},
  [TOKEN_NIL]							= {literal,			NULL,					PREC_NONE},
  [TOKEN_OR]							= {NULL,				or_,					PREC_OR},
  [TOKEN_PRINT]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_RETURN]					= {NULL,				NULL,					PREC_NONE},
  [TOKEN_SUPER]						= {super_,			NULL,					PREC_NONE},
  [TOKEN_THIS]						= {this_,				NULL,					PREC_NONE},
  [TOKEN_TRUE]						= {literal,			NULL,					PREC_NONE},
  [TOKEN_VAR]							= {NULL,				NULL,					PREC_NONE},
  [TOKEN_WHILE]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_SWITCH]					= {NULL,				NULL,					PREC_NONE},
  [TOKEN_CASE]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_DEFAULT]					= {NULL,				NULL,					PREC_NONE},
  [TOKEN_ERROR]						= {NULL,				NULL,					PREC_NONE},
  [TOKEN_EOF]							= {NULL,				NULL,					PREC_NONE},
};

/** Parses any expressions at the same precidence level or higher. 
 * Used to ensure the other parse functions do not greedily parse an entire expression.
 * @param[in] precidence The precidence level to parse at or above.
 */
static void parsePrecidence(Compiler* compiler, Precidence precidence) {

	// Parsing a prefix operator
	advance(compiler->parser); // Pass operator
	ParsePrefixFn prefixRule = getRule(compiler->parser->previous.type)->prefix; // Get prefix rule for operator just passed
	if (prefixRule == NULL) { // If there is no prefix rule for the operator, show an error
		error(compiler->parser, "Expected expression"); // Literals parse through the table, so by definition the expression must start with a 'unary'
		return;
	}

	// Tells prefix if it allowed to parse a '=' assignment. Also usable for infix '[', if I ever get there...
	bool canAssign = precidence <= PREC_ASSIGNMENT; // Whether we are at a low enough precidence to allow assignment in prefix

	// Call whatever function was retrieved
	prefixRule(compiler, canAssign);

	// While the precidence of the currently examined token is equal or greater than the input precidence, parse it
	// Loop relies on precidence being available and above the set precidence. If an unrecognized operator is found (has PREC_NONE in table), the function will end.
	while (precidence <= getRule(compiler->parser->current.type)->precidence) {
		advance(compiler->parser); // Get operator into parser.previous
		ParseInfixFn infixRule = getRule(compiler->parser->previous.type)->infix;
		infixRule(compiler, canAssign);
	}

	// Catch the case where the parser gets stuck with an unparsable '=' in one pass.
	if (canAssign && match(compiler, TOKEN_EQUAL)) {
		error(compiler->parser, "Invalid assignment target.");
	}
}

/** Retrieve the parse rule based off the input token type.
 * @param[in] type The token type to retrieve information from.
 * @return ParseRule pointer to the prefix, infix and infix precidence information of a token.
 */
static ParseRule* getRule(TokenType type) {
	// Function exists to allow `binary` et al. functions access to `rules` singleton
	return &rules[type];
}

/** Parses an expression starting at the lowest available precidence.5
 */
static void expression(Compiler* compiler) {
	// Lowest precidence is assignment, so this expression call will parse the entire expression
	parsePrecidence(compiler, PREC_COMMA); //! THIS MUST BE KEPT UP TO DATE WHEN NEW PRECIDENCE LEVELS ARE ADDED
}

// ~ Statements

/** Parses a statement ending semicolon. If in REPL mode, the semicolon is optional.
 * @return true if a semicolon was found, false if not.
 */
static bool REPLSemicolon(Compiler* compiler) {
	if (!compiler->parser->vm->isREPL || !check(compiler, TOKEN_EOF)) {
		consume(compiler, TOKEN_SEMICOLON, "Expected ';' after statement.");
	}
	return compiler->parser->previous.type == TOKEN_SEMICOLON;
}

/** Parses a block of statements.
 * @details
 * Assumes the '}' has already been consumed.
 */
static void block(Compiler* compiler) {
	while (!check(compiler, TOKEN_RIGHT_CURLY) && !check(compiler, TOKEN_EOF)) {
		declaration(compiler);
	}

	consume(compiler, TOKEN_RIGHT_CURLY, "Expected '}' after block.");
}

/** Parses a print statement.
 * @details
 * Assumes that the 'print' keyword has already been consumed.
 */
static void printStatement(Compiler* compiler) {
	expression(compiler);
	REPLSemicolon(compiler); // consume(TOKEN_SEMICOLON, "Expected ';' after value.");
	emitByte(compiler, OP_PRINT);
}

/** Parses an expression statement.
 */
static void expressionStatement(Compiler* compiler) {
	expression(compiler);
	if (!REPLSemicolon(compiler))
		emitByte(compiler, OP_POPREPL);
	else
		emitByte(compiler, OP_POP);
}

/** Parses an if statement with optional else.
 * @pre Assumes the 'if' keyword has already been consumed.
 */
static void ifStatement(Compiler* compiler) {
	consume(compiler, TOKEN_LEFT_PAREN, "Expected '(' after 'if'.");
	expression(compiler); // Condition. Is left on the stack.
	consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')' after condition.");

	int thenJump = emitJump(compiler, OP_JUMP_IF_FALSE); // Instruction to jump if stack top is false. Saves the offset to be filled in later.
	emitByte(compiler, OP_POP); // Pop condition. Done before any true branch code.
	statement(compiler);

	int elseJump = emitJump(compiler, OP_JUMP); // Create the unconditional jump instruction for the true branch to be filled in later.
	
	patchJump(compiler, thenJump); // Replace the placeholder instruction with the actual jump amount
	emitByte(compiler, OP_POP); // Pop at start of else. NOTE: makes an implicit else branch
	
	if (match(compiler, TOKEN_ELSE)) {
		statement(compiler);
	}

	patchJump(compiler, elseJump);
}

/** Parses a new variable declaration.
 * @pre Assumes that the 'var' keyword has already been consumed.
 */
static void varDeclaration(Compiler* compiler) {
	// If local, place on stack and update locals array
	// If global, put identifier string on stack
	index_t global = parseVariable(compiler, "Expecteded variable name.", false);

	if (match(compiler, TOKEN_COLON)) {
		consume(compiler, TOKEN_IDENTIFIER, "Expected type identifier after ':'.");
	}

	if (match(compiler, TOKEN_LEFT_BRACE)) {
		expression(compiler);
		
		consume(compiler, TOKEN_RIGHT_BRACE, "Expected ']' after array size.");
		if (match(compiler, TOKEN_EQUAL)) {
			error(compiler->parser, "List size declarations cannot have an initializer.");
		}

		emitByte(compiler, OP_NIL_LIST);

	} else if (match(compiler, TOKEN_EQUAL)) {
		// Check for initializer expression
		expression(compiler);
	} else {
		emitByte(compiler, OP_NIL);
	}


	REPLSemicolon(compiler);
	// If local, do nothing
	// If global, emit bytecode to store the value in the global table
	defineVariable(compiler, global, false);
}

/** Parses a new variable declaration.
 * @pre Assumes that the 'var' keyword has already been consumed.
 */
static void letDeclaration(Compiler* compiler) {
	index_t global = parseVariable(compiler, "Expecteded constant name.", true);
	
	if (match(compiler, TOKEN_COLON)) {
		consume(compiler, TOKEN_IDENTIFIER, "Expected type identifier after ':'.");
	}

	if (match(compiler, TOKEN_LEFT_BRACE)) {
		expression(compiler);

		consume(compiler, TOKEN_RIGHT_BRACE, "Expected ']' after array size.");
		if (match(compiler, TOKEN_EQUAL)) {
			error(compiler->parser, "List size declarations cannot have an initializer.");
		}

		emitByte(compiler, OP_NIL_LIST);
	} else if (match(compiler, TOKEN_EQUAL)) {
			expression(compiler);
	} else {
		error(compiler->parser, "Expected constant initializer or list size.");
	}

	REPLSemicolon(compiler);
	defineVariable(compiler, global, true);
}

/** Parses a funciton declaration.
 * @details
 * Assumes that the 'fun' keyword has already been consumed.
 */
static void funDeclaration(Compiler* compiler) {
	bool constFunc = false;
	index_t global = parseVariable(compiler, "Expected function name.", constFunc);
	markInitialized(compiler); // Allow recursion
	function(compiler, TYPE_FUNCTION); // Functions are first class, so this simply places one on the stack
	defineVariable(compiler, global, constFunc); // Define the variable;
}

static void setupClassCompiler(Compiler* compiler, ClassCompiler* classCompiler) {
	// Initialize the class compiler's state
	classCompiler->name = compiler->parser->previous;
	classCompiler->enclosing = compiler->class;
	classCompiler->hasSuperclass = false;
	// Update the compiler's state
	compiler->class = classCompiler;
}

static void endClassCompiler(Compiler* compiler, ClassCompiler* classCompiler) {
	// Restore the compiler's state
	compiler->class = compiler->class->enclosing;
}

static void classDeclaration(Compiler* compiler) {
	const bool constClass = false;
	
	consume(compiler, TOKEN_IDENTIFIER, "Expected class name.");
	Token className = compiler->parser->previous;
	index_t name = identifierConstant(compiler, &compiler->parser->previous); // Place class name into constant pool so it can be printed later
	declareVariable(compiler, constClass); // Binds class object to a variable of the same name. ADDS VAR TO SCOPE

	// We need to track the innermost class compiler so we can determine if 'this' is valid
	ClassCompiler classCompiler;
	setupClassCompiler(compiler, &classCompiler);

	// Declare before class body so it can be used in the body.
	emitLongable(compiler, OP_CLASS, OP_CLASS_LONG, name); // Emit instruction to create class at runtime
	defineVariable(compiler, name, constClass); // Define variable for class's name

	// Check for superclass
	if (match(compiler, TOKEN_LESSER_MINUS)) {
		consume(compiler, TOKEN_IDENTIFIER, "Expected superclass name.");
		// Create a new variable on stack for super (since variable() operates on parser.previous)
		variable(compiler, false); // Load class we are inheriting from onto the stack

		// fix does this work?
		if (identifiersEqual(&compiler->parser->previous, &className)) {
			error(compiler->parser, "A class cannot inherit from itself.");
		}

		beginScope(compiler); // Begin scope to resolve super. Without a scope, all supers would collide
		addLocal(compiler, syntheticToken(compiler, "super"), false); // Name the variable() above 'super'
		defineVariable(compiler, 0, false); // Define variable

		namedVariable(compiler, className, false);
		emitByte(compiler, OP_INHERIT);
		classCompiler.hasSuperclass = true;
	}

	namedVariable(compiler, className, false); // Place class name on the stack
	consume(compiler, TOKEN_LEFT_CURLY, "Expected '{' before class body.");

	while (!check(compiler, TOKEN_RIGHT_CURLY) && !check(compiler, TOKEN_EOF)) {
		method(compiler);
	}

	consume(compiler, TOKEN_RIGHT_CURLY, "Expected '}' after class body.");
	emitByte(compiler, OP_POP); // Pop class name

	if (classCompiler.hasSuperclass) {
		endScope(compiler); // End of super scope
	}

	endClassCompiler(compiler, &classCompiler); // Restore the compiler's state
}

static void method(Compiler* compiler) {
	// Name of the method
	consume(compiler, TOKEN_IDENTIFIER, "Expected method name.");
	uint8_t constant = identifierConstant(compiler, &compiler->parser->previous); // place method name into constant pool

	// Closure for method body
	FunctionType type = TYPE_METHOD;
	// Check if initialzer -> want to disallow explicit (user) return and have implicit 'return this' instead of 'return nil'
	if (compiler->parser->previous.length == 4 && memcmp(compiler->parser->previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	function(compiler, type); // Place closure on the stack

	// Bind method to class
	emitLongable(compiler, OP_METHOD, OP_METHOD_LONG, constant);
}

/** Parses an import statement.
 * @pre Assumes that the 'import' keyword has already been consumed.
 * @param[in] canAssign unused.
 */
static void importDeclaration(Compiler* compiler) {
	if (match(compiler, TOKEN_STRING)) {
		index_t importConstant = makeConstant(compiler, OBJ_VAL(copyString(
			compiler->parser->vm,
			compiler->parser->previous.start + 1,
			compiler->parser->previous.length - 2
		)));

		emitLongable(compiler, OP_CONSTANT, OP_CONSTANT_LONG, importConstant);
		emitBytes(compiler, OP_IMPORT, OP_POP);
	} else {
		error(compiler->parser, "Builtin imports not implemented yet");
	}

	// Old method
	//// expression(compiler);
	//// emitBytes(compiler, OP_INCLUDE, OP_POP); // Function has implicit return due to endCompiler. include is a declaration, so we dont care about the output

	REPLSemicolon(compiler);
}

/** Parses an while statement.
 * @pre Assumes the 'while' keyword has already been consumed.
 */
static void whileStatement(Compiler* compiler) {
	int loopStart = currentChunk(compiler)->count; // Save the current byte offset for the loop start.
	int oldLoop = compiler->recentLoop; // Save the position of the previous loop
	int oldBreak = compiler->numBreak;
	compiler->recentLoop = loopStart; // Set the destination for any 'continue' statements.
	consume(compiler, TOKEN_LEFT_PAREN, "Expected '(' after 'while'.");
	expression(compiler);
	consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')' after condition.");

	int exitJump = emitJump(compiler, OP_JUMP_IF_FALSE);
	emitByte(compiler, OP_POP); // Pop condition
	statement(compiler); // Do anything in the while loop
	emitLoop(compiler, loopStart); // Jump back to the start of the loop

	patchJump(compiler, exitJump);
	emitByte(compiler, OP_POP); // Pop condition
	
	// Go for the different of the current number of breaks minus the number of old breaks (gives number of new ones)
	// Patch here b/c nothing should be on the stack and want to avoid the OP_POP produced by the end of the loop
	patchBreaks(compiler, oldBreak);
	compiler->recentLoop = oldLoop; // Restore the previous 'loop scope'
}

/** Parses a for loop.
 * @pre Assumes the 'for' keyword has already been consumed.
 */
static void forStatement(Compiler* compiler) {
	beginScope(compiler); // Begin a new scope for the loop
	consume(compiler, TOKEN_LEFT_PAREN, "Expected '(' after 'for'.");

	// Optional initializer
	if (match(compiler, TOKEN_SEMICOLON)) {
		// No initializer
	} else if (match(compiler, TOKEN_VAR)) {
		varDeclaration(compiler);
	} else {
		expressionStatement(compiler);
	}
	
	// Optional condition
	int loopStart = currentChunk(compiler)->count;
	int exitJump = -1;
	if (!match(compiler, TOKEN_SEMICOLON)) { // If no condition, this will consume the ';'
		expression(compiler);
		consume(compiler, TOKEN_SEMICOLON, "Expected ';' after loop condition.");
		
		exitJump = emitJump(compiler, OP_JUMP_IF_FALSE);
		emitByte(compiler, OP_POP); // Pop condition
	}

	// Optional increment
	// Works by jumping OVER the increment, executing the body, and then returning to the increment. After that, it jumps to the condition
	if (!match(compiler, TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(compiler, OP_JUMP); // Skip the increment for now
		int incrementStart = currentChunk(compiler)->count;
		expression(compiler);
		emitByte(compiler, OP_POP); // Pop expr. statement result
		consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')' after for clauses.");

		emitLoop(compiler, loopStart); // Jump back to the condition. If condition is not there, it will jump to the unconditional jump to the body
		loopStart = incrementStart; // Body will now jump to the increment start
		patchJump(compiler, bodyJump); // Jump here to execute the body
	}
	
	int oldLoop = compiler->recentLoop; // Save the old loop position in case it is needed
	int oldBreak = compiler->numBreak;
	compiler->recentLoop = loopStart; // loop set will be the condition or the increment if it exists

	// Body and jump back to condition
	statement(compiler);
	emitLoop(compiler, loopStart);

	if (exitJump != -1) {
		patchJump(compiler, exitJump);
		emitByte(compiler, OP_POP); // Pop condition
	}

	// Go for the different of the current number of breaks minus the number of old breaks (gives number of new ones)
	// Patch here b/c nothing should be on the stack and want to avoid the OP_POP produced by the end of the loop
	patchBreaks(compiler, oldBreak);
	compiler->recentLoop = oldLoop; // Restore the previous 'loop scope'
	
	endScope(compiler);
}

/** Parses a continue statement
 * @pre The 'continue' keyword has already been consumed.
 */
static void continueStatement(Compiler* compiler) {
	if (compiler->recentLoop == -1) {
		error(compiler->parser, "Cannot continue outside of a loop.");
	}

	// Unconditional jump back to top of the loop
	emitLoop(compiler, compiler->recentLoop);
	REPLSemicolon(compiler);
}

/** Parses a continue statement
 * @pre The 'break' keyword has already been consumed.
 */
static void breakStatement(Compiler* compiler) {
	if (compiler->recentLoop == -1) {
		error(compiler->parser, "Cannot break outside of a loop.");
	}

	prematureEndScope(compiler);
	int bodyJump = emitJump(compiler, OP_JUMP);
	if (compiler->numBreak >= MAX_BREAKS) {
		error(compiler->parser, "Cannot have more than 255 breaks in a loop.");
	}
	compiler->recentBreak[compiler->numBreak++] = bodyJump;
	REPLSemicolon(compiler);
}

/** Parses a switch statement and corresponding cases.
 * @details
 * Switch statements allow for fall through and break's.
 */
static void switchStatement(Compiler* compiler) {
	consume(compiler, TOKEN_LEFT_PAREN, "Expected '(' after 'switch'.");
	expression(compiler);
	Token name = syntheticToken(compiler, "$switch");
	declareNamedVariable(compiler, &name, false);
	
	consume(compiler, TOKEN_RIGHT_PAREN, "Expected ')' after value.");
	consume(compiler, TOKEN_LEFT_CURLY, "Expected '{' before switch cases.");

#define BEFORE_CASE 0
#define BEFORE_DEFAULT 1
#define AFTER_DEFAULT 2

	int state = BEFORE_CASE;
	int caseEnds[MAX_CASES];
	int caseCount = 0;
	int previousCaseSkip = -1;
	int oldBreak = compiler->numBreak;
	int oldLoop = compiler->recentLoop;
	compiler->recentLoop = currentChunk(compiler)->count;

	while (!match(compiler, TOKEN_RIGHT_CURLY) && !check(compiler, TOKEN_EOF)) {
		if (match(compiler, TOKEN_CASE) || match(compiler, TOKEN_DEFAULT)) {
			TokenType caseType = compiler->parser->previous.type;

			if (state == AFTER_DEFAULT) {
				error(compiler->parser, "Can't have another case or default after the default case.");
			}

			if (state == BEFORE_DEFAULT) {
				// At the end of the previous case, jump over the others.
				caseEnds[caseCount++] = emitJump(compiler, OP_JUMP);

				// Patch its condition to jump to the next case (this one).
				patchJump(compiler, previousCaseSkip);
				emitByte(compiler, OP_POP);
			}

			if (caseType == TOKEN_CASE) {
				// See if the case is equal to the value.
				emitByte(compiler, OP_DUP);
				parsePrecidence(compiler, PREC_TERM); // Exclude ':' from parsing
				emitByte(compiler, OP_EQUAL);

				consume(compiler, TOKEN_COLON, "Expected ':' after case value.");

				// Jump if the case doesn't match
				previousCaseSkip = emitJump(compiler, OP_JUMP_IF_FALSE);
				// Pop the comparison result.
				emitByte(compiler, OP_POP);

				// Jump here if you came from elsewhere
				if (state == BEFORE_DEFAULT)
					patchJump(compiler, caseEnds[caseCount - 1]);

				// Change state after all checks are done
				state = BEFORE_DEFAULT;

			} else {
				// If cases exist before, flow here
				if (state == BEFORE_DEFAULT)
					patchJump(compiler, caseEnds[caseCount - 1]);

				consume(compiler, TOKEN_COLON, "Expected ':' after default.");
				previousCaseSkip = -1;

				state = AFTER_DEFAULT;
			}
		} else {
			// Otherwise, it's a statement inside the current case.
			if (state == BEFORE_CASE) {
				error(compiler->parser, "Can't have statements before any case.");
			}
			statement(compiler);
		}
	}

	// If we ended without a default case, patch its condition jump.
	if (state == BEFORE_DEFAULT) {
		// emitByte(OP_POP);
		patchJump(compiler, previousCaseSkip);
	}

	patchBreaks(compiler, oldBreak);
	compiler->recentLoop = oldLoop;
	emitByte(compiler, OP_POP); // The switch value.

#undef BEFORE_CASE 
#undef BEFORE_DEFAULT 
#undef AFTER_DEFAULT 
}

#ifndef USE_STACK_PROPERTY_DELETE
/** Emits 2-4 bytes which correspond to the creation of a constant value.
 * @param[in] value The value to write.
 */
static void emitDelProperty(Value value) {
	index_t index = makeConstant(value);
	emitLongable(OP_DEL_PROPERTY, OP_DEL_PROPERTY_LONG, index);
}
#endif

/** Removes a property from an instance.
 */
static void delStatement(Compiler* compiler) {
	// Get initial variable
	do {
		if (!match(compiler, TOKEN_IDENTIFIER)) {
			error(compiler->parser, "Expected identifier after 'del'.");
		}
		namedVariable(compiler, compiler->parser->previous, false);

		if (!check(compiler, TOKEN_DOT)) {
			error(compiler->parser, "Expected '.' after identifier.");
		}

		// Keep getting until there is no more dots
		while (match(compiler, TOKEN_DOT) && !check(compiler, TOKEN_EOF)) {
			consume(compiler, TOKEN_IDENTIFIER, "Expected property name after '.'.");
			if (!check(compiler, TOKEN_DOT)) {
				break;
			}

			index_t nameIndex = identifierConstant(compiler, &compiler->parser->previous); // property name
			emitLongable(compiler, OP_GET_PROPERTY, OP_GET_PROPERTY_LONG, nameIndex);
		}

		// Stack will have the instance on the top and the property we are interested in is parsed but NOT emitted yet
		// Delete the property
#ifdef USE_STACK_PROPERTY_DELETE
		emitConstant(compiler, OBJ_VAL(copyString(compiler->parser->vm, compiler->parser->previous.start, compiler->parser->previous.length)));
		emitByte(compiler, OP_DEL_PROPERTY);
#else
		emitDelProperty(OBJ_VAL(copyString(parser.previous.start, parser.previous.length)));
#endif
	} while (match(compiler, TOKEN_COMMA));

	REPLSemicolon(compiler);
}

/** Parses a return statement. Optionally allows an expression for a return value.
 */
static void returnStatement(Compiler* compiler) {
	if (compiler->type == TYPE_SCRIPT) {
		error(compiler->parser, "Cannot return from top-level code.");
	}
	if (match(compiler, TOKEN_SEMICOLON)) {
		emitReturn(compiler);
	} else {
		if (compiler->type == TYPE_INITIALIZER) {
			error(compiler->parser, "Cannot return a value from an initializer.");
		}
		
		expression(compiler);
		consume(compiler, TOKEN_SEMICOLON, "Expected ';' after return value.");
		emitByte(compiler, OP_RETURN);
	}
}

/** Parses a statement.
 */
static void statement(Compiler* compiler) {
	if (match(compiler, TOKEN_PRINT)) {
		printStatement(compiler);
	} else if (match(compiler, TOKEN_LEFT_CURLY)) {
		beginScope(compiler);
		block(compiler);
		endScope(compiler);
	} else if (match(compiler, TOKEN_IF)) {
		ifStatement(compiler);
	} else if (match(compiler, TOKEN_WHILE)) {
		whileStatement(compiler);
	} else if (match(compiler, TOKEN_FOR)) {
		forStatement(compiler);
	} else if (match(compiler, TOKEN_CONTINUE)) {
		continueStatement(compiler);
	} else if (match(compiler, TOKEN_BREAK)) {
		breakStatement(compiler);
	} else if (match(compiler, TOKEN_DEL)) {
		delStatement(compiler);
	} else if (match(compiler, TOKEN_SWITCH)) {
		beginScope(compiler);
		switchStatement(compiler);
		endScope(compiler);
	} else if (match(compiler, TOKEN_RETURN)) {
		returnStatement(compiler);
	} else {
		expressionStatement(compiler);
	}
}

/** Parses a declaration statement.
 */
static void declaration(Compiler* compiler) {
	if (match(compiler, TOKEN_VAR)) {
		varDeclaration(compiler);
	} else if (match(compiler, TOKEN_LET)) {
		letDeclaration(compiler);
	} else if (match(compiler, TOKEN_FUN)) {
		funDeclaration(compiler);
	} else if (match(compiler, TOKEN_CLASS)) {
		classDeclaration(compiler);
	} else if (match(compiler, TOKEN_IMPORT)) {
		importDeclaration(compiler);
	} else {
		statement(compiler);
	}
	
	if (compiler->parser->panicMode)
		synchronize(compiler->parser);
}

//~ Compilation Functions

ObjFunction* compile(VM* vm, ObjModule* module, const char* source) {
	// Initialize the parser
	Parser parser;
	parser.vm = vm;
	parser.hadError = false;
	parser.panicMode = false;
	parser.module = module;

	// Init scanner
	Scanner scanner;
	initScanner(&scanner, source);
	parser.scanner = scanner;

	// Init compiler
	Compiler compiler;
	initCompiler(&parser, &compiler, NULL, TYPE_SCRIPT);

	// 'Prime' the scanner
	advance(compiler.parser);

	// A program is a series of declaration
	while (!match(&compiler, TOKEN_EOF)) {
		declaration(&compiler);
	}

	ObjFunction* function = endCompiler(&compiler, true);
	return parser.hadError ? NULL : function;
}

void markCompilerRoots(VM* vm) {
	Compiler* compiler = vm->compiler;
	// todo: mark marts of the compiler
	while (compiler != NULL) {
		markObject(vm, (Obj*) compiler->function);
		compiler = compiler->enclosing;
	}
}
