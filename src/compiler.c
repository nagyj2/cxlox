#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "vm.h"
#include "object.h"
#include "memory.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/* Singleton representing the currently parsing and previously parsed tokens. */
typedef struct {
	Token current;		//* The parser currently being parsed.
	Token previous;		//* The most recently parsed token.
	bool hadError;		//* Whether or not an error has occurred.
	bool panicMode;		//* If true, the parser will discard tokens until a synchrnization point is found.
} Parser;

/* Enumerates the different precidence levels for expressions. Written in a way so each enum is 'greater' than the values which preceed it.*/
typedef enum {
	PREC_NONE,
	PREC_ASSIGNMENT,	// =
	PREC_OR,					// or
	PREC_AND,					// and
	PREC_EQUALITY,		// == !=
	PREC_COMPARISON,	// < > <= >=
	PREC_TERM,				// + -
	PREC_FACTOR,			// * /
	PREC_UNARY,				// - !
	PREC_CALL,				// . ()
	PREC_PRIMARY
} Precidence;

/* Function type which takes one argument and no return. Used to store the desired function in ParseRule struct. */
typedef void (*ParseFn)(bool canAssign);

/** For any operation which starts with a token, this structure contains the: 
 * the function to compile a prefix expression starting with a token of that type, 
 * the function to compile an infix expression whose left operand is followed by a token of that type, 
 *the precedence of an infix expression that uses that token as an operator.
 */
typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precidence precidence;
} ParseRule;

/** Represents a local variable inside some scope.
 * Locals are kept on the stack when declared, unlike global variables which are sent to a separate hash table, so
 * to index a local, the number of elements to 'backtrack' on the stack is required. When a local is declared, the
 * current depth of the local is saved so it can be removed when exiting scope. The compiler learns how far back the
 * local is for retrieval when the local is called. The compiler will start at the top of the local stack and walk backwards.
 * B/c only locals are stored on the stack and are in the same order here as in the VM, the offset it finds in the
 * locals array is the number of spots to backtrack on the stack.
 *
 * Captured statis is tracked because it dictates whether the local should be hoisted to the heap when the variable goes out of scope.
 * Up until hoisting, anything may refer to its stack position, so care must be taken to ensure the element can be tracked
 * on the stack. When it is hoisted, the ObjUpvalue 'location' attribute is set to the 'closed' attribute.
 */
typedef struct {
	Token name;				//* Variable name.
	int depth;				//* Depth of the scope in which the variable is declared from the global scope.
	bool isCaptured;	//* Whether or not the variable is captured by a closure. Controls how it is removed from the stack. 
} Local;

/** Represent a local variable which is inside an enclosing function. Allows for closures to refer to variables declared outside
 * their scope. This is done by storing a pointer to the location of the variable and then all references to the variable using that
 * new pointer. This struct represents a single upvalue which exists within the currently compiling chunk. They also track whether
 * they are local to the current closure. Tracking locality is important because when it determines how they are found at runtime.
 * Local upvalues have their index representing the stack position of the upvalue and non-locals represent the index in the upvalue
 * array. Essentially, a non-local upvalue is a pointer to another upvalue.
 *
 * Upvalues are stored a within an array by the compiler and their index and order mimics how they appear in the VM's upvalue array.
 * Just like the locals array and stack, the mirroring allows for the indexes to the proper element to be created in the compiler
 * and then used by the VM. Unlike the locals array, the count of the upvalues in a closure is kept in the function object.
 */
typedef struct {
	uint8_t index;		//* Index of where the variable is. If local, index is the frame stack index position. If non-local, index is where in the upvalue array the variable is. When local, always +1 compared to stack b/c callee occupies slot 0. When non-local, indexes based on upvalue array indices.
	bool isLocal;			//* Whether the captured variable is local to the currently compiling closure. If 1, the index indicates where on the stack (from frame slot 0) the variable is. If 0, the index represents where the variable is in the upvalue array.
} Upvalue;

/** The type of code which is being compiled. */
typedef enum {
	TYPE_FUNCTION,			//* A function body is being compiled.
	TYPE_SCRIPT,				//* The top-level (global) code is being compiled.
	TYPE_METHOD,				//* A method is being compiled.
	TYPE_INITIALIZER,		//* Initializer method is being compiled.
} FunctionType;

typedef struct Compiler Compiler;
/** State for the compiler.
 * @note The size of locals should be the exact same size as the stack. If one changes, the other must as well.
 */
struct Compiler {
	Compiler* enclosing;						//* The compiler before the current one.
	ObjFunction* function;					//* The function being compiled.
	FunctionType type;							//* The type of function being compiled.
	Local locals[UINT8_COUNT]; 			//* Array of local variables. Length is fixed at 256 due to 8 bit indexes.
	int localCount; 								//* Number of local variables currently in scope.
	int scopeDepth; 								//* Depth of of the scope where the compiler currently is. How 'far' the scope is from the global scope.
	Upvalue upvalues[UINT8_COUNT];	//* Upvalue array for the current closure.
};

/** Captures the innermost class being compiled.
 */
typedef struct ClassCompiler {
	struct ClassCompiler* enclosing;
} ClassCompiler;

// Parser singleton.
Parser parser;
// Current compiler singleton.
Compiler* current = NULL;
// Current class compiler singleton.
ClassCompiler* currentClass = NULL;

// Forward declares to allow references.
static void expression();
static void statement();
static void declaration();
static void varDeclaration();
static void funDeclaration();
static void classDeclaration();
static void expressionStatement();
static void printStatement();
static void ifStatement();
static void whileStatement();
static void forStatement();
static void returnStatement();
static void block();
static void function();
static void method();
static ParseRule* getRule(TokenType type);
static void parsePrecidence(Precidence precidence);

/** The chunk which is currently getting bytecode emitted to it.
 * @return Chunk* pointer to the chunk currently being emitted to.
 */
static Chunk* currentChunk() {
	return &current->function->chunk;
}

//~ Error functions

/** Prints an error to stderr. Includes line number information.
 * @param[in] token The token which represents the error.
 * @param[in] message The error message to display.
 */
static void errorAt(Token* token, const char* message) {
	if (parser.panicMode) // Ignore if in panic mode
		return;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else { // Show error's lexeme
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

/** Reports an error at the token which is currently being parsed.
 * 
 * @param[in] message The error to display.
 */
static void errorAtCurrent(const char* message) {
	errorAt(&parser.current, message);
}

/** Reports an error at the token which was just parsed.
 * 
 * @param[in] message The error to display.
 */
static void error(const char* message) {
	errorAt(&parser.previous, message);
}

//~ Helper Functions

/** Advances the parser forward to the next token. Updates the global parser state.
 * 
 */
static void advance() {
	parser.previous = parser.current;

	// B/c errors are through special tokens, wrap in a for loop so multiple error tokens in a row are shown and discarded
	for (;;) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser.current.start);
	}
}

/** Consume input until a synchronization token is found.
 * 
 */
static void synchronize() {
	parser.panicMode = false;

	while (parser.current.type != TOKEN_EOF) {
		if (parser.previous.type == TOKEN_SEMICOLON)
			return;
		switch (parser.current.type) {
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

		advance();
	}
}

/** Consumes the next token if it matches the input type and causes an error if it doesn't.
 * 
 * @param[in] type The token type to compare the current token against.
 * @param[in] message Message to display if the token doesn't match.
 */
static void consume(TokenType type, const char* message) {
	if (parser.current.type == type) {
		advance();
		return;
	}

	errorAtCurrent(message);
}

/** Returns whether the current token matches the input token type.
 * 
 * @param[in] type The token type to compare the current token against.
 * @return true if the token matches.
 * @return false if the token doesnt match.
 */
static bool check(TokenType type) {
	return parser.current.type == type;
}

/** Returns whether or not the current token matches the input type.
 * @details
 * If a match is encountered, the token is consumed.
 *
 * @param[in] type The token type to check for
 * @return true if the token was found.
 * @return false if the token was not found.
 */
static bool match(TokenType type) {
	if (!check(type))
		return false;

	advance();
	return true;
}



//~ Bytecode Emission

/** Appends a byte to the current chunk.
 * 
 * @param[in] byte The byte to append.
 */
static void emitByte(uint8_t byte) {
	writeChunk(currentChunk(), byte, parser.previous.line);
}

/** Emits two sequential bytes to the current chunk.
 * 
 * @param[in] byte1 The first byte to emit.
 * @param[in] byte2 The second byte to emit.
 */
static void emitBytes(uint8_t byte1, uint8_t byte2) {
	emitByte(byte1);
	emitByte(byte2);
}

/** Replaces the jump placeholder bytes at the given offset to jump to the current byte.
 * @details
 * Assumes a 16 bit jump placeholder starts at the code chunk at index @p offset.
 * @param[in] offset The position of the jump placeholder to replace.
 */
static void patchJump(int offset) {
	// -2 to account for the size of the jump placeholder.
	int jump = currentChunk()->count - offset - 2; // find the number of bytes to skip

	if (jump > UINT16_MAX) {
		error("Too much code to jump over.");
	}

	currentChunk()->code[offset] = (jump >> 8) & 0xFF; // MSB
	currentChunk()->code[offset + 1] = jump & 0xFF;
}

/** Emits 3 bytes corresponding to a jump opcode and a 2 byte jump offset.
 * @param[in] instruction The jump opcode to emit.
 * @return int The position of the jump offset bytes.
 */
static int emitJump(uint8_t instruction) {
	emitByte(instruction);
	emitByte(0xff); // Jump offset placeholder.
	emitByte(0xff);
	return currentChunk()->count - 2;
}

/** Emit 3 bytes corresponding to a unconditional jump backwards.
 * @param[in] loopStart The byte offset for the loop to jump back to.
 */
static void emitLoop(int loopStart) {
	emitByte(OP_LOOP);

	// +2 for the size of the jump address
	int offset = currentChunk()->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error("Loop body too large.");

	emitByte((offset >> 8) & 0xff);
	emitByte(offset & 0xff);
}

/** Places a constant into the current chunk's constant pool.
 * @details
 * Causes an error if the number of constants in the chunk exceeds the maximum.
 * @param[in] value The value to write to the constant pool.
 * @return uint8_t byte representing the index of the constant in the constant pool.
 */
static uint8_t makeConstant(Value value) {
	int constant = addConstant(currentChunk(), value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t) constant;
}

/** Emits an implied nil constant and a opcode for function return.
 */
static void emitReturn() {
	if (current->type == TYPE_INITIALIZER) {
		emitBytes(OP_GET_LOCAL, 0); // we reserved slot 0 for 'this'. Return this if initializer
	} else {
		emitByte(OP_NIL); // Implicitly places nil on the stack
	}
	
	// If a return expression is present, it will be the stack top and get returned. Otherwise the just emitted nil will
	emitByte(OP_RETURN);
}

/** Emits 2 bytes which correspond to the creation of a constant value.
 * @param[in] value The value to write.
 */
static void emitConstant(Value value) {
	emitBytes(OP_CONSTANT, makeConstant(value));
}

/** Initializes the state of the compiler and sets it to be the current compiler.
 * @pre Assumes the name of the function has just been consumed if parsing a function body.
 * @param[out] compiler The compiler to initialize.
 * @param[in] type The type of function being compiled.
 */
static void initCompiler(Compiler* compiler, FunctionType type) {
	compiler->enclosing = current; // Store the previous compiler.
	compiler->function = newFunction();
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	current = compiler;

	// If not parsing the main script, assign the name to the previously parsed token
	if (type != TYPE_SCRIPT) {
		// Now that compilers can die, we need to copy the string so that the reference doesn't get nullified
		current->function->name = copyString(parser.previous.start, parser.previous.length);
	}

	// Reserve a position in the stack for the compiler's use
	Local* local = &current->locals[current->localCount++];
	local->depth = 0; // Save as a top level scoped local. Prevents it from getting removed from endScope
	local->isCaptured = false;
	if (type != TYPE_FUNCTION) {
		local->name.start = "this";
		local->name.length = 4;
	} else {
		local->name.start = "";
		local->name.length = 0;
	}
}

/** Marks the end of a compilation.
 * @return ObjFunction* The completely compiled function.
 */
static ObjFunction* endCompiler() {
	emitReturn();
	ObjFunction* function = current->function; // Return the function we just made

	// If debug is enabled, print the completed bytecode if there was no error
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
	}
#endif

	current = current->enclosing; // Restore the previous compiler
	return function;
}

/** Signals the start of a new scope to the current compiler singleton.
 */
static void beginScope() {
	current->scopeDepth++;
}

/** Signals the end of the current scope to the compiler singleton.
 * @details
 * Removes all local variables stored at the current scope.
 */
static void endScope() {
	current->scopeDepth--;
	while (current->localCount > 0
		&& current->locals[current->localCount - 1].depth > current->scopeDepth) {
		// If local is camptured, emit a special pop instruction
		if (current->locals[current->localCount - 1].isCaptured) {
			emitByte(OP_CLOSE_UPVALUE);
		} else {
			emitByte(OP_POP);
		}
		current->localCount--;
	}
}

/** Declare the top most local variable available for use by removing the sentinel -1 depth.
 * @details
 * If the variable is not global to the current function, its depth is updated.
 */
static void markInitialized() {
	// Do not mark initialization for global functions/ variables
	if (current->localCount == 0)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/** Emits 2 bytes to define a new variable for use. After this point the variable can be referenced.
 * If defining a local variable, it will be marked as initialized by the compiler.
 * If defining a global variable, code will be emitted to define the variable globally.
 * 
 * @param[in] global The index to the constant pool location of the global variable's name.
 */
static void defineVariable(uint8_t global) {
	// If a local, don't put into constant pool
	if (current->scopeDepth > 0) {
		markInitialized();
		return; // locals dont need any new bytes b/c their value is already on top of the stack
	}
	emitBytes(OP_DEFINE_GLOBAL, global);
}

/** Creates a string constant for an identifier, places it in the constant pool, and returns the index of the constant.
 * 
 * @param[in] name The name of the variable to declare.
 * @return uint8_t 
 */
static uint8_t identifierConstant(Token* name) {
	return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

/** Test if two identifier (string) tokens are equal.
 * 
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

/** Resolves a variable as a local or global.
 * @details
 * If the variable is a local, an integer corresponding to how far down it is on the stack is returned.
 * If the variable is a global, a corresponding value is not found and a sentinel value -1 is returned.
 *
 * @param[in] compiler The compiler to resolve the variable from.
 * @param[in] name The variable to try and resolve.
 * @return int representing the index of the variable in the locals array or a sentinel value of -1.
 */
static int resolveLocal(Compiler* compiler, Token* name) {
	for (int i=compiler->localCount - 1; i >= 0; i--) {
		Local* local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1) {
				error("Cannot read local variable in its own initializer.");
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
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
	int upvalueCount = compiler->function->upvalueCount; // Find the current number of upvalues

	// Check for an existing upvalue. B/c they map by index, we want to verify that the index is the same and the locality.
	// Locality matters because the origin of the upvalue call can refer to different variables
	for (int i = 0; i < upvalueCount; i++) {
		Upvalue* upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) {
			return i; // Return the index in the array
		}
	}

	// Verify there isnt too many upvalues
	if (upvalueCount == UINT8_COUNT) {
		error("Too many closure variables in function.");
		return 0;
	}

	// Fill in the new values for (read: create) a new upvalue
	compiler->upvalues[upvalueCount].index = index;
	compiler->upvalues[upvalueCount].isLocal = isLocal;
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
	int local = resolveLocal(compiler->enclosing, name);
	if (local != -1) { // If found as a local, create a local upvalue in the below compiler.
		compiler->enclosing->locals[local].isCaptured = true; // Mark the local as captured. Used to instruct VM how to dispose of it
		// Variable was found in the enclosing compiler, so mark it as local.
		return addUpvalue(compiler, (uint8_t) local, true);
	}

	// If it is not local, resolve the upvalue recursively to the top of the call stack
	int upvalue = resolveUpvalue(compiler->enclosing, name);
	if (upvalue != -1) { // If found, create a non-local upvalue to permit an upvalue chain
		// Creates an upvalue. Mark it as non-local because it is NOT in in the closure immediately above current
		// This allows for a upvalue to point to an upvalue
		return addUpvalue(compiler, (uint8_t) upvalue, false);
	}

	// Not found
	return -1;
}

/** Emits 2 bytes to retrieve or set a global variable's value and place it on the stack.
 * 
 * @param[in] name The name of the variable to retrieve.
 */
static void namedVariable(Token name, bool canAssign) {
	uint8_t getOp, setOp;
	// Attempt to find local with name
	int arg = resolveLocal(current, &name);

	// Local variable 
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(current, &name)) != -1) { // Captured Upvalue
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	} else { // Global
		arg = identifierConstant(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && match(TOKEN_EQUAL)) { // Check if it should be a setter right before we emit the opcode
		expression();
		emitBytes(setOp, (uint8_t) arg);
	} else {
		emitBytes(getOp, (uint8_t) arg);
	}
}


/** Retrieves the next available local variable slot and assigns a name and depth.
 * 
 * @param[in] name The name to assign to the local variable.
 */
static void addLocal(Token name) {
	if (current->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}
	
	Local* local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->isCaptured = false;
}

/** Converts the (simulated) top stack element into a local variable if it isn't global.
 * Uses the last read token as the name. Will cause compile error if name is reused.
 * Adds variable to scope.
 */
static void declareVariable() {
	// Globals are placed into the constant pool, so we don't need to do anything special here
	if (current->scopeDepth == 0) {
		return;
	}

	Token* name = &parser.previous;
	// Ensure local isn't already defined. Start at back and decrease index b/c current scoped items are there
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local* local = &current->locals[i]; // Get next local
		// If we walk to a different scope than the one we are interested in, break b/c we are out of the current scope
		if (local->depth != -1 && local->depth < current->scopeDepth) {
			break;
		}
		// Currently scoped var. Check names
		if (identifiersEqual(name, &local->name)) {
			error("Already a variable with this name in scope.");
		}
	}
	addLocal(*name);
}

//~ Grammar Evaluation
// The following functions only greedily evaluate their productions. They do not 'feed' into one another or care about precidence.
// For example, unary will parse `-a.b + c` like `-(a.b + c)` when it should be `-(a.b) + c` because of the call to expression().
// Other structures are required to ensure each function only consumes what it should.

/** Parses a variable and returns the index of the variable in the constant pool.
 * 
 * @param[in] errorMessage The message to show if the variable name is missing.
 * @return uint8_t index of the variable in the constant pool
 */
static uint8_t parseVariable(const char* errorMessage) {
	consume(TOKEN_IDENTIFIER, errorMessage);

	declareVariable();
	// If local, var will be placed on stack, so dont give the global location
	if (current->scopeDepth > 0)
		return 0;

	return identifierConstant(&parser.previous);
}

/** Emits a constant number value to the current chunk. 
 *  @pre The number token is in the parser.previous position.
 */
static void number(bool canAssign) {
	// Convert the lexeme from the last token to a number.
	//? How is the end of the lexeme marked
	double value = strtod(parser.previous.start, NULL);
	emitConstant(NUMBER_VAL(value));
}

/** Emits a string constant copied from the input source. The string is stored in the constant pool as all Values are.
 *
 * @details
 * The allocated Value is stored in the constant pool, but the actual char array behind the string is heap allocated by C. The constant pool value contains a pointer to 
 * the heap allocated char array. The value in the constant pool is indexed in the same way as numerical and boolean constants.
 */
static void string(bool canAssign) {
	emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/** Emits a constant literal to the current chunk.
 * @pre The literal token is in the parser.previous position.
 *
 */
static void literal(bool canAssign) {
	switch (parser.previous.type) {
		case TOKEN_FALSE:
			emitByte(OP_FALSE);
			break;
		case TOKEN_TRUE:
			emitByte(OP_TRUE);
			break;
		case TOKEN_NIL:
			emitByte(OP_NIL);
			break;
		default:
			return;
	}
}

/** Parses an expression which culminates in a right parentheses.
 * @pre The left parentheses has already been consumed.
 */
static void grouping(bool canAssign) {
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/** Parses and emits a binary expression.
 * @pre The left operand has already been emitted and the operand has just been consumed.
 */
static void binary(bool canAssign) {
	// Save type of operation
	TokenType operatorType = parser.previous.type;
	// Find the precidence of the operator
	ParseRule* rule = getRule(operatorType);
	// Parse an expression at the precidence level of one above the operator for left associative oerators.
	// This will place the next operation on the stack
	parsePrecidence((Precidence) (rule->precidence + 1));

	// Emit actual binary operation
	switch (operatorType) {
		case TOKEN_PLUS:
			emitByte(OP_ADD);
			break;
	 	case TOKEN_MINUS:
			emitByte(OP_SUBTRACT);
			break;
	 	case TOKEN_STAR:
			emitByte(OP_MULTIPLY);
			break;
	 	case TOKEN_SLASH:
			emitByte(OP_DIVIDE);
			break;
	 	case TOKEN_EQUAL_EQUAL:
			emitByte(OP_EQUAL);
			break;
	 	case TOKEN_BANG_EQUAL:
			emitBytes(OP_EQUAL, OP_NOT);
			break;
	 	case TOKEN_GREATER:
			emitByte(OP_GREATER);
			break;
	 	case TOKEN_GREATER_EQUAL:
			emitBytes(OP_LESSER, OP_NOT);
			break;
	 	case TOKEN_LESSER:
			emitByte(OP_LESSER);
			break;
	 	case TOKEN_LESSER_EQUAL:
			emitBytes(OP_GREATER, OP_NOT);
			break;
		default: // Impossible
			return;
	}
}

/** Parses a number of arguments and returns the number of arguments parsed.
 * @details
 * Parsed arguments are left on the stack
 */
static uint8_t argumentList() {
	uint8_t argCount = 0;
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			expression();
			argCount++;
			if (argCount >= 255) { // b/c using uint8_t exclusively
				error("Cannot have more than 255 arguments.");
			}
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

/** Parses a function call and corresponding arguments.
 * @pre Assumes '(' has already been consumed.
 * @param[in] canAssign unused.
 */
static void call(bool canAssign) {
	uint8_t argCount = argumentList(); // The number of elements on the stack to take as input
	emitBytes(OP_CALL, argCount);
}

/** Parses a property get or set.
 * @param[in] canAssign If true, dot indicates an assignment. Otherwise a get is parsed
 */
static void dot(bool canAssign) {
	consume(TOKEN_IDENTIFIER, "Expected property name after '.'.");
	uint8_t name = identifierConstant(&parser.previous); // property name

	// Check if an assignment is being parsed and if an assignment can even occur
	if (canAssign && match(TOKEN_EQUAL)) {
		expression();
		emitBytes(OP_SET_PROPERTY, name);
	} else {
		emitBytes(OP_GET_PROPERTY, name);
	}
}

/** Parse a disjunction expression.
 * @details
 * Performes short circuit evaluation. If the left operand is false, the right operand is not evaluated.
 * Assumes the left operand is already on the stack and 'and' has been consumed.
 * @param[in] canAssign unused.
 */
static void and_(bool canAssign) {
	int endJump = emitJump(OP_JUMP_IF_FALSE); // If left is false, skip the right
	emitByte(OP_POP); // Pop the left operand. Ensures entire evaluation only has 1 operand left on stack
	parsePrecidence(PREC_AND); // Parse the right operand
	patchJump(endJump); // Jump to here if left is false
}

/** Parse a conjunction expression.
 * @details
 * Performes short circuit evaluation. If the left operand is true, the right operand is not evaluated.
 * Assumes the left operand is already on the stack and 'or' has been consumed.
 * @param[in] canAssign unused.
 */
static void or_(bool canAssign) {
	int elseJump = emitJump(OP_JUMP_IF_FALSE); // If left is false, jump to the right operand
	int endJump = emitJump(OP_JUMP); // Jump to the end if left is true
	
	patchJump(elseJump); // Jump to here if left is false
	emitByte(OP_POP); // Pop the left operand. Ensures entire evaluation only has 1 operand left on stack
	
	parsePrecidence(PREC_OR); // Parse the right operand
	patchJump(endJump); // Jump to here if left is true
}

/** Parses a unary expression.
 * @pre The operator has already been consumed.
 */
static void unary(bool canAssign) {
	TokenType operatorType = parser.previous.type;

	// Compile the operand at unary precidence level or higher. Allowing the same precidence level allows nesting of operations.
	parsePrecidence(PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType) {
		case TOKEN_MINUS:
			emitByte(OP_NEGATE);
			break;
		case TOKEN_BANG:
			emitByte(OP_NOT);
			break;
		default: // Impossible
			return;
	}
}

/** Parses a variable.
 * @details
 * Assumes the variable has already been consumed and is in `parser.previous`.
 */
static void variable(bool canAssign) {
	namedVariable(parser.previous, canAssign);
}

/** Emits instructions/ alters state to implement the 'this' keyword.
 * @pre The 'this' keyword has already been consumed.
 * @param[in] canAssign
 */
static void this_(bool canAssign) {
	if (currentClass == NULL) {
		error("Cannot use 'this' outside of a class.");
		return;
	}
	variable(false); // simply create a variable called 'this'
}

static void function(FunctionType type) {
	Compiler compiler;
	initCompiler(&compiler, type);
	beginScope();

	// Parse the parameter list
	consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255) {
				error("Cannot have more than 255 parameters.");
			}
			uint8_t constant = parseVariable("Expect parameter name.");
			defineVariable(constant); // Do not initialize. Initialization will occur when passing functions
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
	consume(TOKEN_LEFT_CURLY, "Expect '{' before function body.");

	// Compile the function body
	block();

	// Finish compiling and create the function object constant
	// Note: Because we end the compiler, there is no corresponding endScope(). Placing an endScope() would simply add more bytecode to pop locals with no benefit
	ObjFunction* function = endCompiler();
	// Emit the constant onto the stack

	emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
	// For every upvalue captured, emit its locality and index
	for (int i = 0; i < function->upvalueCount; i++) {
		emitByte(compiler.upvalues[i].isLocal ? 1 : 0); // Flag on whether the variable is a local or not
		emitByte(compiler.upvalues[i].index); // index of the variable in the stack
	}
}

// Declared after all function declarations so they can be placed into the table.
/** Singleton representing the functions to call when a token is encountered when parsing an expression and the precidence level to parse for binary expressions. 
 * Literals are included in this table with the 'unary' slot representing the function to parse the literal.
 * Precidence column is used for the infix precedence of the operator. If some prefix (or postfix) operators had different precidence levels, their precidences would
 * also need to be stored.
 */
ParseRule rules[] = {  // PREFIX     INFIX    PRECIDENCE (INFIX) */
	[TOKEN_LEFT_PAREN]    = {grouping, call,    PREC_CALL},
	[TOKEN_RIGHT_PAREN]   = {NULL,     NULL,    PREC_NONE},
	[TOKEN_LEFT_CURLY]    = {NULL,     NULL,    PREC_NONE}, 
	[TOKEN_RIGHT_CURLY]   = {NULL,     NULL,    PREC_NONE},
	[TOKEN_COMMA]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_DOT]           = {NULL,     dot,    	PREC_CALL},
	[TOKEN_MINUS]         = {unary,    binary,  PREC_TERM},
	[TOKEN_PLUS]          = {NULL,     binary,  PREC_TERM},
	[TOKEN_SEMICOLON]     = {NULL,     NULL,    PREC_NONE},
	[TOKEN_SLASH]         = {NULL,     binary,  PREC_FACTOR},
	[TOKEN_STAR]          = {NULL,     binary,  PREC_FACTOR},
	[TOKEN_BANG]          = {unary,    NULL,    PREC_NONE},
	[TOKEN_BANG_EQUAL]    = {NULL,     binary,  PREC_EQUALITY},
	[TOKEN_EQUAL]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_EQUAL_EQUAL]   = {NULL,     binary,  PREC_EQUALITY},
	[TOKEN_GREATER]       = {NULL,     binary,  PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] = {NULL,     binary,  PREC_COMPARISON},
	[TOKEN_LESSER]        = {NULL,     binary,  PREC_COMPARISON},
	[TOKEN_LESSER_EQUAL]  = {NULL,     binary,  PREC_COMPARISON},
	[TOKEN_IDENTIFIER]    = {variable, NULL,    PREC_NONE},
	[TOKEN_STRING]        = {string,   NULL,    PREC_NONE},
	[TOKEN_NUMBER]        = {number,   NULL,    PREC_NONE}, // Literals also appear as an 'operator
	[TOKEN_AND]           = {NULL,     and_,    PREC_AND},
	[TOKEN_CLASS]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_ELSE]          = {NULL,     NULL,    PREC_NONE},
	[TOKEN_FALSE]         = {literal,  NULL,    PREC_NONE},
	[TOKEN_FOR]           = {NULL,     NULL,    PREC_NONE},
	[TOKEN_FUN]           = {NULL,     NULL,    PREC_NONE},
	[TOKEN_IF]            = {NULL,     NULL,    PREC_NONE},
	[TOKEN_NIL]           = {literal,  NULL,    PREC_NONE},
	[TOKEN_OR]            = {NULL,     or_,    	PREC_OR},
	[TOKEN_PRINT]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_RETURN]        = {NULL,     NULL,    PREC_NONE},
	[TOKEN_SUPER]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_THIS]          = {this_,    NULL,    PREC_NONE},
	[TOKEN_TRUE]          = {literal,  NULL,    PREC_NONE},
	[TOKEN_VAR]           = {NULL,     NULL,    PREC_NONE},
	[TOKEN_WHILE]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_ERROR]         = {NULL,     NULL,    PREC_NONE},
	[TOKEN_EOF]           = {NULL,     NULL,    PREC_NONE},
};

/** Parses any expressions at the same precidence level or higher. 
 * Used to ensure the other parse functions do not greedily parse an entire expression.
 *
 * @param[in] precidence The precidence level to parse at or above.
 */
static void parsePrecidence(Precidence precidence) {

	// Parsing a prefix operator
	advance(); // Pass operator
	ParseFn prefixRule = getRule(parser.previous.type)->prefix; // Get prefix rule for operator just passed
	if (prefixRule == NULL) { // If there is no prefix rule for the operator, show an error
		error("Expected expression"); // Literals parse through the table, so by definition the expression must start with a 'unary'
		return;
	}

	// Tells prefix if it allowed to parse a '=' assignment. Also usable for infix '[', if I ever get there...
	bool canAssign = precidence <= PREC_ASSIGNMENT; // Whether we are at a low enough precidence to allow assignment in prefix

	// Call whatever function was retrieved
	prefixRule(canAssign);

	// While the precidence of the currently examined token is equal or greater than the input precidence, parse it
	// Loop relies on precidence being available and above the set precidence. If an unrecognized operator is found (has PREC_NONE in table), the function will end.
	while (precidence <= getRule(parser.current.type)->precidence) {
		advance(); // Get operator into parser.previous
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule(canAssign);
	}

	// Catch the case where the parser gets stuck with an unparsable '=' in one pass.
	if (canAssign && match(TOKEN_EQUAL)) {
		error("Invalid assignment target.");
	}
}

/** Retrieve the parse rule based off the input token type.
 * 
 * @param[in] type The token type to retrieve information from.
 * @return ParseRule pointer to the prefix, infix and infix precidence information of a token.
 */
static ParseRule* getRule(TokenType type) {
	// Function exists to allow `binary` et al. functions access to `rules` singleton
	return &rules[type];
}

/** Parses an expression starting at the lowest available precidence.
 * 
 */
static void expression() {
	// Lowest precidence is assignment, so this expression call will parse the entire expression
	parsePrecidence(PREC_ASSIGNMENT);
}

// ~ Statements

/** Parses a block of statements.
 * @details
 * Assumes the '}' has already been consumed.
 */
static void block() {
	while (!check(TOKEN_RIGHT_CURLY) && !check(TOKEN_EOF)) {
		declaration();
	}

	consume(TOKEN_RIGHT_CURLY, "Expected '}' after block.");
}

/** Parses a print statement.
 * @details
 * Assumes that the 'print' keyword has already been consumed.
 *
 */
static void printStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "Expected ';' after value.");
	emitByte(OP_PRINT);
}

/** Parses an expression statement.
 * 
 */
static void expressionStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "Expected ';' after value.");
	emitByte(OP_POP);
}

/** Parses an if statement with optional else.
 * @details
 * Assumes the 'if' keyword has already been consumed.
 */
static void ifStatement() {
	consume(TOKEN_LEFT_PAREN, "Expected '(' after 'if'.");
	expression(); // Condition. Is left on the stack.
	consume(TOKEN_RIGHT_PAREN, "Expected ')' after condition.");

	int thenJump = emitJump(OP_JUMP_IF_FALSE); // Instruction to jump if stack top is false. Saves the offset to be filled in later.
	emitByte(OP_POP); // Pop condition. Done before any true branch code.
	statement();

	int elseJump = emitJump(OP_JUMP); // Create the unconditional jump instruction for the true branch to be filled in later.
	
	patchJump(thenJump); // Replace the placeholder instruction with the actual jump amount
	emitByte(OP_POP); // Pop at start of else. NOTE: makes an implicit else branch
	
	if (match(TOKEN_ELSE)) {
		statement();
	}

	patchJump(elseJump);
}

/** Parses a new variable declaration.
 * @details
 * Assumes that the 'var' keyword has already been consumed.
 *
 */
static void varDeclaration() {
	uint8_t global = parseVariable("Expecteded variable name.");

	// Check for initializer expression
	if (match(TOKEN_EQUAL)) {
		expression();
	} else {
		emitByte(OP_NIL);
	}

	consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");
	defineVariable(global);
}

/** Parses a funciton declaration.
 * @details
 * Assumes that the 'fun' keyword has already been consumed.
 */
static void funDeclaration() {
	uint8_t global = parseVariable("Expected function name.");
	markInitialized(); // Allow recursion
	function(TYPE_FUNCTION); // Functions are first class, so this simply places one on the stack
	defineVariable(global); // Define the variable;
}

static void classDeclaration() {
	consume(TOKEN_IDENTIFIER, "Expected class name.");
	Token className = parser.previous;
	uint8_t name = identifierConstant(&parser.previous); // Place class name into constant pool so it can be printed later

	declareVariable(); // Binds class object to a variable of the same name. ADDS VAR TO SCOPE
	// Declare before class body so it can be used in the body.
	emitBytes(OP_CLASS, name); // Emit instruction to create class at runtime
	defineVariable(name); // Define variable for class's name

	// We need to track the innermost class compiler so we can determine if 'this' is valid
	ClassCompiler classCompiler; // Class compiler for this class
	classCompiler.enclosing = currentClass; // Save old innermost class
	currentClass = &classCompiler; // Set the current class compiler to be the innermost

	namedVariable(className, false); // Place class name on the stack
	consume(TOKEN_LEFT_CURLY, "Expected '{' before class body.");

	while (!check(TOKEN_RIGHT_CURLY) && !check(TOKEN_EOF)) {
		method();
	}

	consume(TOKEN_RIGHT_CURLY, "Expected '}' after class body.");
	emitByte(OP_POP); // Pop class name
	currentClass = currentClass->enclosing; // Restore innermost class
}

static void method() {
	// Name of the method
	consume(TOKEN_IDENTIFIER, "Expected method name.");
	uint8_t constant = identifierConstant(&parser.previous); // place method name into constant pool

	// Closure for method body
	FunctionType type = TYPE_METHOD;
	// Check if initialzer -> want to disallow explicit (user) return and have implicit 'return this' instead of 'return nil'
	if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	function(type); // Place closure on the stack

	// Bind method to class
	emitBytes(OP_METHOD, constant);
}

/** Parses an while statement.
 * @details
 * Assumes the 'while' keyword has already been consumed.
 */
static void whileStatement() {
	int loopStart = currentChunk()->count; // Save the current byte offset for the loop start.
	consume(TOKEN_LEFT_PAREN, "Expected '(' after 'while'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expected ')' after condition.");

	int exitJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP); // Pop condition
	statement(); // Do anything in the while loop
	emitLoop(loopStart); // Jump back to the start of the loop

	patchJump(exitJump);
	emitByte(OP_POP); // Pop condition
}

/** Parsesd a for loop.
 * @details
 * Assumes the 'for' keyword has already been consumed.
 */
static void forStatement() {
	beginScope(); // Begin a new scope for the loop
	consume(TOKEN_LEFT_PAREN, "Expected '(' after 'for'.");

	// Optional initializer
	if (match(TOKEN_SEMICOLON)) {
		// No initializer
	} else if (match(TOKEN_VAR)) {
		varDeclaration();
	} else {
		expressionStatement();
	}
	
	// Optional condition
	int loopStart = currentChunk()->count;
	int exitJump = -1;
	if (!match(TOKEN_SEMICOLON)) { // If no condition, this will consume the ';'
		expression();
		consume(TOKEN_SEMICOLON, "Expected ';' after loop condition.");
		
		exitJump = emitJump(OP_JUMP_IF_FALSE);
		emitByte(OP_POP); // Pop condition
	}

	// Optional increment
	// Works by jumping OVER the increment, executing the body, and then returning to the increment. After that, it jumps to the condition
	if (!match(TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(OP_JUMP); // Skip the increment for now
		int incrementStart = currentChunk()->count;
		expression();
		emitByte(OP_POP); // Pop expr. statement result
		consume(TOKEN_RIGHT_PAREN, "Expected ')' after for clauses.");

		emitLoop(loopStart); // Jump back to the condition. If condition is not there, it will jump to the unconditional jump to the body
		loopStart = incrementStart; // Body will now jump to the increment start
		patchJump(bodyJump); // Jump here to execute the body
	}

	// Body and jump back to condition
	statement();
	emitLoop(loopStart);

	if (exitJump != -1) {
		patchJump(exitJump);
		emitByte(OP_POP); // Pop condition
	}

	endScope();
}

static void returnStatement() {
	if (current->type == TYPE_SCRIPT) {
		error("Cannot return from top-level code.");
	}
	if (match(TOKEN_SEMICOLON)) {
		emitReturn();
	} else {
		if (current->type == TYPE_INITIALIZER) {
			error("Cannot return a value from an initializer.");
		}
		
		expression();
		consume(TOKEN_SEMICOLON, "Expected ';' after return value.");
		emitByte(OP_RETURN);
	}
}

/** Parses a statement.
 * 
 */
static void statement() {
	if (match(TOKEN_PRINT)) {
		printStatement();
	} else if (match(TOKEN_LEFT_CURLY)) {
		beginScope();
		block();
		endScope();
	} else if (match(TOKEN_IF)) {
		ifStatement();
	} else if (match(TOKEN_WHILE)) {
		whileStatement();
	} else if (match(TOKEN_FOR)) {
		forStatement();
	} else if (match(TOKEN_RETURN)) {
		returnStatement();
	} else {
		expressionStatement();
	}
}

/** Parses a declaration statement.
 * 
 */
static void declaration() {
	if (match(TOKEN_VAR)) {
		varDeclaration();
	} else if (match(TOKEN_FUN)) {
		funDeclaration();
	} else if (match(TOKEN_CLASS)) {
		classDeclaration();
	} else {
		statement();
	}
	
	if (parser.panicMode)
		synchronize();
}

//~ Compilation Functions

ObjFunction* compile(const char* source) {
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler, TYPE_SCRIPT);

	// Initialize the parser
	parser.hadError = false;
	parser.panicMode = false;

	advance(); // 'Prime' the scanner

	// A program is a series of declaration
	while (!match(TOKEN_EOF)) {
		declaration();
	}

	ObjFunction* function = endCompiler();
	return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
	Compiler* compiler = current;
	while (compiler != NULL) {
		markObject((Obj*) compiler->function);
		compiler = compiler->enclosing;
	}
}
