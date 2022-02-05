#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "vm.h"
#include "object.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

// Point where long index constants are stored.
#define CONST_TO_LONG_CONST UINT8_MAX
// Maximum number of constants in a chunk.
#define MAX_CONSTANTS_PER_CHUNK ((2 << 24) - 1)

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
	PREC_COMMA,				//> ,
	PREC_ASSIGNMENT,	//> =
	PREC_OPTIONAL,		//> :
	PREC_CONDITIONAL,	//> ?
	PREC_OR,					//> or
	PREC_AND,					//> and
	PREC_EQUALITY,		//> == !=
	PREC_COMPARISON,	//> < > <= >=
	PREC_TERM,				//> + -
	PREC_FACTOR,			//> * /
	PREC_UNARY,				//> - !
	PREC_CALL,				//> . ()
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
 */
typedef struct {
	Token name;			//* Variable name.
	int depth;			//* Depth of the scope in which the variable is declared from the global scope.
} Local;

/** State for the compiler.
 * @note The size of locals should be the exact same size as the stack. If one changes, the other must as well.
 */
typedef struct {
	Local locals[UINT8_COUNT]; 	//* Array of local variables. Length is fixed at 256 due to 8 bit indexes.
	int localCount; 						//* Number of local variables currently in scope.
	int scopeDepth; 						//* Depth of of the scope where the compiler currently is. How 'far' the scope is from the global scope.
} Compiler;

// Parser singleton.
Parser parser;
// Current compiler singleton.
Compiler* current = NULL;
// The chunk which is currently getting bytecode emitted to it.
Chunk* compilingChunk;

// Forward declares to allow references.
static void expression();
static void statement();
static void declaration();
static void varDeclaration();
static void expressionStatement();
static void printStatement();
static void ifStatement();
static void whileStatement();
static void forStatement();
static ParseRule* getRule(TokenType type);
static void parsePrecidence(Precidence precidence);

/** The chunk which is currently getting bytecode emitted to it.
 * 
 * @return Chunk* pointer to the chunk currently being emitted to.
 */
static Chunk* currentChunk() {
	return compilingChunk;
}

//~ Error functions

/** Prints an error to stderr. Includes line number information.
 * 
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
 *
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
 * 
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
 * 
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
 * Causes an error if the number of constants in the chunk exceeds the maximum.
 *
 * @param[in] value The value to write to the constant pool.
 * @return uint8_t byte representing the index of the constant in the constant pool.
 */
static int makeConstant(Value value) {
	int constant = addConstant(currentChunk(), value);
	if (constant > MAX_CONSTANTS_PER_CHUNK) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return constant;
}

/** Emits a return opcode.
 * 
 */
static void emitReturn() {
	emitByte(OP_RETURN);
}

/** Emits 2 bytes which correspond to the creation of a constant value.
 * 
 * @param[in] value The value to write.
 */
static void emitConstant(Value value) {
	int index = makeConstant(value);
	if (index < CONST_TO_LONG_CONST) {
		emitBytes(OP_CONSTANT, index);
	} else {
		// Emit opcode and then the 3 bytes of the index.
		emitBytes(OP_CONSTANT_LONG, (uint8_t) (index & 0xff));
		emitBytes((uint8_t) ((index >> 8) & 0xff), (uint8_t) ((index >> 16) & 0xff));
	}
}

/** Initializes the state of the compiler and sets it to be the current compiler.
 * 
 * @param[in,out] compiler The compiler to initialize.
 */
static void initCompiler(Compiler* compiler) {
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	current = compiler;
}

/** Signals the start of a new scope to the current compiler singleton.
 * 
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
	int oldLocals = 0;
	while (current->localCount > 0
		&& current->locals[current->localCount - 1].depth > current->scopeDepth) {
		current->localCount--;
		oldLocals++;
	}
	
	if (oldLocals == 0)
		return;
	else if (oldLocals == 1)
		emitByte(OP_POP);
	else
		emitBytes(OP_POPN, (uint8_t) oldLocals);
}

/** Declare the top most local variable available for use by removing the sentinel -1 depth.
 * 
 */
static void markInitialized() {
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/** Emits 2 bytes to define a new global variable.
 * 
 * @param[in] global The index to the constant pool location of the global variable's name.
 */
static void defineVariable(uint8_t global) {
	// If a local, don't put into constant pool
	if (current->scopeDepth > 0) {
		markInitialized();
		return; // locals dont need any new bytes b/c their value is already on top of the stack
	}
	
	if (global > CONST_TO_LONG_CONST) {
		emitBytes(OP_DEFINE_GLOBAL_LONG, (uint8_t) (global & 0xff));
		emitBytes((uint8_t) ((global >> 8) & 0xff), (uint8_t) ((global >> 16) & 0xff));
	} else {
		emitBytes(OP_DEFINE_GLOBAL, global);
	}
}

/** Creates a string constant for an identifier, places it in the constant pool, and returns the index of the constant.
 * 
 * @param[in] name The name of the variable to declare.
 * @return uint8_t 
 */
static int identifierConstant(Token* name) {
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

/** Emits bytes to save or load a indexed variable.
 * @details
 * If the index is over the maximum for for a single byte, the index is split into three bytes.
 *
 * @param[in] index The index of the variable.
 * @param[in] opcode The opcode to perform on the variable.
 */
static void emitLocalIndexed(int index, uint8_t opcode) {
	if (index > CONST_TO_LONG_CONST) {
		emitBytes(opcode, (uint8_t) (index & 0xff));
		emitBytes((uint8_t) ((index >> 8) & 0xff), (uint8_t) ((index >> 16) & 0xff));
	}	else {
		emitBytes(opcode, (uint8_t) index);
	}
}

/** Emits 2 bytes to retrieve or set a global variable's value.
 * @details
 * Parses the 'assignment' production in the grammar.
 *
 * @param[in] name The name of the variable to retrieve.
 */
static void namedVariable(Token name, bool canAssign) {
	uint8_t getOp, setOp;
	// Attempt to find local with name
	int index = resolveLocal(current, &name);
	
	if (index != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else {
		index = identifierConstant(&name);
		if (index > CONST_TO_LONG_CONST) {
			getOp = OP_GET_GLOBAL_LONG;
			setOp = OP_SET_GLOBAL_LONG;
		} else {
			getOp = OP_GET_GLOBAL;
			setOp = OP_SET_GLOBAL;
		}
	}

	if (canAssign && (match(TOKEN_EQUAL) || match(TOKEN_MINUS_EQUAL) || match(TOKEN_PLUS_EQUAL) || match(TOKEN_STAR_EQUAL) || match(TOKEN_SLASH_EQUAL))) { // Check if it should be a setter right before we emit the opcode
		TokenType operator = parser.previous.type;
		switch (parser.previous.type) {
			case TOKEN_MINUS_EQUAL: {
				emitLocalIndexed(index, getOp);
				expression();
				emitByte(OP_SUBTRACT);
				break;
			}
			case TOKEN_PLUS_EQUAL: {
				emitLocalIndexed(index, getOp);
				expression();
				emitByte(OP_ADD);
				break;
			}
			case TOKEN_STAR_EQUAL: {
				emitLocalIndexed(index, getOp);
				expression();
				emitByte(OP_MULTIPLY);
				break;
			}
			case TOKEN_SLASH_EQUAL: {
				emitLocalIndexed(index, getOp);
				expression();
				emitByte(OP_DIVIDE);
				break;
			}
			default:
				expression();
				break;
		}
		emitLocalIndexed(index, setOp);
		
	} else {
		emitLocalIndexed(index, getOp);
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
}

/** Converts the (simulated) top stack element into a local variable.
 * 
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

//~ Extra Parsing

/** Parses the rhs of a comma expression. Assumes the comma has already been consumed by parsePrecidence.
 * 
 */
static void comma(bool canAssign) {
	// TokenType operatorType = parser.previous.type; // Always a comma token
	emitByte(OP_POP);
	
	parsePrecidence(PREC_COMMA);

	// Do something...
}

/** Parses a conditional expression. Assumes the condition and '?' has already been consumed by parse precidence.
 * 
 */
static void conditional(bool canAssign) {
	TokenType operatorType = parser.previous.type;

	parsePrecidence(PREC_CONDITIONAL + 1);

	// consume(TOKEN_COLON, "Expect ':' after condition.");

	// parsePrecidence(PREC_ASSIGNMENT); // Same precidence so it is right associative

	// Do something...
	// emitBytes(OP_POP, OP_POP);
	emitByte(OP_CONDITIONAL);
}

/** Parses an optional statement. Assumes the test value and ':' has already been consumed by parse precidence.
 * 
 */
static void optional(bool canAssign) {
	TokenType operatorType = parser.previous.type;

	parsePrecidence(PREC_OPTIONAL); // Same precidence so it is right associative
	emitByte(OP_OPTIONAL);
}

/** Parses a variable.
 * @details
 * Assumes the variable has already been consumed and is in `parser.previous`.
 */
static void variable(bool canAssign) {
	namedVariable(parser.previous, canAssign);
}

// Declared after all function declarations so they can be placed into the table.
/** Singleton representing the functions to call when a token is encountered when parsing an expression and the precidence level to parse for binary expressions. 
 * Literals are included in this table with the 'unary' slot representing the function to parse the literal.
 * Precidence column is used for the infix precedence of the operator. If some prefix (or postfix) operators had different precidence levels, their precidences would
 * also need to be stored.
 */
ParseRule rules[] = {  // PREFIX     INFIX    		PRECIDENCE (INFIX) */
	[TOKEN_LEFT_PAREN]    = {grouping, NULL,    		PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_LEFT_CURLY]    = {NULL,     NULL,    		PREC_NONE}, 
  [TOKEN_RIGHT_CURLY]   = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary,  		PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary,  		PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary,  		PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary,  		PREC_FACTOR},
  [TOKEN_QUESTION]      = {NULL,     conditional, PREC_CONDITIONAL},	// For conditional expressions
  [TOKEN_COLON]         = {NULL,     optional,		PREC_OPTIONAL},			// For defaulted values 
	[TOKEN_COMMA]         = {NULL,     comma,   		PREC_COMMA},				// For comma operator
  [TOKEN_BANG]          = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_BANG]          = {unary,    NULL,    		PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary,  		PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary,  		PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary,  		PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary,  		PREC_COMPARISON},
  [TOKEN_LESSER]        = {NULL,     binary,  		PREC_COMPARISON},
  [TOKEN_LESSER_EQUAL]  = {NULL,     binary,  		PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,    		PREC_NONE},
	[TOKEN_STRING]        = {string,   NULL,    		PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,    		PREC_NONE}, // Literals also appear as an 'operator
  [TOKEN_AND]           = {NULL,     and_,    		PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,    		PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,    		PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    			PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,    		PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,    		PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,    		PREC_NONE},
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

/** Parses an expression starting at the lowest available precidence.5
 * 
 */
static void expression() {
	// Lowest precidence is assignment, so this expression call will parse the entire expression
	parsePrecidence(PREC_COMMA); //! THIS MUST BE KEPT UP TO DATE WHEN NEW PRECIDENCE LEVELS ARE ADDED
}

// ~ Statements

/** Parses a statement ending semicolon. If in REPL mode, the semicolon is optional.
 * 
 */
static void REPLSemicolon() {
	if (!vm.isREPL || !check(TOKEN_EOF)) {
		consume(TOKEN_SEMICOLON, "Expected ';' after statement.");
	}
}

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
	REPLSemicolon(); // consume(TOKEN_SEMICOLON, "Expected ';' after value.");
	emitByte(OP_PRINT);
}

/** Parses an expression statement.
 * 
 */
static void expressionStatement() {
	expression();
	REPLSemicolon(); // consume(TOKEN_SEMICOLON, "Expected ';' after value.");
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

	REPLSemicolon(); // consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");
	defineVariable(global);
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
	} else {
		statement();
	}
	
	if (parser.panicMode)
		synchronize();
}

//~ Compilation Functions

/** Marks the end of a compilation.
 * 
 */
static void endCompiler() {
	emitReturn();

	// If debug is enabled, print the completed bytecode if there was no error.
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(), "code");
	}
#endif
}

bool compile(const char* source, Chunk* chunk) {
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler);

	// Initialize the compiling chunk
	compilingChunk = chunk;

	// Initialize the parser
	parser.hadError = false;
	parser.panicMode = false;

	advance(); // 'Prime' the scanner

	// A program is a series of declaration
	while (!match(TOKEN_EOF)) {
		declaration();
	}

	endCompiler();
	return !parser.hadError;
}

// print "Consume " + "This"; var a = 9; print a; a = 1; print a;
