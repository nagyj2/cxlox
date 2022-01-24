#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

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
	PREC_COMMA,				//> ,
	PREC_ASSIGNMENT,	//> =
	PREC_CONDITIONAL,	//> ?:
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

/* Function type which takes no arguments and no return. Used to store the desired function in ParseRule struct. */
typedef void (*ParseFn)();

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

// Parser singleton.
Parser parser;
// The chunk which is currently getting bytecode emitted to it.
Chunk* compilingChunk;

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

/** Places a constant into the current chunk's constant pool. 
 * Causes an error if the number of constants in the chunk exceeds the maximum.
 *
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
	emitBytes(OP_CONSTANT, makeConstant(value));
}

//~ Grammar Evaluation
// The following functions only greedily evaluate their productions. They do not 'feed' into one another or care about precidence.
// For example, unary will parse `-a.b + c` like `-(a.b + c)` when it should be `-(a.b) + c` because of the call to expression().
// Other structures are required to ensure each function only consumes what it should.

// Forward declares to allow references.
static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecidence(Precidence precidence);

/** Emits a constant value to the current chunk. Assumes the number token is in the parser.previous position.
 * 
 */
static void number() {
	// Convert the lexeme from the last token to a number.
	//? How is the end of the lexeme marked
	double value = strtod(parser.previous.start, NULL);
	emitConstant(value);
}

/** Parses an expression which culminates in a right parentheses. Assumes the left parentheses has already been consumed.
 * 
 */
static void grouping() {
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/** Parses and emits a binary expression. Assumes the left operand has already been emitted and the operand has just been consumed.
 * 
 */
static void binary() {
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
		default: // Impossible
			return;
	}
}

/** Parses a unary expression. Assumes the operator has already been consumed.
 * 
 */
static void unary() {
	TokenType operatorType = parser.previous.type;

	// Compile the operand at unary precidence level or higher. Allowing the same precidence level allows nesting of operations.
	parsePrecidence(PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType) {
		case TOKEN_MINUS:
			emitByte(OP_NEGATE);
			break;
		default: // Impossible
			return;
	}
}

//~ Extra Parsing

/** Parses the rhs of a comma expression. Assumes the comma has already been consumed by parsePrecidence.
 * 
 */
static void comma() {
	// TokenType operatorType = parser.previous.type; // Always a comma token
	
	parsePrecidence(PREC_COMMA);

	// Do something...
}

/** Parses a ternary statement. Assumes the condition and '?' has already been consumed by parse precidence.
 * 
 */
static void ternary() {
	TokenType operatorType = parser.previous.type;

	parsePrecidence(PREC_CONDITIONAL + 1); 

	consume(TOKEN_COLON, "Expect ':' after condition.");

	parsePrecidence(PREC_ASSIGNMENT); // Same precidence so it is right associative

	// Do something...
}

// Declared after all function declarations so they can be placed into the table.
/** Singleton representing the functions to call when a token is encountered when parsing an expression and the precidence level to parse for binary expressions. 
 * Literals are included in this table with the 'unary' slot representing the function to parse the literal.
 * Precidence column is used for the infix precedence of the operator. If some prefix (or postfix) operators had different precidence levels, their precidences would
 * also need to be stored.
 */
ParseRule rules[] = {  // PREFIX     INFIX    PRECIDENCE */
	[TOKEN_LEFT_PAREN]    = {grouping, NULL,    PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,    PREC_NONE},
  [TOKEN_LEFT_CURLY]    = {NULL,     NULL,    PREC_NONE}, 
  [TOKEN_RIGHT_CURLY]   = {NULL,     NULL,    PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,    PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary,  PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary,  PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,    PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary,  PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary,  PREC_FACTOR},
  [TOKEN_QUESTION]      = {NULL,     ternary, PREC_CONDITIONAL},	// For conditional ternary
  [TOKEN_COLON]         = {NULL,     NULL,    PREC_NONE},					// To stop pratt parsing 
	[TOKEN_COMMA]         = {NULL,     comma,   PREC_COMMA},				// For comma operator
  [TOKEN_BANG]          = {NULL,     NULL,    PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     NULL,    PREC_NONE},
  [TOKEN_EQUAL]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     NULL,    PREC_NONE},
  [TOKEN_GREATER]       = {NULL,     NULL,    PREC_NONE},
  [TOKEN_GREATER_EQUAL] = {NULL,     NULL,    PREC_NONE},
  [TOKEN_LESSER]        = {NULL,     NULL,    PREC_NONE},
  [TOKEN_LESSER_EQUAL]  = {NULL,     NULL,    PREC_NONE},
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,    PREC_NONE},
  [TOKEN_STRING]        = {NULL,     NULL,    PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,    PREC_NONE}, // Literals also appear as an 'operator
  [TOKEN_AND]           = {NULL,     NULL,    PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,    PREC_NONE},
  [TOKEN_FALSE]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,    PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,    PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,    PREC_NONE},
  [TOKEN_NIL]           = {NULL,     NULL,    PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,    PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,    PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,    PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,    PREC_NONE},
  [TOKEN_TRUE]          = {NULL,     NULL,    PREC_NONE},
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

	// Call whatever function was retrieved
	prefixRule();

	// While the precidence of the currently examined token is equal or greater than the input precidence, parse it
	// Loop relies on precidence being available and above the set precidence. If an unrecognized operator is found (has PREC_NONE in table), the function will end.
	while (precidence <= getRule(parser.current.type)->precidence) {
		advance(); // Get operator into parser.previous
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule();
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

	// Initialize the compiling chunk
	compilingChunk = chunk;

	// Initialize the parser
	parser.hadError = false;
	parser.panicMode = false;

	advance(); // 'Prime' the scanner
	expression(); // For now, just expressions
	consume(TOKEN_EOF, "Expect end of expression.");

	endCompiler();
	return !parser.hadError;
}
