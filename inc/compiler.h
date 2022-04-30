#ifndef clox_compiler_h
#define clox_compiler_h

#include "common.h"
#include "vm.h"
#include "object.h"
#include "scanner.h"
#include "table.h"

// Point where long index constants are stored.
#define CONST_TO_LONG_CONST UINT8_MAX
// Maximum number of constants in a chunk.
#define MAX_CONSTANTS_PER_CHUNK ((2 << 24) - 1)
// Max number of breaks available in a chunk at any one time.
#define MAX_BREAKS UINT8_MAX
// Max number of cases available in a case statement
#define MAX_CASES UINT8_MAX

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
	PREC_CALL,				//> . () .?
	PREC_PRIMARY
} Precidence;

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
	bool constant;		//* Whether or not the variable is constant.
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
	bool constant;		//* Whether or not the upvalue is constant.
} Upvalue;

/** Captures the innermost class being compiled.
 */
typedef struct ClassCompiler {
	struct ClassCompiler* enclosing;
	bool hasSuperclass;
	Token name;
} ClassCompiler;

/* Singleton representing the currently parsing and previously parsed tokens. */
typedef struct {
	VM* vm;							//* The vm the parser belongs to
	Scanner scanner;		//* The scanner used to parse the source code.
	
	Token current;			//* The parser currently being parsed.
	Token previous;			//* The most recently parsed token.
	bool hadError;			//* Whether or not an error has occurred.
	bool panicMode;			//* If true, the parser will discard tokens until a synchrnization point is found.
	ObjModule* module;	//* The module being compiled.
} Parser;


/** State for the compiler.
 * @note The size of locals should be the exact same size as the stack. If one changes, the other must as well.
 */
struct _Compiler {
	Parser* parser;									//* Parser for this compilation
	Table stringConstants;					//* String internment table for the current VM

	Compiler* enclosing;						//* The compiler before the current one.
	ClassCompiler* class;						//* The outermost class being compiled.
	
	ObjFunction* function;					//* The function being compiled.
	FunctionType type;							//* The type of function being compiled.
	
	Local locals[UINT8_COUNT]; 			//* Array of local variables. Length is fixed at 256 due to 8 bit indexes.
	int localCount; 								//* Number of local variables currently in scope.
	Upvalue upvalues[UINT8_COUNT];	//* Upvalue array for the current closure.

	int scopeDepth; 								//* Depth of of the scope where the compiler currently is. How 'far' the scope is from the global scope.

	int recentLoop;									//* The most recent loop position
	int numBreak;										//* The number of break positions
	int recentBreak[MAX_BREAKS];		//* The most recent break position

	bool inCall;										//* Whether or not the compiler is parsing a function call
};

// Function type which takes one argument and no return. Used to store the desired function in ParseRule struct.
typedef void (*ParsePrefixFn)(Compiler* compiler, bool canAssign);
typedef void (*ParseInfixFn)(Compiler* compiler, bool canAssign);

/** For any operation which starts with a token, this structure contains the: 
 * the function to compile a prefix expression starting with a token of that type, 
 * the function to compile an infix expression whose left operand is followed by a token of that type, 
 *the precedence of an infix expression that uses that token as an operator.
 */
typedef struct {
	ParsePrefixFn prefix;
	ParseInfixFn infix;
	Precidence precidence;
} ParseRule;

/** Tokenizes, parses and writes bytecode for the input source code and placed it into a chunk.
 * @param[in] source The source code to compile.
 * @return ObjFunction* The compiled code corresponding to the script or function.
 */
ObjFunction* compile(VM* vm, ObjModule* module, const char* source);

/** Marks the currently compiling functions and closures as reachable.
 */
void markCompilerRoots(VM* vm);

#endif /* clox_compiler_h */
