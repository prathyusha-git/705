# OCaml Subset Interpreter
## CIS705 Graduate Project - Kansas State University, Fall 2024

A complete implementation of an interpreter for a subset of OCaml, featuring lexical analysis, parsing, type checking, and evaluation.

### Project Overview

This interpreter implements a subset of OCaml that includes:
- Basic types (int)
- User-defined algebraic data types
- Functions and recursive functions
- Pattern matching
- Product types (pairs)
- Let bindings

### Features

1. **Lexical Analysis**
   - Token recognition for keywords, identifiers, constructors
   - Handling of arithmetic operators, delimiters
   - Whitespace and comment handling
   - Error reporting for invalid tokens

2. **Parser**
   - Complete syntax tree construction
   - Support for:
     * Type declarations
     * Let bindings
     * Function definitions
     * Pattern matching expressions
     * Arithmetic expressions
   - Comprehensive error handling

3. **Type Checker**
   - Static type checking
   - Type inference for expressions
   - Environment management for types
   - Constructor type validation
   - Pattern matching exhaustiveness checking

4. **Interpreter**
   - Full evaluation of OCaml subset programs
   - Environment-based value management
   - Support for recursive functions
   - Pattern matching evaluation
   - Error handling during execution

### Example Usage

```ocaml
(* Type Declaration *)
type tree = | Leaf of int | Node of (tree * tree)

(* Function Definition *)
let rec sum_tree (t : tree) =
    (match t with
    | Leaf n -> n
    | Node (l, r) -> (sum_tree l) + (sum_tree r)) ;;

(* Usage *)
let myTree = Node (Leaf 1, Node (Leaf 2, Leaf 3)) ;;
sum_tree myTree
```

### Installation and Running

1. Ensure you have OCaml installed on your system
2. Clone this repository
3. Running the Interpreter
   Start OCaml interactive mode (REPL):
   ocaml
4. Load our interpreter:
   #use "final.ml";;
 
### Project Structure

- `final.ml`: Main interpreter implementation
  * Lexical analyzer (Scanner)
  * Parser
  * Type checker
  * Evaluator
  * Environment management
  * Error handling

### Error Handling

The interpreter provides detailed error messages for:
- Syntax errors
- Type mismatches
- Undefined variables
- Constructor misuse
- Pattern matching failures

### Limitations

This implementation is a subset of OCaml and has the following limitations:
- No polymorphic types
- Limited to basic arithmetic operations (+, -, *)
- No module system
- No mutable references
- No exception handling

### Testing

The interpreter has been tested with various programs including:
- Basic arithmetic expressions
- Function definitions and applications
- Recursive functions
- Pattern matching with custom types
- Complex nested expressions

### Contributors

Ke Dong 

Prathyusha Mardhi

### Acknowledgments

This project was developed as part of the CIS705 course at Kansas State University, Fall 2024.
