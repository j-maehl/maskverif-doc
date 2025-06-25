# Comprehensive Documentation of the Project — All Files and Data Structures Explained

---

## Table of Contents

- [Comprehensive Documentation of the Project — All Files and Data Structures Explained](#comprehensive-documentation-of-the-project--all-files-and-data-structures-explained)
  - [Table of Contents](#table-of-contents)
  - [1. Overview](#1-overview)
    - [1.1 PP](#11-pp)
  - [2. File-by-File Explanation](#2-file-by-file-explanation)
    - [2.1. `main.ml`](#21-mainml)
    - [2.2. `ilang.ml`](#22-ilangml)
    - [2.3. `checker.ml`](#23-checkerml)
    - [2.4. `state.ml` (Data Structures Overview)](#24-stateml-data-structures-overview)
    - [2.5. `parsetree.ml` (Data Structures Overview)](#25-parsetreeml-data-structures-overview)
    - [2.6. `prog.ml` (Data Structures and Functions)](#26-progml-data-structures-and-functions)
      - [Data Structures:](#data-structures)
      - [Key Functions and Their Roles:](#key-functions-and-their-roles)
  - [3. Summary of Data Interactions](#3-summary-of-data-interactions)
  - [4. Additional Notes](#4-additional-notes)
- [End of Documentation](#end-of-documentation)

---

## 1. Overview

This documentation summarizes the architecture and internal workings of a system that processes masked cryptographic functions, verifies various security properties (like Non-Interference (NI), Strong Non-Interference (SNI), Threshold, etc.), and supports input from a custom intermediate language (ILANG). It includes:

- A REPL-style command interpreter.
- Parsing, lexing, and AST representation.
- Conversion to intermediate program representation.
- Symbolic internal state management.
- Checking algorithms with serial and parallel modes.
- A mechanism for macro expansion and detailed type/initialization checks.

---

### 1.1 PP
`pp` always is pretty printing. Whereever it is used, it is printing something.
Can be seen when using `verbose 1` or `verbose 2` settings in `.mv`-file.

## 2. File-by-File Explanation

---

### 2.1. `main.ml`

**Role:**  
Main entry point implementing a REPL loop, command parsing, global state management, and dispatching function/operator loading and security property checks.

**Key Data Structures:**

- `globals : (func * check_opt) Hashtbl.t` — global mapping of loaded functions/operators and their associated checking options.
- `check_opt` — record controlling glitch assumptions, transition usage, parallelism, and checking order.

**Main Workflow:**

- Reads commands from `stdin` or files.
- Parses commands via Menhir parser wrapped in `Parse` functions.
- Processes commands with `process_command`:
  - Adding operators or functions (`add_operator`, `add_func`).
  - Reading files or ILANG files (`process_file`, `Ilang.process_file`).
  - Running security checks (`check_ni`, `check_sni`, `check_threshold`, `check_spini`).
  - Changing verbosity or exiting.
- Stores and retrieves functions/operators from `globals`.
- Proper error and parse error handling.
- Outputs results to stdout/stderr and continuously loops unless exited.

---

### 2.2. `ilang.ml`

**Role:**  
Parsing and processing ILANG files (a specialized intermediate language format).

**Main Function:**

- `process_file : string -> Prog.Process.func`
  - Parses an ILANG source file through `Parse.read_file`.
  - Processes the AST into an intermediate module representation (`Process.process_prog`).
  - Converts the processed module into a compatible internal function representation (`ToProg.func_of_mod`).
  - Returns the resulting `func`.

**Usage:**  
Called by `main.ml` on `Read_ilang` commands to incorporate ILANG definitions into the system.

---

### 2.3. `checker.ml`

**Role:**  
Implements security property verification, including exhaustive search algorithms and parallelization.

**Key Features:**

- **`check_all`** — recursively explores tuples of load functions (ldfs) and applies bijection splitting and checking.
- **`check_all_para`** — parallelized process-forking version using Unix processes, shared counters (`Shrcnt`), and pipes for synchronization and work distribution.
- Progress tracking and user feedback with timers and counters.
- Formatting helpers for output results (`pp_ok`, `pp_fail`).
- Selects between serial and parallel checks (`check_all_opt`) based on options.

---

### 2.4. `state.ml` (Data Structures Overview)

**Essential Types:**

- `node`: Represents symbolic nodes in an expression graph; tracks children, descriptors, classes, and unique IDs.
- `descriptor`: Categorizes `node` types (random vars, shares, operators, tuples, constants).
- `class_parent`: Union-find style parent links for equivalence classes.
- `Count.t`: Mutable counter type.
- `Pinfo.t`: Tracks usage info for shares (used shares and count).
- `state`: Global state for symbolic computation including stacks of randoms, private vars, todo nodes, bijections, params, and hash tables.

---

### 2.5. `parsetree.ml` (Data Structures Overview)

**Core AST Types:**

- `expr`: Variants `Evar`, `Econst`, and `Eop` represent variables, constants, and operator applications.
- `vcall` and `vcall1`: Variable calls capturing variables or tuples with optional range shifts.
- Instructions including assignments, leaks, and macros.
- Functions (`func`) encapsulate input/output parameters, kind, commands, and variables.
- `cmd`: List of instructions.
- `ids`: Identifier lists or ranges used to compactly represent sets of variables.

---

### 2.6. `prog.ml` (Data Structures and Functions)

#### Data Structures:

- `expr`: Internal typed expressions (`Evar`, `Econst`, unary `Eop1`, binary `Eop2`, n-ary `Eop`, boxed expressions).
- `assgn`: Assignment instruction with variable, instruction kind (subst, hide etc.), and expression.
- `vcall`, `vcalls`: Lists of variables representing tuples or multiple variable calls.
- `macro_call`: Represents macro function invocations with multiple outputs and inputs.
- `leak`: Represents explicit information leakage instructions.
- `instr_d`, `instr`: Discriminated union of instruction types plus pretty-print info.
- `cmd`: Sequence of instructions (command).
- `func`: Central function representation, holding inputs, outputs, variables, commands, and kind.
- `print_info`: Controls verbosity during pretty printing.

#### Key Functions and Their Roles:

- `get_global` — Lookup global function with error reporting.
- Environment management: `add_var`, `get_vars`, `get_var`, `check_single` manage variable declarations and expansions.
- Variable call translation: `get_vcall1`, `get_vcall` handle conversions from AST variable calls with range expansions and rotations.
- `to_expr` and related recursive functions convert parsed AST expressions into internal expressions with type checking and initialization checks.
- Function call argument and output binding: `get_vcalls` and `set_vcalls`.
- Safety checks: `check_init` ensures variables are initialized before usage; `check_para` checks validity of parallel assignments.
- Macro expansion functions recursively inline macros into instruction lists (`macro_expand_c`, `macro_expand_func`).
- `func` — Main entry converting parsed functions to internal representations, with macro expansion and consistency checks.

---

## 3. Summary of Data Interactions

- **Entry:** `main.ml` handles commands and file input.
- **Parsing:** `Parse` processes input to AST; `ilang.ml` processes ILANG to intermediate module and converts to internal func.
- **Conversion:** `prog.ml` converts AST to typed expressions and instructions, performs macro expansion.
- **Symbolic State & Checking:** `state.ml` manages symbolic nodes; `checker.ml` runs property checks (sequential or parallel).
- **Outputs:** Results and errors are formatted and printed in `main.ml` with tracing and verbose modes.
- **Global Data:** Collected in `globals` hashtable, storing functions/operators accessible through the lifecycle.

---

## 4. Additional Notes

- The project supports advanced security analyses on cryptographic masked constructions, focusing on verifying leakages.
- It carefully manages variable initialization and type safety at every translation step.
- Macro expansion eliminates syntactic sugar, easing analysis.
- Parallel checking leverages system-level forking and IPC for scalability.
- Pretty printing and detailed location tracking support diagnostic outputs and debugging.
- The system is extensible: new commands, operators, and functions can be added dynamically.

---

# End of Documentation

---

**If you require additional detailed call-stack walkthroughs, example analyses on sample inputs, or explanations of auxiliary modules (`L`, `Util`, `Shrcnt`, etc.), please ask!**