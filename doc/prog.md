# Detailed Explanation of Functions in `prog.ml`

The module `ToProg` inside `prog.ml` is the main transformer from parsed AST forms (from `parsetree.ml`) to the internal program representation used later for analysis and verification.



---


# Function Explanation

## 1. `let get_global globals id`

- **Purpose**: Look up a global function in the `globals` hashtable using the identifier `id`.
- **Process**: Uses `Hashtbl.find` with the key data `id` (the string content).
- **Error handling**: If not found, raises an error indicating "undeclared macro".
- **Role**: Ensures that macros or global functions referred to are declared before use.

---

## 2. Types inside `ToProg` module

### `vkind`
Tag for variable kind which can be:
- `VKvar` of `var` (basic single variable).
- `VKtuple` of `var list` (tuple of variables).

### `env` (environment record)
Holds:
- `globals`: global functions env.
- `locals`: map from identifiers to `vkind` (local variables).
- `others`: list of other vars (usually for "other" variables).
- `init`: set of initialized variables (to track initialization status).

> This environment records all locals and global references during translation.

---

## 3. `add_var other env id kind`

- Adds a variable of kind `kind` bound to identifier `id` in the environment `env`.
- Checks if `id` is already in `env.locals` to avoid multiple declarations.
- Stores `kind` in `locals`.
- If `other` is true and `kind` is a single variable, adds it to `others`.

> Used to consistently manage local variable declarations and "other" variables.

---

## 4. `get_vars env id`

- Retrieves the list of variables corresponding to an identifier `id`.
- Looks up `id` in local environment.
- If not found, creates a new variable initialized for `id` with width `E.w1` (bitwidth 1).
- Variables can be tuples (`VKtuple`), in which case all parts are returned.

> Ensures all variables referenced are declared in locals or created.

---

## 5. `check_single loc id xs`

- Checks that the list of variables `xs` contains exactly one variable.
- If there are multiple, this implies the variable is shared or a tuple.
- Throws an error if `id` is a shared variable but used where sharing is not allowed.

> Used mainly to ensure scalar variable usage in expressions or assignments where sharing tuples are invalid.

---

## 6. `get_var env id`

- Calls `get_vars` and then ensures exactly one variable is returned via `check_single`.

> Used for extracting singular variables in expressions and assignments.

---

## 7. `mk_range id (i,j)` and `mk_rangen id rs`

- Creates lists of identifiers representing ranges:
  - `mk_range` creates identifiers with suffixes for indices from `i` to `j`.
  - `mk_rangen` applies `mk_range` to a list of ranges returning a flattened list.

> Helps expand array-like variable references or ranges in `id` for tuple sharing.

---

## 8. `get_vcall1 env vcall`

- Retrieves the variable(s) referenced by a `vcall1` (which can be a single variable, ranged variables, or tuple).
- Resolves identifiers and expansions to a list of vars.

---

## 9. `rotate_xs dir xs i`

- Rotates list `xs` by `i` positions either left or right.

> Used when variable calls (`vcall`) include shift annotations (e.g., slicing or rotating vectors).

---

## 10. `get_vcall env (vc1, shf)`

- Expands variable call(s) by:
  - Getting variables for `vc1` via `get_vcall1`.
  - Applying optional rotation/shifting `shf`.

> Returns the resulting list of variables.  
> Enables transforming parsed variable accesses with optional shifts into internal lists.

---

## 11. `check_init env loc xs`

- Checks whether all variables in `xs` are initialized (`env.init`).
- Raises errors for any uninitialized variable.

---

## 12. `get_op op`

- Looks up operator `op` by name using `E.Op.find`.
- Raises error if operator is unknown.

> Used to validate and resolve operator tokens during expression construction.

---

## 13. `check_ty_e loc e ty`

- Checks at location `loc` that expression `e` has type `ty`.
- Uses `type_of_expr` helper to infer type.
- Reports type mismatch errors.

---

## 14. Expression Constructors — Mutually Recursive Functions

These recursively translate parsed expressions to internal expressions:

### `to_expr env e`
- Main expression converter from parsetree expr `e` to internal expr.
- Handles:
  - `Evar v`: converts variables via `get_vcall`, ensures singleton, checks init.
  - `Econst`: wraps constants.
  - `Eop`: resolves operator and maps sub-expressions.
- Calls `to_expr_ty` for typed expressions.

### `to_expr_ty env e ty`
- Calls `to_expr`, then checks its type matches `ty`.

### `to_expr_n env n e`
- Converts expression `e` into a list of `n` expressions.
- Handles vectorized operations or vector constants.
- Supports constant broadcasting.

> Used when expecting multiple shares/outputs.

### `to_expr_n_ty env n e ty`
- Like `to_expr_n` but with type checking on each element.

---

## 15. `set_init env x`

- Adds variable `x` to `env.init` to mark it as initialized.

> Used after assigning outputs or properly initialized variables in transformations.

---

## 16. `get_vcalls env id vcalls pins ins`

- Checks and translates inputs for function calls.
- Takes:
  - `pins`: public input variables with types
  - `ins`: masked inputs with types
  - `vcalls`: variable calls from source program code
- Validates argument count matches expected counts.
- Checks types for all variables.
- Checks initialization for shares.

> Returns a flat list of internal variables grouped combining `pins` and `ins`.  
> Used to bind formal parameters to actual arguments.

---

## 17. `set_vcalls env id vcalls outs`

- Processes and validates output bindings for a function call.
- Checks lengths match between `outs` and `vcalls`.
- Marks all output variables as initialized.
- Checks type consistency between output variables and computed values.

> Returns list of variables bound on function outputs.  
> Ensures consistency and initialization after function calls.

---

## 18. `vars` (function)

- Recursively collects all variables (`Evar`) appearing in an expression.
- Returns a set of variables.

> Useful for dependency analysis and checking parallel assignments.

---

## 19. `check_para loc is`

- Checks that parallel assignments described in list `is` are valid.
- Ensures variables on right-hand side are disjoint with left-hand side assigned variables to prevent illegal overlaps.
- Raises error on invalid shadowing.

> Enforces safe parallel assignment, crucial for soundness.

---

## 20. `pp_loc_info loc msg`

- Formats location and optional message for pretty-printing.

> Used in instructions to add source code position info in generated code.

---

## 21. `mk_instr loc ?msg i`

- Constructs an instruction record with data `i`.
- Annotates with location and optional message printer.

> Standardizes instruction construction preserving source location.

---

## 22. `to_assgn env i`

- Transforms a parsed assignment instruction (`i`) into internal representation.
- Converts left-hand side variables (possibly tuples or ranges) into internal variables.
- Converts right-hand side expressions to internal expressions respecting types and initialization.
- Builds internal `assgn` structures with type-checked variables and expressions.
- Checks/shifts/enforces initialization.

> Ensures that assignment statements are fully typed and properly tracked.

---

## 23. Macro Expansion Workflow

Functions related to expanding macros and replacing them with sequences of instructions (`macro_expand_c`, `macro_expand_call`, etc.).

- Expansion performs inlining of macro commands recursively.
- Tracks generated `init` assignments to properly initialize variables.
- Converts macro calls into concrete instructions, removes dependence on macros post-expansion.

> Used to flatten macros for simpler downstream analyses.

---

## 24. `macro_expand_func globals func`

- Expands macros in a function command sequence.
- Updates function environment to include new variables/rand variables.
- Returns expanded function without macro instructions, suitable for final analysis.

---

## 25. `func globals f`

- The main entry point to convert raw function representation `f` into internal representation.
- Calls `ToProg.to_func` to transform AST function into internal `func`.
- Expands macros in the function.
- Adds the processed function to global environment.

> Returns the fully processed function.

---

## Summary Table of Key Functions

| Function                          | Inputs                           | Outputs            | Purpose & Notes                                                  |
|----------------------------------|----------------------------------|--------------------|------------------------------------------------------------------|
| `get_global globals id`          | Global map, identifier           | Global function     | Lookup global function by name                                  |
| `add_var other env id kind`      | bool, env, id, vkind             | unit                | Add variable with kind to environment locals                    |
| `get_vars env id`                | env, id                          | var list            | Get or create list of vars for identifier                       |
| `get_var env id`                 | env, id                          | var                 | Get single var from identifier (error if tuple)                 |
| `to_expr env e`                  | env, AST expr                    | internal expr       | Convert AST expression to typed internal expr                   |
| `to_expr_n env n e`              | env, number, AST expr            | expr list           | Convert expression to a list of n expressions                   |
| `set_init env x`                 | env, var                         | unit                | Mark variable as initialized in env                             |
| `get_vcalls env id vcalls pins ins` | env, id, vcalls, pins, ins      | var list            | Bind inputs to function call with type & init checking          |
| `set_vcalls env id vcalls outs` | env, id, vcalls, outs            | var list            | Bind output variables, mark initialized, check types            |
| `vars`                           | expr                             | var set             | Compute vars used in an expression                              |
| `check_para loc is`              | location, instr list             | unit                | Validate parallel assignment safety                             |
| `macro_expand_c env c`           | env, command list                | expanded commands   | Recursively expand macros to concrete instrs                    |
| `macro_expand_func globals func` | globals, func                    | func                | Expand all macros in a function                                 |
| `func globals f`                 | globals, raw func AST            | processed func      | Main function translation and macro expansion                   |
