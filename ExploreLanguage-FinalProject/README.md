# CS 208 – Assignment 5: Tiny Functional Language

**Author:** Eeman Naveed  
**Language:** ChaiLang – a tiny Scheme-like prefix language

---

## Language Features

My mini language is a prefix, parenthesized language with:

- **Numbers & Booleans**
  - Integers: `0`, `1`, `10`, `123`
  - Booleans: `#t`, `#f`
- **Arithmetic**
  - `(+ 2 3)`, `(* 2 3)`, `(- 10 4)`
- **Comparisons & Logic**
  - `(< 2 3)`, `(= 2 2)`, `(<= 2 3)`
  - `(and #t #f)`, `(or #t #f)`, `(not #t)`
- **Conditionals**
  - `if` expressions: `(if cond then-expr else-expr)`
  - `cond` expressions:
    ```scheme
    (cond
      ((< 2 1) 0)
      ((= 2 2) 5)
      (else 9))
    ```
- **Local Bindings (let)**
  - `(let ((x e1) (y e2) ...) body)`
- **Anonymous Functions (lambda)**
  - `(lambda (x y ...) body)`
- **Function Application**
  - `(f arg1 arg2 ...)`
  - `((lambda (x) body) 5)`
- **Closures**
  - Functions capture the environment where they were defined.
- **Strings & Print (bonus)**
  - Strings: `"hello"`
  - Print: `(print "hello chai!")`
- **Lists (bonus)**
  - `(list 1 2 3)`
  - `(head xs)`, `(tail xs)`, `(empty? xs)`, `(cons x xs)`

---

## What This Language Supports

### From Assignment 3
- Integer and boolean expressions  
- Arithmetic operators  
- Basic `if` expressions  

### From Assignment 4
- Anonymous functions (`lambda`)  
- Function calls  
- Closures (functions remember environments)  
- `cond` expressions  

### New / Emphasized in Assignment 5
- `let` bindings with multiple variables  
- Lists and list operations  
- `print` for simple I/O  
- Clear runtime error messages  
- Integrated tests and examples

---

## Files

- `YourLang.hs` – comments only; contains the formal grammar (BNF), sample programs, AST sketch, and notes about semantics and errors.  
- `Parsing.hs` – parser combinator library from the book (unchanged).  
- `Lang.hs` – implementation of:
  - data types (`Expr`, `Value`, `Operator`, environment)
  - parser for ChaiLang expressions
  - interpreter `interp :: Env -> Expr -> Value`
  - helper `run :: String -> Value` to run programs easily in GHCi.

---

## How to Run

From the directory with the files:

```bash
ghci

