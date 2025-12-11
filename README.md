# CS 208 – Programming Languages (Knox College)
 
This repository contains my work for CS 208: Programming Languages at Knox College.  
The course explored **functional programming**, **parsing**, **interpreters**, **recursion**, and the design of **mini programming languages** using Haskell.

The assignments in this repo demonstrate core concepts in programming language design, evaluation strategies, environment models, and building small interpreters from scratch.

---

## 📁 Repository Structure

### 📂 Assignment1-Pictures/
A collection of small functional programs for generating and transforming pixel-based images.  
Implemented in Haskell using recursive functions.

**Includes:**
- `dotAndLine`
- `blendColor`, `combine`
- `rectangle`
- `union`, `cut`
- `paintSolid`, `paint`
- Transformations: `Fill`, `Zoom`, `Flip`
- `blur`, `blurMany`

**Skills demonstrated:** recursion, higher-order functions, custom types, pixel manipulation, image composition.

---

### 📂 Assignment2-BankAPI/
A small HTTP bank server implemented in Haskell using:
- **SQLite** for persistent storage  
- **WAI** (Web Application Interface) for HTTP routing  
- **Custom command parser**  
- Endpoints: **deposit**, **withdraw**, **balance**

**Concepts implemented:**
- Transaction-log database  
- Summing account balances  
- Parsing URL paths into commands  
- Returning HTTP responses  
- Basic error handling (`ERROR` for invalid commands)  
- Running a server on port 3421

**Skills demonstrated:** IO operations, HTTP servers, database integration, functional parsing, monads.

---

### 📂 ExploreLanguage-FinalProject/
My final project: **ChaiLang**, a tiny Scheme-like prefix language that I designed and implemented.

**ChaiLang supports:**
- Arithmetic & boolean expressions  
- `if` and `cond`  
- Anonymous functions (`lambda`)  
- Function application  
- Closures (environment capture)  
- `let` bindings  
- Lists: `list`, `head`, `tail`, `cons`, `empty?`  
- Strings & print  
- Error messages and test cases  

**Files include:**
- Formal grammar (BNF)
- Interpreter (Haskell)
- Sample programs
- Project report (PDF)

**Skills demonstrated:**  
parser design, interpreter construction, language semantics, closures, lexical scope, AST representation.

---

## 🧠 Key Skills & Concepts Learned

### ✔ Functional Programming in Haskell
- Higher-order functions  
- Recursion  
- Immutable data structures  

### ✔ Parser & Interpreter Design
- Writing grammars  
- Building ASTs  
- Implementing `interp :: Env -> Expr -> Value`  

### ✔ Environments & Closures
- Lexical scoping  
- Capturing variables  
- Evaluating lambda expressions  

### ✔ Databases & HTTP Servers
- SQLite queries  
- WAI HTTP server  
- URL-based command routing  

### ✔ Software Engineering Practices
- Modular file structure  
- Clear documentation  
- Testing in GHCi  
- Clean functional design  

---

## ▶️ How to Run the Programs (Mac Instructions)

### 🟦 Assignment 1 and Final Project (running Haskell code in GHCi)

1. Open the **Terminal** app on your Mac.
2. Navigate to the folder that contains your Haskell files. Example:
cd ~/Desktop/CS208/Assignment1-Pictures
3. Start GHCi:
ghci
4. Load your file:
:l Assignment1.hs
For the final project:
:l Lang.hs
5. Run test expressions:
run "(+ 2 3)"
run "(let ((x 2) (y 3)) (+ x y))"

---

### 🟩 Assignment 2 — Bank API (Haskell server with SQLite)

1. Open Terminal.
2. Navigate to the Assignment 2 folder:
cd ~/Desktop/CS208/Assignment2-BankAPI
3. Run the server:
runhaskell Assignment2.hs
4. When the server starts, it will be running on **port 3421**.
5. Open any browser and type commands directly into the address bar

   Example:

Deposit:
localhost:3421/deposit/eeman/10

Check balance:
localhost:3421/balance/eeman

Withdraw (if implemented):
localhost:3421/withdraw/eeman/5

You will see simple text responses such as:
OK
15

6. To stop the server:
Control + C
---

### 🟧 Notes for Mac Users
- These commands work on any Mac with GHC installed.  
- Use `cd` to move into the correct folder.  
- `ghci` loads Haskell interactively.  
- `runhaskell` executes a `.hs` file directly.  
- If the terminal says something is missing, install GHC or Stack.

---
