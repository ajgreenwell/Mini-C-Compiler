### MiniC Compiler
## In collaboration with: Prof. Robert Muller

MiniC is a simplified dialect of C. It includes

- integer and boolean types and expressions;
- function definitions and calls;
- imperative features: assignment, branching, while-loops and printing.

##### Example 1

```c
int iterativeFact(int m) {
  int answer;
  answer = 1;
  while (m > 0) {
    answer = answer * m;
    m = m - 1;
    }
  return answer;
}

int recursiveFact(int n) {
  if (n == 0)
    return 1;
  else
    return n * recursiveFact(n - 1);
}

int main() {
  print iterativeFact(6);
  print recursiveFact(6);
  return 0;
}
```

---

##### Example 2

```c
int mod(int a, int b) {
  if (b < 0) return 0;
  else while (a >= b) a = a - b;
  return a;
}

int abss(int a) {
  if (a < 0) return 0 - a;
  else return a;
}

int gcd(int a, int b) {
  a = abss(a);
  b = abss(b);
  if (a == 0 or b == 0) return a + b;
  else if (b > a) return gcd(a, mod(b, a));
  else return gcd(mod(a, b), b);
}

int main() {
  int n;
  n = gcd(14039, 1529);
  print n;
  return 0;
}
```

---

## The miniC Project

The overall structure of the miniC compiler is

```
miniC program => Lexer ->
                 Parser ->
                 TypeChecker ->
                 Name ->
                 Lift ->
                 CopyProp ->
                 Control ->
                 Codegen => MIPS program
```

The miniC project is a four-part project implementing four of the last five of these components of the compiler. The four parts are:

```ocaml
Part 1: Write Name.translate : Ast.program -> Ast.program
Part 2: Write Lift.translate : Ast.program -> Ast.program
Part 3: Write Control.translate : Ast.program -> Quads.instructionstream
Part 4: Write Codegen.translate : Quads.instructionstream -> Mips.Codestream.t
```

   ### Logistics

The `src` directory contains the following files.

```
asm/            dbg/            lib/            test/
bin/            dune-project    solved*         try/
```

+ The `asm/` directory contains assembly language output of the solved compiler for all of the source files in the `test/` directory;

+ The `dbg/` directory contains `.dbg` files produced by the solved compiler for all of the source files in the `test/` directory;

+ The `lib/` directory contains OCaml library modules in support of the miniC compiler;

+ The `test/` directory contains several test source files. It also contains 1. a copy of the Mars MIPS simulator and 2. a shell script `test/test.sh` for testing the compiler output against known outputs in `test/ans/`;

+ The `bin/` directory contains 39 OCaml source files along with a `dune` build file. This harness code is configured to be easy to work with in a Unix environment that includes an implementation of OCaml, dune and the atom editor.

+ The `dune-project` file supports the dune build system;

+ The `solved` file is a MacOS binary for a solved version of the compiler. To run it, type

  ```bash
  > cd src
  > ./solved file.mc
  ```

  If the code in `file.mc` is well-formed, the compiler will generate two output files: `file.asm` containing the compiled object code and `file.dbg` which contains text displaying the compiler data structures after each compilation phase. It would be a good idea to create a few test miniC files and run them through the compiler to see how the compiler works. It produces inefficient code, but it is relatively simple and most importantly it works.

+ The `try/` directory contains sample outputs for running individual phases of the compiler. See below.

---

#### Building the miniC Compiler

   ```
   > cd src
   > dune build bin/main.exe
   ```

   #### Running the miniC compiler

   ```
   > cd src
   > dune exec bin/main.exe foo.mc
   ```

This creates `foo.asm` and `foo.dbg`. The `dbg` file shows the compiler intermediate representations at each of the major phases of compilation.

### Part 1: Naming Phase

The naming phase of the compiler is implemented by the source-to-source transformation

  ```
  Name.translate : Ast.program -> Ast.program
  ```

The harness code for the `Name` module is found in the file `name.ml`. This is the first transformation executed after the type checker. This translation walks the program tree, preserving all of the program phrases as found but making recursive transformations on `Ast.term`s. In particular, the results of all subterms should be assigned to newly created temporary variables (so-called "fresh" variables) by introducing let terms with fresh variables. For example, the naming transformation, should recursively translate the application term `+(12, a)` to:

```
let x1 = 12 in let x2 = a in let x3 = +(x1, x2) in x3
```

 where `x1`, `x2` and `x3` are fresh variables.

The recursive translation here tends to give rise to lots of nested-lets. For example `+(+(2, 3), 4)` would translate to

```
let x1 = let x2 = 2 in
         let x3 = 3 in
         let x4 = +(x2, x3)
         in
         x4 in
let x5 = 4 in
let x6 = +(x1, x5)
in
x6
```

---

### Part 2: Lifting Phase

The lifting phase of the compiler is implemented by the source-to-source translation

```
Lift.translate : Ast.program -> Ast.program 
```

The harness code for the `Lift` module is found in file `lift.ml`. This is the second phase executed after the type checker. This translation walks the program tree, preserving all of the program phrases as found but making recursive transformations on `Ast.term`s that lift nested lets according to the following rule:

```
let x1 = (let x2 = e2 in e3) in e4 =lift=> let x2 = e2 in (let x1 = e3 in e4)
```

The result of this transformation should have no terms of the form on the left of the =lift=> above.

---


### Part 3: Control Phase

The transformations after naming and lifting translate away from the compiler intermediate language of `Ast` toward more machine-centric languages.

```
Control.translate : Ast.program -> Quads.instructionstream 
```

This translation makes the control-flow that is implicit in `Ast.statement` explicit in the language of quadruples. The target language for this transformation is defined in the harness code file `quads.ml`. This transformation should recursively walk the `Ast.program` tree, translating each `Ast.procedure` to an equivalent `Quads.procedure`. The body of the former is an `Ast.statement` while the body of the latter is  a `Quads.instruction list`, i.e., it is a list of simpler, more machine-like instructions that may (or may not) include labels. These labels can be the targets of unconditional (`Quads.Jmp`) or conditional (`Quads.JmpZero`) branch instructions.

- An `Ast.Assign` statement should translate to a `Quads.Get` instruction;

- An `Ast.While` statement of the form `Ast.While {expr; statement}`, if **[i1; ...; ik]** is the translation of `expr` and **[ik+2; ..., ik+m]** is the translation of `statement` then

  ```
  [l1:i1; ...; ik; jzero x, l2; ik+2; ..., ik+m; jmp l1; l2:nop]
  ```

  (where **x** holds the result of the test term) is the translation;

- An `Ast.IfS` statement of the form `Ast.IfS {expr; thn; els}`, if **[i1; ...; ik]** is the translation of `expr`, **[ik+2; ...; ik+m]** is the translation of `thn` and **[ik+m+2; ...; in]** is the translation of `els` then

  ```
  [i1; ...; ik; jzero x, l1; ik+2; ..., ik+m; jmp l2; l1:ik+m+2; ..., ik+m; l2:nop]
  ```

   (where **x** holds the result of the test term) is the translation.

---

### Part 4: Code Generation Phase

The `Codegen` translation deals with storage allocation for procedure and function variables as well as function call and return protocols. For the purposes of this gut-simple compiler, we will allocate storage for all variables, both programmer supplied and the temporaries generated by the compiler, in an activation record allocated on the call stack. The structure of an activation record is standard

   ```
          +----------------------------+
   A      |      Caller-Save Regs      |                 higher addresses
   A      +----------------------------+
   A      |       Value of Arg 1       |                 
   A      +--           :            --+
   A      |             :              |
   A      +--                        --+
   A      |       Value of Arg n       |
   A      +----------------------------+
   A      |  Caller's Return Address   |
          +----------------------------+
   B      |   Caller's Frame Pointer   | <-- fp
   B      +----------------------------+
   B      |      Local Variable 1      |
   B      +--            :           --+
   B      |              :             |
   B      +--                        --+
   B      |      Local Variable k      |
   B      +----------------------------+
   B      |   Callee Save Registers    |                 lower addresses
          +----------------------------+
                                         <-- sp
   ```

where the parts labeled with `A` (above left) are managed by the caller and the parts labeled with `B` are managed by the callee (i.e., the procedure being called). Since the miniC compiler has no register allocator, there is no need to save or restore the contents of (most) machine registers around procedure calls and returns.  Note that all procedure variables can be referenced indirectly using the frame pointer register `fp`. In particular, the storage cells allocated for a procedure's parameters are allocated at positive offsets from the `fp` and storage cells for local variables are allocated at negative offsets from the `fp`. Activation records are constructed by a combination of actions that are performed

   1. around procedure calls,

   2. on procedure entry and

   3. before the called procedure returns.

   - **Procedure call**: code is required to push the arguments onto the call stack, to push the return address (`ra`) register and then transfer to the called procedure using a jump and link (`jal`) instruction.

     The code following the `jal` must restore the `ra` register and deallocate the storage for the actual arguments (thereby resetting the stack pointer (`sp`) register.

   - **Procedure entry**: code is required to store the callers frame pointer and then allocate storage at the bottom of the activation record for all local variables (including temporaries). Note that the activation record on the stack will require one 4-byte word of memory for each variable introduced by the programmer and likewise for each temporary name introduced by earlier phases of the compiler. You're going to need to find all of the temporaries in the `Quads.procedure` allocating a frame pointer offset for each. These variable offsets are best stored in an environment.

   - **Procedure return**: if the procedure is a function then the value to be returned to the caller must be stored in the value register `$v0` register and all storage for local variables must be removed from the stack, leaving the `sp` register pointing at the caller's saved frame pointer on the stack. The return is completed with a `jr` instruction.
