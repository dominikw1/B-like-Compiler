# Compiler for a B-like subset of C

## Overview

This Compiler translates a subset of C into x86-64 Assembly. It utilizes LLVM-IR for convenience, but uses its own back-end.

### Algorithms used for stages

- Parsing: Parsing is done using a Pratt-Parser.
- IR Generation: LLVM-IR is generated from the AST using the algorithm presented by Braun et al.
- Instruction Selection: A DAG-covering approach is used for ISel.
- Register Allocation: Registers are allocated on a greedy basis with lots of stack-spillage. No established algorithm is used for this stage yet.

## Build

```
cmake -S . -B build
cmake --build build
```

## Run

```
cd build/src
./Compiler [-c/-a/-l/-i/-r] <input_file>
```


