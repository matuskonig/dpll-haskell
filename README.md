# Haskell DPLL implementation
## Introduction

This program is implementing the DPLL algorithm, used for finding solutions for SAT problem.
SAT (propositional satisfiability problem) aims to decide, for the formula in CNF given, whether
there exists a model, for which it holds.

## Compilation

Program can be compilled in a standard way through 

```
ghc main.hs -outputdir out -O3
```
Setting output directory is recommended, also it is recommended to use the O3 optimalization flag

## Usage

### Basic usage info
In every case, the program receives a CNF formula and it find a model (if exists), which satisfies the formula.
The variables (literals) are represented as integers, positive interer represents positive variable and vice versa.

For example, input $(x_1 \vee \neg x_2) \wedge (\neg x_1 \vee x_2)$ can be represented as 

```
1 -2
-1 2
```
The model found is 2, 1, which stands for $x_1 = True$, $x_2 = True$

The program has 2 way of use: 
1.  The program can be used as a standalone application. Formula in CNF is passed to `stdin`, every clause is put in the new line, 
    literals are whitespace separated.
2. `dpll` solver function is exported in the module, thus it is possible to use it further in other haskell applications.

## Notes
- Because variables can be arbitrary integer value, variable 0 can also be used, however it is discouraged to do so,
    because 0 and -0 are valid and same values and this could lead to unpredictable results.
- Variables, which can have arbitrary Boolean value, are not in the model. The model returned (if found)
    is a minimal model in the sense, given the formula and algorithm branching and value assignment decisions. 