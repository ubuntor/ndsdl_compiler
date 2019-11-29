# NDSdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane`,
`menhir`, `bignum` from `opam`.

Then run `make`. The executable will be in `_build/default/ndsdl_compiler.exe`.

You can also run `dune exec -- ./ndsdl_compiler.exe` to run the compiler.

## Structure
  - `ndsdl_compiler.ml`: Toplevel commandline. Runs all phases of compilation.
  - `parse/`: Parser and lexer.
  - `ir/`: Intermediate representations (NDSdL, dL).
    are valid and sum to 1.
  - `trans/`: Translation of NDSdL into dL. Soundness proofs for translation
    are in the paper. Also does static checking while translating:
    - All probabilities should be valid and sum to 1 in a linear combination.
    - The probability variable must not be written to.
  - `output/`: Pretty printing dL to a file.

## New Syntax
```
Program a ::= ...
  | {p_1: a_1 +++ ... +++ p_n: a_n}     Linear combination: p_i must be constant
```
