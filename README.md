# SdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane` from
`opam`.

Then run `make`. The executable will be in `_build/default/sdl_compiler.exe`.

## Structure
  - `main.ml`: Main. Runs all phases of compilation.
  - `parse/`: Parser and lexer.
  - `check/`: Static checking.
  - `trans/`: Translation of SdL into dL. Soundness proofs for translation
    are in the paper.
