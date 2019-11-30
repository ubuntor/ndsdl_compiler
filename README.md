# NDSdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane`,
`menhir`, `bignum` from `opam`.

Then run `make`. The executable will be in `_build/default/ndsdl_compiler.exe`.

You can also run `dune exec -- ./ndsdl_compiler.exe` to run the compiler.

## Structure
  - `ndsdl_compiler.ml`: Toplevel commandline. Runs all phases of compilation.
  - `parse/`: Parser and lexer.
  - `ir/`: Intermediate representations (NDSdL_extra, NDSdL, dL).
    are valid and sum to 1.
  - `trans/`: Translation between intermediate representations.
    Soundness proofs for translations are in the paper.

    Also does static checking while translating:
    - All probabilities should be valid and sum to 1 in a linear combination.
    - The probability variable must not be written to.
  - `output/`: Pretty printing dL to a file.

## Syntax
```
Program a ::=
  | x := {p_1: e_1, ..., p_n: e_n}      Random assignment from pmf:
                                          p_i constant probabilities
  | x := Bernoulli(p)                   Bernoulli distribution:
                                          p constant probability
  | x := Geometric(p)                   (1-indexed) Geometric distribution:
                                          p constant probability
  | {p_1: a_1 +++ ... +++ p_n: a_n}     Linear combination:
                                          p_i constant probabilities
  | {a}*:p                              Probabilistic repetition:
                                          p constant probability
  | a; b                                Note that the semicolon is explicitly
                                          required for sequential composition
                                          instead of after each assignment.
  | ?P                                  These have the same meaning as in
  | a ++ b                                KeYmaera X.
  | x := e
  | {a}*
  | {x' = e, ...}
  | {x' = e, ... & P}
  | {a}

Term e ::=
  | x                                   These have the same meaning as in
                                          KeYmaera X.
  | c                                   Decimal literal.
  | -e
  | e_1+e_2
  | e_1-e_2
  | e_1*e_2
  | e_1/e_2
  | e_1^e_2
  | (e)

Formula P ::=
  | true                                These have the same meaning as in
  | false                                 KeYmaera X.
  | P&Q
  | P|Q
  | P->Q
  | P<->Q
  | !P
  | e_1=e_2
  | e_1<e_2
  | e_1<=e_2
  | e_1>e_2
  | e_1>=e_2
  | e_1!=e_2
  | \forall x e
  | \exists x e
  | [a]P
  | <a>P
  | (P)
```
