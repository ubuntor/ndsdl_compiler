# NDSdL Compiler

## Build Instructions

Make sure `opam` is installed, then install `dune`, `core`, `ppx_jane`,
`menhir`, `bignum` from `opam`.

Then run `make`. The executable will be in `_build/default/ndsdl_compiler.exe`.

You can also run `dune exec -- ./ndsdl_compiler.exe` to run the compiler.

## Structure
  - `ndsdl_compiler.ml`: Toplevel commandline. Runs all phases of compilation.
  - `parse/`: Parser and lexer.
  - `ir/`: Intermediate representations:
    - `Ndsdl_extra`: NDSdL with syntactic sugar for Bernoulli and Geometric,
      and `@unroll` annotations for probabilistic loops.
    - `Ndsdl`: NDSdL.
    - `Dl`: dL.
  - `trans/`: Translation between intermediate representations.
    Soundness proof for translation is in the paper.

    Also does static checking while translating:
    - All probabilities should be valid and sum to 1 in a probabilistic choice.
    - The number of times to unroll probabilistic loops should be a nonnegative
      integer.
  - `output/`: Pretty printing dL to a file.

## Syntax
```
Program a ::=
  | {p_1: a_1 +++ ... +++ p_n: a_n}     Probabilistic choice:
                                          p_i constant probabilities
                                          The choices are parsed as a list,
                                          not as binary choices.
  | {a}*:p                              Probabilistic loop:
                                          p constant probability
  | {a}*:p@unroll(n)                    Unroll probabilistic loop:
                                          p constant probability,
                                          n nonnegative integer.
                                          Unrolls the probabilistic loop n times
                                          for better probability bounds.
  | x := {p_1: e_1, ..., p_n: e_n}      Random assignment from pmf:
                                          p_i constant probabilities
  | x := Bernoulli(p)                   Bernoulli distribution:
                                          p constant probability
  | x := Geometric(p)                   (1-indexed) Geometric distribution:
                                          p constant probability
  | x := Geometric(p)@unroll(n)         Unroll (1-indexed) geometric distribution:
                                          p constant probability
                                          n nonnegative integer
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
  | [a]P | [a;]P                        We allow an extra semicolon at the end
  | <a>P | <a;>P                          of a program.
  | <|a|>P <= p | <|a;|>P <= p          Probability upper bound:
                                          p constant probability
  | (P)
```
