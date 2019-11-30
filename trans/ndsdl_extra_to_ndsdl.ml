open Core

exception StaticError of string

let rec translate_term (term : Ndsdl_extra.Term.t) : Ndsdl.Term.t =
  match term with
  | Var x -> Var x
  | Number n -> Number n
  | Unop (op, e) -> Unop (op, translate_term e)
  | Binop (op, e1, e2) -> Binop (op, translate_term e1, translate_term e2)

let rec translate_formula (formula : Ndsdl_extra.Formula.t) : Ndsdl.Formula.t =
  match formula with
  | True -> True
  | False -> False
  | Logicalunop (op, p) -> Logicalunop (op, translate_formula p)
  | Logicalbinop (op, p, q) ->
      Logicalbinop (op, translate_formula p, translate_formula q)
  | Compare (op, e1, e2) -> Compare (op, translate_term e1, translate_term e2)
  | Forall (x, p) -> Forall (x, translate_formula p)
  | Exists (x, p) -> Exists (x, translate_formula p)
  | Box (a, p) -> Box (translate_program a, translate_formula p)
  | Diamond (a, p) -> Diamond (translate_program a, translate_formula p)

and translate_program (program : Ndsdl_extra.Program.t) =
  match program with
  | Assign (x, e) -> Assign (x, translate_term e)
  | Assignany x -> Assignany x
  | Assignpmf (x, choices) ->
      let choices =
        List.map choices ~f:(fun (p, e) -> (translate_term p, translate_term e))
      in
      Assignpmf (x, choices)
  | Assignbernoulli (x, e) ->
      let e = translate_term e in
      Assignpmf
        (x, [ (e, Number "1"); (Binop (`Minus, Number "1", e), Number "0") ])
  | Assigngeometric (x, e) ->
      let e = translate_term e in
      Compose
        ( Assign (x, Number "1"),
          Probloop (e, Assign (x, Binop (`Plus, Var x, Number "1"))) )
  | Test p -> Test (translate_formula p)
  | Compose (a, b) -> Compose (translate_program a, translate_program b)
  | Loop a -> Loop (translate_program a)
  | Probloop (e, a) -> Probloop (translate_term e, translate_program a)
  | Choice (a, b) -> Choice (translate_program a, translate_program b)
  | Probchoice choices ->
      let choices =
        List.map choices ~f:(fun (e, a) ->
            (translate_term e, translate_program a))
      in
      Probchoice choices
  | Ode (xs, po) ->
      let xs = List.map xs ~f:(fun (x, e) -> (x, translate_term e)) in
      let po = Option.map po ~f:(fun p -> translate_formula p) in
      Ode (xs, po)
