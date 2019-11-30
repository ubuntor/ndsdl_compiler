open Core

exception StaticError of string

let to_const_probability term =
  let rec to_const (term : Ndsdl.Term.t) =
    match term with
    | Number n -> Bignum.of_string n
    | Unop (`Neg, e) -> Bignum.(zero - to_const e)
    | Binop (`Plus, e1, e2) -> Bignum.(to_const e1 + to_const e2)
    | Binop (`Minus, e1, e2) -> Bignum.(to_const e1 - to_const e2)
    | Binop (`Times, e1, e2) -> Bignum.(to_const e1 * to_const e2)
    | Binop (`Div, e1, e2) -> Bignum.(to_const e1 / to_const e2)
    | Var _ -> raise (StaticError "probability is not constant")
    | Binop (`Exp, _, _) ->
        raise (StaticError "exp not supported as probability")
  in
  let probability = to_const term in
  if Bignum.(probability < zero || probability > one) then
    raise (StaticError "Probability is not between 0 and 1");
  probability

let rec translate_choices choices : Dl.Program.t =
  match choices with
  | [] -> raise (StaticError "no choices")
  | [ a ] -> a
  | x :: xs -> Choice (x, translate_choices xs)

let multiply_probability p ~prob_var =
  Dl.Program.Assign
    ( prob_var,
      Dl.Term.Binop
        ( `Times,
          Dl.Term.Number (Bignum.to_string_accurate p),
          Dl.Term.Var prob_var ) )

let rec translate_term (term : Ndsdl.Term.t) : Dl.Term.t =
  match term with
  | Var x -> Var x
  | Number n -> Number n
  | Unop (op, e) -> Unop (op, translate_term e)
  | Binop (op, e1, e2) -> Binop (op, translate_term e1, translate_term e2)

let rec translate_formula (formula : Ndsdl.Formula.t) ~prob_var : Dl.Formula.t =
  match formula with
  | True -> True
  | False -> False
  | Logicalunop (op, p) -> Logicalunop (op, translate_formula p ~prob_var)
  | Logicalbinop (op, p, q) ->
      Logicalbinop
        (op, translate_formula p ~prob_var, translate_formula q ~prob_var)
  | Compare (op, e1, e2) -> Compare (op, translate_term e1, translate_term e2)
  | Forall (x, p) -> Forall (x, translate_formula p ~prob_var)
  | Exists (x, p) -> Exists (x, translate_formula p ~prob_var)
  | Box (a, p) ->
      Box
        ( Dl.Program.Compose
            ( Dl.Program.Assign (prob_var, Dl.Term.Number "1"),
              translate_program a ~prob_var ),
          translate_formula p ~prob_var )
  | Diamond (a, p) ->
      Diamond
        ( Dl.Program.Compose
            ( Dl.Program.Assign (prob_var, Dl.Term.Number "1"),
              translate_program a ~prob_var ),
          translate_formula p ~prob_var )

and translate_program (program : Ndsdl.Program.t) ~prob_var : Dl.Program.t =
  match program with
  | Assign (x, e) ->
      if String.equal x prob_var then
        raise (StaticError "Cannot assign to probability variable")
      else Assign (x, translate_term e)
  | Assignany x ->
      if String.equal x prob_var then
        raise (StaticError "Cannot assign to probability variable")
      else Assignany x
  | Assignpmf (x, choices) ->
      let choices =
        List.map choices ~f:(fun (p, e) ->
            (to_const_probability p, Dl.Program.Assign (x, translate_term e)))
      in
      let total =
        List.fold choices ~init:Bignum.zero ~f:(fun total (p, _) ->
            Bignum.(total + p))
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1");
      let choices =
        List.map choices ~f:(fun (p, a) ->
            Dl.Program.Compose (multiply_probability p ~prob_var, a))
      in
      translate_choices choices
  | Test p -> Test (translate_formula p ~prob_var)
  | Compose (a, b) ->
      Compose (translate_program a ~prob_var, translate_program b ~prob_var)
  | Loop a -> Loop (translate_program a ~prob_var)
  | Probloop (e, a) ->
      let p = to_const_probability e in
      Loop
        (Dl.Program.Compose
           (multiply_probability p ~prob_var, translate_program a ~prob_var))
  | Choice (a, b) ->
      Choice (translate_program a ~prob_var, translate_program b ~prob_var)
  | Probchoice choices ->
      let choices =
        List.map choices ~f:(fun (e, a) ->
            (to_const_probability e, translate_program a ~prob_var))
      in
      let total =
        List.fold choices ~init:Bignum.zero ~f:(fun total (p, _) ->
            Bignum.(total + p))
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1");
      let choices =
        List.map choices ~f:(fun (p, a) ->
            Dl.Program.Compose (multiply_probability p ~prob_var, a))
      in
      translate_choices choices
  | Ode (xs, po) ->
      Ode
        ( List.map xs ~f:(fun (x, e) -> (x, translate_term e)),
          Option.map po ~f:(fun p -> translate_formula p ~prob_var) )
