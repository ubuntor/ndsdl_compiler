open Core

exception StaticError of string

let rec to_const_probability (term : Ndsdl.Term.t) =
  match term with
  | Number n -> Bignum.of_string n
  | Plus (e1, e2) -> Bignum.(to_const_probability e1 + to_const_probability e2)
  | Minus (e1, e2) -> Bignum.(to_const_probability e1 - to_const_probability e2)
  | Times (e1, e2) -> Bignum.(to_const_probability e1 * to_const_probability e2)
  | Div (e1, e2) -> Bignum.(to_const_probability e1 / to_const_probability e2)
  | Neg e -> Bignum.(zero - to_const_probability e)
  | Var _ -> raise (StaticError "probability is not constant")
  | Exp _ -> raise (StaticError "exp not supported as probability")

let rec translate_choices choices : Dl.Program.t =
  match choices with
  | [] -> raise (StaticError "no choices")
  | [ a ] -> a
  | x :: xs -> Choice (x, translate_choices xs)

let rec translate_term (term : Ndsdl.Term.t) : Dl.Term.t =
  match term with
  | Var x -> Var x
  | Number n -> Number n
  | Plus (e1, e2) -> Plus (translate_term e1, translate_term e2)
  | Minus (e1, e2) -> Minus (translate_term e1, translate_term e2)
  | Times (e1, e2) -> Times (translate_term e1, translate_term e2)
  | Div (e1, e2) -> Div (translate_term e1, translate_term e2)
  | Exp (e1, e2) -> Exp (translate_term e1, translate_term e2)
  | Neg e -> Neg (translate_term e)

let rec translate_formula (formula : Ndsdl.Formula.t) ~prob_var : Dl.Formula.t =
  match formula with
  | True -> True
  | False -> False
  | Or (p, q) ->
      Or (translate_formula p ~prob_var, translate_formula q ~prob_var)
  | Not p -> Not (translate_formula p ~prob_var)
  | And (p, q) ->
      And (translate_formula p ~prob_var, translate_formula q ~prob_var)
  | Implies (p, q) ->
      Implies (translate_formula p ~prob_var, translate_formula q ~prob_var)
  | Equiv (p, q) ->
      Equiv (translate_formula p ~prob_var, translate_formula q ~prob_var)
  | Eq (e1, e2) -> Eq (translate_term e1, translate_term e2)
  | Lt (e1, e2) -> Lt (translate_term e1, translate_term e2)
  | Le (e1, e2) -> Le (translate_term e1, translate_term e2)
  | Gt (e1, e2) -> Gt (translate_term e1, translate_term e2)
  | Ge (e1, e2) -> Ge (translate_term e1, translate_term e2)
  | Neq (e1, e2) -> Neq (translate_term e1, translate_term e2)
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
  | Test p -> Test (translate_formula p ~prob_var)
  | Compose (a, b) ->
      Compose (translate_program a ~prob_var, translate_program b ~prob_var)
  | Loop a -> Loop (translate_program a ~prob_var)
  | Choice (a, b) ->
      Choice (translate_program a ~prob_var, translate_program b ~prob_var)
  | Probchoice xs ->
      let choices =
        List.map xs ~f:(fun (e, a) ->
            let probability = to_const_probability e in
            if Bignum.(probability < zero || probability > one) then
              raise (StaticError "Probability is not between 0 and 1");
            (probability, translate_program a ~prob_var))
      in
      let total =
        List.fold choices ~init:Bignum.zero ~f:(fun total (p, _) ->
            Bignum.(total + p))
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1");
      let choices =
        List.map choices ~f:(fun (p, a) ->
            Dl.Program.Compose
              ( Dl.Program.Assign
                  ( prob_var,
                    Dl.Term.Times
                      ( Dl.Term.Number (Bignum.to_string_accurate p),
                        Dl.Term.Var prob_var ) ),
                a ))
      in
      translate_choices choices
  | Ode (xs, po) ->
      Ode
        ( List.map xs ~f:(fun (x, e) -> (x, translate_term e)),
          Option.map po ~f:(fun p -> translate_formula p ~prob_var) )
