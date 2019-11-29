open Core

exception StaticError of string

let rec to_const_probability (e : Ndsdl.Term.t) =
  match e with
  | Number n -> Bignum.of_string n
  | Plus (e1, e2) -> Bignum.(to_const_probability e1 + to_const_probability e2)
  | Minus (e1, e2) -> Bignum.(to_const_probability e1 - to_const_probability e2)
  | Times (e1, e2) -> Bignum.(to_const_probability e1 * to_const_probability e2)
  | Div (e1, e2) -> Bignum.(to_const_probability e1 / to_const_probability e2)
  | Neg e -> Bignum.(zero - to_const_probability e)
  | Var _ -> raise (StaticError "probability is not constant")
  | Exp _ -> raise (StaticError "exp not supported as probability")

let rec check_formula (p : Ndsdl.Formula.t) =
  match p with
  | Not p | Forall (_, p) | Exists (_, p) -> check_formula p
  | Or (p, q) | And (p, q) | Implies (p, q) | Equiv (p, q) ->
      check_formula p;
      check_formula q
  | Box (a, p) | Diamond (a, p) ->
      check_program a;
      check_formula p
  | True | False | Eq _ | Lt _ | Le _ | Gt _ | Ge _ | Neq _ -> ()

and check_program (a : Ndsdl.Program.t) =
  match a with
  | Test p | Ode (_, Some p) -> check_formula p
  | Compose (a, b) | Choice (a, b) ->
      check_program a;
      check_program b
  | Loop a -> check_program a
  | Probchoice xs ->
      let total =
        List.fold xs ~init:Bignum.zero ~f:(fun total (e, a) ->
            check_program a;
            let probability = Bignum.(total + to_const_probability e) in
            if Bignum.(probability < zero || probability > one) then
              raise (StaticError "Probability is not between 0 and 1");
            probability)
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1")
  | Assign _ | Assignany _ | Ode (_, None) -> ()
