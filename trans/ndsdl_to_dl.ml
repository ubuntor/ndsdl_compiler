open Core

exception StaticError of string

let fresh_var_counter = ref 0

let fresh_var () =
  let var = "tt_" ^ Int.to_string !fresh_var_counter in
  incr fresh_var_counter;
  var

let to_const_probability term =
  let rec to_const (term : Ndsdl.Term.t) =
    match term with
    | Number n -> Bignum.of_string n
    | Unop (`Neg, e) -> Bignum.(zero - to_const e)
    | Binop (`Plus, e1, e2) -> Bignum.(to_const e1 + to_const e2)
    | Binop (`Minus, e1, e2) -> Bignum.(to_const e1 - to_const e2)
    | Binop (`Times, e1, e2) -> Bignum.(to_const e1 * to_const e2)
    | Binop (`Div, e1, e2) -> Bignum.(to_const e1 / to_const e2)
    | Max (e1, e2) -> Bignum.max (to_const e1) (to_const e2)
    | Var _ -> raise (StaticError "probability is not constant")
    | Binop (`Exp, _, _) ->
        raise (StaticError "exp not supported as probability")
  in
  let probability = to_const term in
  if not Bignum.(zero <= probability && probability <= one) then
    raise (StaticError "Probability is not between 0 and 1");
  probability

let check_probability term = ignore (to_const_probability term)

let rec program_to_rev_list (program : Ndsdl.Program.t) =
  match program with
  | Compose (a, b) -> program_to_rev_list b @ program_to_rev_list a
  | _ -> [ program ]

(* See the associated paper for definitions of indicator, rho, and sigma. *)
let indicator formula var : Ndsdl.Formula.t =
  Logicalbinop
    ( `Or,
      Logicalbinop (`And, formula, Compare (`Eq, Var var, Number "1")),
      Logicalbinop
        (`And, Logicalunop (`Not, formula), Compare (`Eq, Var var, Number "0"))
    )

(* We operate on a reversed list of programs so we can access the last program easily. *)
let rec rho ((rev_program_list : Ndsdl.Program.t list), formula) :
    Ndsdl.Formula.t list * Ndsdl.Term.t =
  match rev_program_list with
  | [] ->
      let y = fresh_var () in
      ([ indicator formula y ], Var y)
  | (Assign _ as a) :: programs
  | (Assignany _ as a) :: programs
  | (Test _ as a) :: programs
  | (Loop _ as a) :: programs
  | (Ode _ as a) :: programs ->
      rho (programs, Diamond (a, formula))
  | (Probloop (p, _) as a) :: programs ->
      check_probability p;
      rho (programs, Diamond (a, formula))
  | Assignpmf (x, choices) :: programs ->
      (* probabilities are checked when translating the final formula *)
      let choices =
        List.map choices ~f:(fun (p, e) ->
            let x, e = rho (programs, Diamond (Assign (x, e), formula)) in
            (x, Ndsdl.Term.Binop (`Times, p, e)))
      in
      List.fold choices ~init:([], Ndsdl.Term.Number "0")
        ~f:(fun (totalx, totale) (x, e) ->
          (totalx @ x, Binop (`Plus, totale, e)))
  | Choice (a, b) :: programs ->
      let x1, e1 = rho (a :: programs, formula) in
      let x2, e2 = rho (b :: programs, formula) in
      (x1 @ x2, Max (e1, e2))
  | Probchoice choices :: programs ->
      (* probabilities are checked when translating the final formula *)
      let choices =
        List.map choices ~f:(fun (p, a) ->
            let x, e = rho (a :: programs, formula) in
            (x, Ndsdl.Term.Binop (`Times, p, e)))
      in
      List.fold choices ~init:([], Ndsdl.Term.Number "0")
        ~f:(fun (totalx, totale) (x, e) ->
          (totalx @ x, Binop (`Plus, totale, e)))
  | (Compose (_, _) as a) :: programs ->
      rho (program_to_rev_list a @ programs, formula)

let sigma ((preconditions, bound), prob) : Ndsdl.Formula.t =
  check_probability prob;
  let precondition =
    List.fold preconditions ~init:Ndsdl.Formula.True ~f:(fun p q ->
        Logicalbinop (`And, p, q))
  in
  Logicalbinop (`Implies, precondition, Compare (`Le, bound, prob))

(* translate n-ary choice to tree of binary choices *)
let rec translate_choices choices : Dl.Program.t =
  match choices with
  | [] -> raise (StaticError "no choices")
  | [ a ] -> a
  | x :: xs -> Choice (x, translate_choices xs)

let rec translate_term (term : Ndsdl.Term.t) : Dl.Term.t =
  match term with
  | Var x -> Var x
  | Number n -> Number n
  | Unop (op, e) -> Unop (op, translate_term e)
  | Binop (op, e1, e2) -> Binop (op, translate_term e1, translate_term e2)
  | Max (e1, e2) -> Max (translate_term e1, translate_term e2)

let rec translate_formula (formula : Ndsdl.Formula.t) : Dl.Formula.t =
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
  | Bound (a, p, e) -> translate_formula (sigma (rho ([ a ], p), e))

and translate_program (program : Ndsdl.Program.t) : Dl.Program.t =
  match program with
  | Assign (x, e) -> Assign (x, translate_term e)
  | Assignany x -> Assignany x
  | Assignpmf (x, choices) ->
      let choices =
        List.filter_map choices ~f:(fun (p, e) ->
            let p = to_const_probability p in
            if Bignum.(p = zero) then None else Some (p, translate_term e))
      in
      let total, choices =
        List.fold_map choices ~init:Bignum.zero ~f:(fun total (p, e) ->
            (Bignum.(total + p), Dl.Program.Assign (x, e)))
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1");
      translate_choices choices
  | Test p -> Test (translate_formula p)
  | Compose (a, b) -> Compose (translate_program a, translate_program b)
  | Loop a -> Loop (translate_program a)
  | Probloop (e, a) ->
      let probability = to_const_probability e in
      if Bignum.(probability = zero) then Test True
      else if Bignum.(probability = one) then
        Compose (Loop (translate_program a), Test False)
      else Loop (translate_program a)
  | Choice (a, b) -> Choice (translate_program a, translate_program b)
  | Probchoice choices ->
      let choices =
        List.filter_map choices ~f:(fun (p, a) ->
            let p = to_const_probability p in
            if Bignum.(p = zero) then None else Some (p, translate_program a))
      in
      let total, choices =
        List.fold_map choices ~init:Bignum.zero ~f:(fun total (p, a) ->
            (Bignum.(total + p), a))
      in
      if Bignum.(total <> one) then
        raise (StaticError "Probabilities do not sum to 1");
      translate_choices choices
  | Ode (xs, po) ->
      let xs = List.map xs ~f:(fun (x, e) -> (x, translate_term e)) in
      let po = Option.map po ~f:(fun p -> translate_formula p) in
      Ode (xs, po)
