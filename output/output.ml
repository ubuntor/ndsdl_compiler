open Core

let rec gather_vars_term (term : Dl.Term.t) =
  match term with
  | Var x -> String.Set.singleton x
  | Number _ -> String.Set.empty
  | Unop (_, e) -> gather_vars_term e
  | Binop (_, e1, e2) ->
      String.Set.union (gather_vars_term e1) (gather_vars_term e2)

let rec gather_vars_formula (formula : Dl.Formula.t) =
  match formula with
  | True | False -> String.Set.empty
  | Logicalunop (_, p) -> gather_vars_formula p
  | Logicalbinop (_, p, q) ->
      String.Set.union (gather_vars_formula p) (gather_vars_formula q)
  | Compare (_, e1, e2) ->
      String.Set.union (gather_vars_term e1) (gather_vars_term e2)
  | Forall (x, p) | Exists (x, p) -> String.Set.remove (gather_vars_formula p) x
  | Box (a, p) | Diamond (a, p) ->
      String.Set.union (gather_vars_program a) (gather_vars_formula p)

and gather_vars_program (program : Dl.Program.t) =
  match program with
  | Assign (x, e) -> String.Set.(union (singleton x) (gather_vars_term e))
  | Assignany x -> String.Set.singleton x
  | Test p -> gather_vars_formula p
  | Compose (a, b) | Choice (a, b) ->
      String.Set.union (gather_vars_program a) (gather_vars_program b)
  | Loop a -> gather_vars_program a
  | Ode (xs, po) ->
      String.Set.union
        ( List.map xs ~f:(fun (x, e) ->
              String.Set.(union (singleton x) (gather_vars_term e)))
        |> String.Set.union_list )
        ( match po with
        | None -> String.Set.empty
        | Some p -> gather_vars_formula p )

let output_to_file dl ~output_file =
  let out = Out_channel.create output_file in
  let vars = gather_vars_formula dl in
  Out_channel.output_string out "ProgramVariables\n";
  String.Set.iter vars ~f:(fun x ->
      Out_channel.output_string out (Printf.sprintf "  Real %s;\n" x));
  Out_channel.output_string out "End.\n\nProblem\n  ";
  Out_channel.output_string out (Dl.Formula.to_string dl);
  Out_channel.newline out;
  Out_channel.output_string out "End.\n";
  Out_channel.close out
