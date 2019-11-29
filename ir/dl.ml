open! Core

module Var = struct
  type t = string [@@deriving sexp]
end

module Number = struct
  type t = string [@@deriving sexp]
end

module Term = struct
  type t =
    | Var of Var.t
    | Number of Number.t
    | Plus of t * t
    | Minus of t * t
    | Times of t * t
    | Div of t * t
    | Exp of t * t
    | Neg of t
  [@@deriving sexp]

  let rec to_string term =
    match term with
    | Var x -> x
    | Number n -> n
    | Plus (e1, e2) -> Printf.sprintf "(%s+%s)" (to_string e1) (to_string e2)
    | Minus (e1, e2) -> Printf.sprintf "(%s-%s)" (to_string e1) (to_string e2)
    | Times (e1, e2) -> Printf.sprintf "(%s*%s)" (to_string e1) (to_string e2)
    | Div (e1, e2) -> Printf.sprintf "(%s/%s)" (to_string e1) (to_string e2)
    | Exp (e1, e2) -> Printf.sprintf "(%s^%s)" (to_string e1) (to_string e2)
    | Neg e -> Printf.sprintf "(-%s)" (to_string e)
end

(* duplicate types needed to use [@@deriving sexp] with mutually recursive types in separate modules *)
module rec Program : sig
  type t =
    | Assign of Var.t * Term.t
    | Assignany of Var.t
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Choice of t * t
    | Ode of ((Var.t * Term.t) list * Formula.t option)
  [@@deriving sexp]

  val to_string : t -> string
end = struct
  type t =
    | Assign of Var.t * Term.t
    | Assignany of Var.t
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Choice of t * t
    | Ode of ((Var.t * Term.t) list * Formula.t option)
  [@@deriving sexp]

  let rec to_string program =
    match program with
    | Assign (x, e) -> Printf.sprintf "%s := %s" x (Term.to_string e)
    | Assignany x -> Printf.sprintf "%s :=*" x
    | Test p -> Printf.sprintf "?%s" (Formula.to_string p)
    | Compose (a, b) -> Printf.sprintf "{%s; %s;}" (to_string a) (to_string b)
    | Loop a -> Printf.sprintf "{%s}*" (to_string a)
    | Choice (a, b) -> Printf.sprintf "{%s ++ %s}" (to_string a) (to_string b)
    | Ode (xs, po) ->
        Printf.sprintf "{%s%s}"
          ( List.map xs ~f:(fun (x, e) ->
                Printf.sprintf "%s' = %s" x (Term.to_string e))
          |> String.concat ~sep:", " )
          ( match po with
          | None -> ""
          | Some p -> Printf.sprintf " & %s" (Formula.to_string p) )
end

and Formula : sig
  type t =
    | True
    | False
    | Or of t * t
    | Not of t
    | And of t * t
    | Implies of t * t
    | Equiv of t * t
    | Eq of Term.t * Term.t
    | Lt of Term.t * Term.t
    | Le of Term.t * Term.t
    | Gt of Term.t * Term.t
    | Ge of Term.t * Term.t
    | Neq of Term.t * Term.t
    | Forall of Var.t * t
    | Exists of Var.t * t
    | Box of Program.t * t
    | Diamond of Program.t * t
  [@@deriving sexp]

  val to_string : t -> string
end = struct
  type t =
    | True
    | False
    | Or of t * t
    | Not of t
    | And of t * t
    | Implies of t * t
    | Equiv of t * t
    | Eq of Term.t * Term.t
    | Lt of Term.t * Term.t
    | Le of Term.t * Term.t
    | Gt of Term.t * Term.t
    | Ge of Term.t * Term.t
    | Neq of Term.t * Term.t
    | Forall of Var.t * t
    | Exists of Var.t * t
    | Box of Program.t * t
    | Diamond of Program.t * t
  [@@deriving sexp]

  let rec to_string formula =
    match formula with
    | True -> "true"
    | False -> "false"
    | Or (p, q) -> Printf.sprintf "(%s|%s)" (to_string p) (to_string q)
    | Not p -> Printf.sprintf "(!%s)" (to_string p)
    | And (p, q) -> Printf.sprintf "(%s&%s)" (to_string p) (to_string q)
    | Implies (p, q) -> Printf.sprintf "(%s -> %s)" (to_string p) (to_string q)
    | Equiv (p, q) -> Printf.sprintf "(%s <-> %s)" (to_string p) (to_string q)
    | Eq (e1, e2) ->
        Printf.sprintf "(%s = %s)" (Term.to_string e1) (Term.to_string e2)
    | Lt (e1, e2) ->
        Printf.sprintf "(%s < %s)" (Term.to_string e1) (Term.to_string e2)
    | Le (e1, e2) ->
        Printf.sprintf "(%s <= %s)" (Term.to_string e1) (Term.to_string e2)
    | Gt (e1, e2) ->
        Printf.sprintf "(%s > %s)" (Term.to_string e1) (Term.to_string e2)
    | Ge (e1, e2) ->
        Printf.sprintf "(%s >= %s)" (Term.to_string e1) (Term.to_string e2)
    | Neq (e1, e2) ->
        Printf.sprintf "(%s != %s)" (Term.to_string e1) (Term.to_string e2)
    | Forall (x, p) -> Printf.sprintf "(\\forall %s %s)" x (to_string p)
    | Exists (x, p) -> Printf.sprintf "(\\exists %s %s)" x (to_string p)
    | Box (a, p) ->
        Printf.sprintf "([%s]%s)" (Program.to_string a) (to_string p)
    | Diamond (a, p) ->
        Printf.sprintf "(<%s>%s)" (Program.to_string a) (to_string p)
end
