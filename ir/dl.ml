(* Based on KeYmaeraXPrettyPrinter.scala *)
open Core

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
    | Unop of [ `Neg ] * t
    | Binop of [ `Plus | `Minus | `Times | `Div | `Exp ] * t * t
  [@@deriving sexp]

  let unop_to_string op = match op with `Neg -> "-"

  let binop_to_string op =
    match op with
    | `Plus -> "+"
    | `Minus -> "-"
    | `Times -> "*"
    | `Div -> "/"
    | `Exp -> "^"

  let rec to_string term =
    match term with
    | Var x -> x
    | Number n -> n
    | Unop (op, e) -> Printf.sprintf "%s(%s)" (unop_to_string op) (to_string e)
    | Binop (op, e1, e2) ->
        Printf.sprintf "(%s)%s(%s)" (to_string e1) (binop_to_string op)
          (to_string e2)
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
    | Assign (x, e) -> Printf.sprintf "%s := %s;" x (Term.to_string e)
    | Assignany x -> Printf.sprintf "%s := *;" x
    | Test p -> Printf.sprintf "?%s;" (Formula.to_string p)
    | Compose (a, b) -> Printf.sprintf "{%s}{%s}" (to_string a) (to_string b)
    | Loop a -> Printf.sprintf "{%s}*" (to_string a)
    | Choice (a, b) -> Printf.sprintf "{%s}++{%s}" (to_string a) (to_string b)
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
    | Logicalunop of [ `Not ] * t
    | Logicalbinop of [ `And | `Or | `Implies | `Equiv ] * t * t
    | Compare of [ `Eq | `Lt | `Le | `Gt | `Ge | `Neq ] * Term.t * Term.t
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
    | Logicalunop of [ `Not ] * t
    | Logicalbinop of [ `And | `Or | `Implies | `Equiv ] * t * t
    | Compare of [ `Eq | `Lt | `Le | `Gt | `Ge | `Neq ] * Term.t * Term.t
    | Forall of Var.t * t
    | Exists of Var.t * t
    | Box of Program.t * t
    | Diamond of Program.t * t
  [@@deriving sexp]

  let logical_unop_to_string op = match op with `Not -> "!"

  let logical_binop_to_string op =
    match op with
    | `And -> "&"
    | `Or -> "|"
    | `Implies -> "->"
    | `Equiv -> "<->"

  let compare_op_to_string op =
    match op with
    | `Eq -> "="
    | `Lt -> "<"
    | `Le -> "<="
    | `Gt -> ">"
    | `Ge -> ">="
    | `Neq -> "!="

  let rec to_string formula =
    match formula with
    | True -> "true"
    | False -> "false"
    | Logicalbinop (op, p, q) ->
        Printf.sprintf "(%s) %s (%s)" (to_string p)
          (logical_binop_to_string op)
          (to_string q)
    | Logicalunop (op, p) ->
        Printf.sprintf "%s(%s)" (logical_unop_to_string op) (to_string p)
    | Compare (op, e1, e2) ->
        Printf.sprintf "(%s) %s (%s)" (Term.to_string e1)
          (compare_op_to_string op) (Term.to_string e2)
    | Forall (x, p) -> Printf.sprintf "\\forall (%s) (%s)" x (to_string p)
    | Exists (x, p) -> Printf.sprintf "\\exists (%s) (%s)" x (to_string p)
    | Box (a, p) ->
        Printf.sprintf "[%s](%s)" (Program.to_string a) (to_string p)
    | Diamond (a, p) ->
        Printf.sprintf "<%s>(%s)" (Program.to_string a) (to_string p)
end
