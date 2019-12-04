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
    | Unop of [ `Neg ] * t
    | Binop of [ `Plus | `Minus | `Times | `Div | `Exp ] * t * t
    | Max of t * t
  [@@deriving sexp]
end

(* duplicate types needed to use [@@deriving sexp] with mutually recursive types in separate modules *)
module rec Program : sig
  type t =
    | Assign of Var.t * Term.t
    | Assignany of Var.t
    | Assignpmf of Var.t * (Term.t * Term.t) list
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Probloop of Term.t * t
    | Choice of t * t
    | Probchoice of (Term.t * t) list
    | Ode of ((Var.t * Term.t) list * Formula.t option)
  [@@deriving sexp]
end = struct
  type t =
    | Assign of Var.t * Term.t
    | Assignany of Var.t
    | Assignpmf of Var.t * (Term.t * Term.t) list
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Probloop of Term.t * t
    | Choice of t * t
    | Probchoice of (Term.t * t) list
    | Ode of ((Var.t * Term.t) list * Formula.t option)
  [@@deriving sexp]
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
    | Bound of Program.t * t * Term.t
  [@@deriving sexp]
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
    | Bound of Program.t * t * Term.t
  [@@deriving sexp]
end
