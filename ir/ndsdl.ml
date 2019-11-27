open! Core

module Term = struct
  type t =
    | Var of string
    | Const of int (* TODO *)
    | Plus of t * t
    | Minus of t * t
    | Times of t * t
    | Div of t * t
    | Exp of t * t
    | Prime of t
    | Neg of t
  [@@deriving sexp]
end

(* duplicate types needed to use [@@deriving sexp] with mutually recursive types in separate modules *)
module rec Program : sig
  type t =
    | Assign of string * Term.t
    | Assignany of string
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Choice of t * t
    | Probchoice of (int * t) * (int * t) (* TODO *)
  [@@deriving sexp]
end = struct
  type t =
    | Assign of string * Term.t
    | Assignany of string
    | Test of Formula.t
    | Compose of t * t
    | Loop of t
    | Choice of t * t
    | Probchoice of (int * t) * (int * t) (* TODO *)
  [@@deriving sexp]
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
    | Forall of string * t
    | Exists of string * t
    | Box of Program.t * t
    | Diamond of Program.t * t
  [@@deriving sexp]
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
    | Forall of string * t
    | Exists of string * t
    | Box of Program.t * t
    | Diamond of Program.t * t
  [@@deriving sexp]
end
