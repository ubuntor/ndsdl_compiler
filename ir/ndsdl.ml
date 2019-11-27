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
  [@@deriving sexp]
end

(* hack for separate modules with mutually recursive types *)
module rec Outer : sig
  module Program : sig
    type t =
      | Assign of string * Term.t
      | Assignany of string
      | Test of Outer.Formula.t
      | Compose of t * t
      | Loop of t * t
      | Choice of t * t
      | Probchoice of (int * t) * (int * t) (* TODO *)
    [@@deriving sexp]
  end

  module Formula : sig
    type t =
      | True
      | False
      | Or of t * t
      | Not of t * t
      | And of t * t
      | Implies of t * t
      | Equiv of t * t
      | Eq of Term.t * Term.t
      | Lt of Term.t * Term.t
      | Le of Term.t * Term.t
      | Gt of Term.t * Term.t
      | Ge of Term.t * Term.t
      | Forall of string * t
      | Exists of string * t
      | Box of Outer.Program.t * t
      | Diamond of Outer.Program.t * t
    [@@deriving sexp]
  end
end =
  Outer

module Program = Outer.Program
module Formula = Outer.Formula
