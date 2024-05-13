open Core
module Place = Int

module Transition : sig
  type t =
    | Silent of Place.Set.t * Place.Set.t
    | Visible of Place.t * char * Place.t
  [@@deriving sexp, compare]

  (** Represent a transition as a triple consisting of the pre-set of a transition, its label, and the post set.
      A transition is enabled if every place in its preset has a token on it *)
end

(* A Petri net with labeled transitions *)
type t = {
  places : Place.Set.t;
  transitions : Transition.t list;
  initial_place : Place.t;
  final_place : Place.t;
}

type configuration = Place.Set.t
(** A configuration is a set of places; those that have a token on them in a Petri net *)

val of_expr : SeriesRational.t -> t
(** [of_expr e] is a Petri net whose pomset language is equivalent to that of [e]x *)
