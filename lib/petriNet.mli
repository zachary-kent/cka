open Core
module Place = Int

module Places : sig
  include Set.S with type Elt.t = Place.t

  val hash_fold_t : t Hash.folder

  val hash : t -> int
end

module Transition : sig
  type t =
    | Silent of Places.t * Places.t
    | Visible of Place.t * char * Place.t
  [@@deriving sexp, compare, hash]

  (** Represent a transition as a triple consisting of the pre-set of a transition, its label, and the post set.
      A transition is enabled if every place in its preset has a token on it *)
end

(* A Petri net with labeled transitions *)
type t = {
  places : Places.t;
  transitions : Transition.t list;
  initial_place : Place.t;
  final_place : Place.t;
}

type configuration = Places.t
(** A configuration is a set of places; those that have a token on them in a Petri net *)

val of_expr : SeriesRational.t -> t
(** [of_expr e] is a Petri net whose pomset language is equivalent to that of [e]x *)

val silent_transitions : t -> (Places.t * Places.t) list
(** [silent_transitions net] is the list of all of the silent transitions of [net] *)

val visible_transitions : t -> (Place.t * char * Place.t) list
(** [visible_transitions net] is the list of all of the visible transitions of [net] *)
