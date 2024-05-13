open Core

module type S = sig
  module State : Map.Key
  (** The abstract state of an NFA *)

  module Alphabet : Map.Key
  (** The abstract alphabet of an NFA *)

  type t
  (** An NFA with epsilon transitions *)

  val add_transition : t -> State.t -> Alphabet.t option -> State.t -> t
  (** [add_transition nfa src label dst] is [nfa] with an additional labeled transition from [src] to [dst] labeled [label] *)

  val create : State.t -> t
  (** [create start] is an NFA with start state [start] *)

  val has_state : t -> State.t -> bool
  (** [has_state nfa state] is [true] iff [nfa] has state [state] *)
end

module Make (State : Map.Key) (Alphabet : Map.Key) :
  S with module State := State and module Alphabet := Alphabet
