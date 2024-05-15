open Core

module type S = sig
  module State : Map.Key
  (** The abstract state of an NFA *)

  module Alphabet : Map.Key
  (** The abstract alphabet of an NFA *)

  module States : Set.S with type Elt.t = State.t
  (** [States] is a set over states *)

  type t
  (** An NFA with epsilon transitions *)

  val add_transition : t -> State.t -> Alphabet.t option -> State.t -> t
  (** [add_transition nfa src label dst] is [nfa] with an additional labeled transition from [src] to [dst] labeled [label] *)

  val create :
    alphabet:Alphabet.t list -> start:State.t -> final:(State.t -> bool) -> t
  (** [create ~alphabet ~start ~final] is an NFA with start state [start] *)

  val start : t -> State.t
  (** [start nfa] is the start state of [nfa] *)

  val alphabet : t -> Alphabet.t list
  (** [alphabet nfa] is the start state of [nfa] *)

  val final : t -> State.t -> bool
  (** [final nfa p] is [true] iff [p] is a final state of [nfa] *)

  val final_many : t -> States.t -> bool
  (** [final_many nfa ps] is [true] iff any state [p] of [ps] is a final state of [nfa] *)

  val delta_many : t -> States.t -> Alphabet.t -> States.t
  (** [delta nfa p a] maps the transition function over every state in p *)

  val epsilon_closure_many : t -> States.t -> States.t
  (** [epsilon_closure_many nfa ps] is the epsilon closure of [ps] *)

  val epsilon_closure : t -> State.t -> States.t
  (** [epsilon_closure nfa p] is the epsilon closure of [ps] *)

  val has_state : t -> State.t -> bool
  (** [has_state nfa state] is [true] iff [nfa] has state [state] *)
end

module Make (State : Map.Key) (Alphabet : Map.Key) :
  S with module State = State and module Alphabet = Alphabet

module Inclusion (A : S) (B : S with module Alphabet = A.Alphabet) : sig
  val inclusion : A.t -> B.t -> bool
  (** [inclusion n1 n2] is [true] iff the langauge recognized by [n1] is a subset of the language recognized by [n2] *)
end
