open Core

module Make (State : sig
  include Hashable
  include Sexpable with type t := t
end) (TransitionLabel : sig
  include Hashable
  include Sexpable with type t := t
end) =
struct
  module StateTable = Hashtbl.Make (State)
  module States = Hash_set.Make (State)

  module Transition = struct
    type t = State.t * TransitionLabel.t * State.t
    [@@deriving compare, sexp, hash]
  end

  module TransitionSet = Hash_set.Make (Transition)

  type t = {
    start : State.t;
    transitions : TransitionSet.t StateTable.t;
    final : States.t;
  }

  let add_transition { transitions; _ } src label dst =
    let trans = (src, label, dst) in
    Hashtbl.update transitions src ~f:(function
      | None -> TransitionSet.of_list [ trans ]
      | Some transitions ->
          Hash_set.add transitions trans;
          transitions)

  let create start =
    {
      start;
      transitions = StateTable.of_alist_exn [ (start, TransitionSet.create ()) ];
      final = States.create ();
    }
end

module Relation = struct
  module Pair = struct
    type t = PetriNet.Place.t * PetriNet.Place.t
    [@@deriving compare, hash, sexp]
  end

  include Set.Make (Pair)

  let hash_fold_t = Set.hash_fold_direct Pair.hash_fold_t
end

module M = struct
  module PlaceSet = struct
    include PetriNet.Place.Set

    let hash_fold_t = Set.hash_fold_direct Int.hash_fold_t
  end

  type t = PlaceSet.t * PlaceSet.t * Relation.t [@@deriving compare, sexp, hash]
end

module Composite =
  Make
    (struct
      type t = PetriNet.Place.Set.t * PetriNet.Place.Set.t * Relation.t
      [@@deriving compare, sexp, hash]
    end)
    (PetriNet.Transition)

let composite n1 n2 = ()
