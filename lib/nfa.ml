open Core

module type S = sig
  module State : Hashtbl.Key
  module Alphabet : Hashtbl.Key
  module States : Set.S with type Elt.t = State.t

  type t

  val add_transition : t -> State.t -> Alphabet.t option -> State.t -> unit

  val create :
    alphabet:Alphabet.t list -> start:State.t -> final:(State.t -> bool) -> t

  val start : t -> State.t
  val alphabet : t -> Alphabet.t list
  val final : t -> State.t -> bool
  val final_many : t -> States.t -> bool
  val delta : t -> State.t -> Alphabet.t -> States.t
  val delta_many : t -> States.t -> Alphabet.t -> States.t
  val epsilon_closure_many : t -> States.t -> States.t
  val epsilon_closure : t -> State.t -> States.t
  val has_state : t -> State.t -> bool
end

module Make (State : Hashtbl.Key) (Alphabet : Hashtbl.Key) = struct
  module State = State
  module Alphabet = Alphabet

  module AlphabetTable = Hashtbl.Make (struct
    include Hashable.Make_and_derive_hash_fold_t (Alphabet)
    include Alphabet

    type nonrec t = t option [@@deriving sexp, hash, compare]
  end)

  module StateHash = Hashable.Make_and_derive_hash_fold_t (State)
  module States = Set.Make (State)

  type t = {
    alphabet : Alphabet.t list;
    start : State.t;
    transitions : States.t AlphabetTable.t StateHash.Table.t;
    final : State.t -> bool;
  }
  [@@deriving fields]

  let add_transition { transitions; _ } src label dst =
    Hashtbl.update transitions src ~f:(function
      | None -> AlphabetTable.of_alist_exn [ (label, States.singleton dst) ]
      | Some transitions ->
          Hashtbl.update transitions label ~f:(function
            | None -> States.singleton dst
            | Some states -> Set.add states dst);
          transitions)

  let create ~alphabet ~start ~final =
    { alphabet; start; transitions = StateHash.Table.create (); final }

  let final_many { final } qs = Set.exists qs ~f:final

  let delta { transitions; _ } state a =
    Option.value ~default:States.empty
    @@
    let open Option.Let_syntax in
    let%bind transitions = Hashtbl.find transitions state in
    Hashtbl.find transitions (Some a)

  let has_state { transitions; _ } = Hashtbl.mem transitions

  let epsilon_transitions { transitions; _ } state =
    Option.value ~default:States.empty
    @@
    let open Option.Let_syntax in
    let%bind transitions = Hashtbl.find transitions state in
    Hashtbl.find transitions None

  let epsilon_closure_many nfa =
    let rec visit visited states =
      if Set.is_subset states ~of_:visited then visited
      else
        let epsilon_transitions =
          Set.fold states ~init:States.empty ~f:(fun acc state ->
              Set.union acc @@ epsilon_transitions nfa state)
        in
        visit (Set.union visited states) epsilon_transitions
    in
    visit States.empty

  let epsilon_closure nfa s = epsilon_closure_many nfa @@ States.singleton s

  let delta_many nfa ps a =
    ps |> Set.to_list
    |> List.map ~f:(fun p -> delta nfa p a)
    |> States.union_list |> epsilon_closure_many nfa
end

module Inclusion (A : S) (B : S with module Alphabet = A.Alphabet) = struct
  module Relation = Set.Make_plain (struct
    type t = A.State.t * B.States.t [@@deriving compare, sexp_of]
  end)

  let inclusion nfa1 nfa2 =
    let pairs x y = List.cartesian_product (Set.to_list x) [ y ] in
    let rec loop r = function
      | [] -> true
      | ((x, y) as p) :: todo ->
          if Set.mem r p (* if x already related to r, skip *) then loop r todo
          else if A.final nfa1 x && (not @@ B.final_many nfa2 y) then false
          else
            let todo' =
              List.concat_map (A.alphabet nfa1) (fun a ->
                  pairs
                    (A.epsilon_closure_many nfa1 @@ A.delta nfa1 x a)
                    (B.delta_many nfa2 y a))
            in
            loop (Set.add r p) @@ List.rev_append todo' todo
    in
    loop Relation.empty
    @@ pairs
         (A.epsilon_closure nfa1 @@ A.start nfa1)
         (B.epsilon_closure nfa2 @@ B.start nfa2)
end
