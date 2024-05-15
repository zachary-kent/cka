open Core

module type S = sig
  module State : Map.Key

  module Alphabet : Map.Key

  module States : Set.S with type Elt.t = State.t

  type t

  val add_transition : t -> State.t -> Alphabet.t option -> State.t -> t

  val create :
    alphabet:Alphabet.t list -> start:State.t -> final:(State.t -> bool) -> t

  val start : t -> State.t

  val alphabet : t -> Alphabet.t list

  val final : t -> State.t -> bool

  val final_many : t -> States.t -> bool

  val delta_many : t -> States.t -> Alphabet.t -> States.t

  val epsilon_closure_many : t -> States.t -> States.t

  val epsilon_closure : t -> State.t -> States.t

  val has_state : t -> State.t -> bool

end

module Make (State : Map.Key) (Alphabet : Map.Key) = struct
  module State = State
  module Alphabet = Alphabet

  module StateMap = Map.Make (State)
  module States = Set.Make (State)

  module AlphabetMap = Map.Make (struct
    type t = Alphabet.t option [@@deriving sexp, compare]
  end)

  type t = {
    alphabet : Alphabet.t list;
    start : State.t;
    transitions : States.t AlphabetMap.t StateMap.t;
    final : State.t -> bool;
  }
  [@@deriving fields]

  let add_transition ({ transitions; _ } as nfa) src label dst =
    {
      nfa with
      transitions =
        Map.update transitions src ~f:(function
          | None -> AlphabetMap.singleton label @@ States.singleton dst
          | Some transitions ->
              Map.update transitions label ~f:(function
                | None -> States.singleton dst
                | Some states -> Set.add states dst));
    }

  let create ~alphabet ~start ~final =
    {
      alphabet;
      start;
      transitions = StateMap.empty;
      final;
    }

  let final_many { final } qs = Set.exists qs ~f:final

  let delta { transitions; _ } state a =
    Option.value ~default:States.empty @@
      let open Option.Let_syntax in
      let%bind transitions = Map.find transitions state in
      Map.find transitions a

  let has_state { transitions; _ } = Map.mem transitions

  let epsilon_transitions { transitions; _ } state =
    Option.value ~default:States.empty @@
      let open Option.Let_syntax in
      let%bind transitions = Map.find transitions state in
      Map.find transitions None

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

  let epsilon_closure nfa s =
    epsilon_closure_many nfa @@ States.singleton s

  let delta_many nfa ps a =
    ps
    |> Set.to_list
    |> List.map ~f:(fun p -> delta nfa p (Some a))
    |> States.union_list
    |> epsilon_closure_many nfa

end

module Inclusion (A : S) (B : S with module Alphabet = A.Alphabet) = struct
  module Relation = Set.Make (struct
    type t = A.States.t * B.States.t [@@deriving compare, sexp]
  end)

  let inclusion nfa1 nfa2 =
    let rec loop r = function
      | [] -> true
      | ((x, y) as p) :: todo ->
        if Set.mem r p
          (* if x already related to r, skip *)
          then loop r todo
        else
          if A.final_many nfa1 x && not @@ B.final_many nfa2 y then false
          else
            let r' = Set.add r p in
            let todo' =
              List.rev_map (A.alphabet nfa1) (fun a -> (A.delta_many nfa1 x a, B.delta_many nfa2 y a))
            in
            loop r' @@ List.rev_append todo todo'
    in
    let todo =
      [
        ( A.epsilon_closure nfa1 @@ A.start nfa1,
          B.epsilon_closure nfa2 @@ B.start nfa2 );
      ]
    in
    loop Relation.empty todo
end
