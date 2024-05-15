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

module Relation = Set.Make (struct
  type t = PetriNet.Place.t * PetriNet.Place.t [@@deriving compare, sexp]
end)

module Composite =
  Make
    (struct
      type t = PetriNet.Place.Set.t * PetriNet.Place.Set.t * Relation.t
      [@@deriving compare, sexp]
    end)
    (struct
      type t = PetriNet.Transition.t
      [@@deriving compare, sexp]
    end)

let matching_visible_transitions (n1 : PetriNet.t) (n2 : PetriNet.t) =
  let open PetriNet in
  List.cartesian_product (visible_transitions n1) (visible_transitions n2)
  |> List.filter_map ~f:(fun ((p0, a, p1), (q0, b, q1)) ->
         if Char.equal a b then Some (a, p0, p1, q0, q1) else None)

let composite (n1 : PetriNet.t) (n2 : PetriNet.t) =
  let open PetriNet in
  let silent_transitions_n1 = silent_transitions n1 in
  let silent_transitions_n2 = silent_transitions n2 in
  let matching_visible_transitions = matching_visible_transitions n1 n2 in
  let start =
    ( Place.Set.singleton n1.initial_place,
      Place.Set.singleton n2.initial_place,
      Relation.singleton (n1.initial_place, n2.initial_place) )
  in
  let worklist = Queue.singleton start in
  let alphabet = n1.transitions in
  let final (c1, c2, _) =
    Set.equal c1 (Place.Set.singleton n1.final_place)
    && Set.equal c2 (Place.Set.singleton n2.final_place)
  in
  let composite = ref @@ Composite.create ~alphabet ~start ~final in
  (* let count = ref 0 in *)
  let rec loop () =
    match Queue.dequeue worklist with
    | None -> ()
    | Some ((c1, c2, r) as state) ->
        if not (Composite.has_state !composite state) then begin
          (* incr count; *)
          (* if !count mod 1000 = 0 then
            printf "%d states\n" !count; *)
          List.iter silent_transitions_n1 ~f:begin 
            fun (pre, post) ->
              if Set.is_subset pre ~of_:c1 then begin
                let c1' = Set.(union (diff c1 pre) post) in
                let r' = ref Relation.empty in
                Set.iter r ~f:begin
                  fun (p, q) ->
                    if not (Set.mem post p) then begin
                      r' := Set.add !r' (p, q)
                    end;
                    if Set.mem pre p then begin
                      r' := Set.union !r' @@ Relation.map post ~f:(fun p' -> (p', q))
                    end
                end;
                let state' = (c1', c2, !r') in
                let transition = Transition.Silent (pre, post) in
                composite := Composite.add_transition !composite state (Some transition) state';
                Queue.enqueue worklist state'
              end
          end;
          List.iter silent_transitions_n2 ~f:begin
            fun (pre, post) ->
              if Set.is_subset pre ~of_:c2 then begin
                let c2' = Set.(union (diff c2 pre) post) in
                let r' = ref Relation.empty in
                let map = Place.Table.create () in
                Set.iter r ~f:begin
                  fun (p, q) ->
                    if not (Set.mem post q) then begin
                      r' := Set.add !r' (p, q)
                    end;
                    if Set.mem pre q then begin
                      Hashtbl.update map p ~f:begin
                        function
                        | None -> Place.Set.singleton q
                        | Some places -> Set.add places q
                      end
                    end
                end;
                Hashtbl.iteri map ~f:begin
                  fun ~key:p ~data ->
                    if Set.equal data pre then begin
                      r' := Set.union !r' @@ Relation.map post ~f:(fun q -> (p, q))
                    end
                end;
                let state' = (c1, c2', !r') in
                composite := Composite.add_transition !composite state None state';
                Queue.enqueue worklist state'
              end
          end;
          List.iter matching_visible_transitions ~f:begin
            fun (a, p0, p1, q0, q1) -> 
              if Set.mem r (p0, q0) then begin
                let c1' = Set.(add (remove c1 p0) p1) in
                let c2' = Set.(add (remove c2 q0) q1) in
                let r' = ref @@ Relation.singleton (p1, q1) in
                Set.iter r ~f:begin
                  fun (p, q) ->
                    if p = p0 then begin
                      r' := Set.add !r' (p1, q)
                    end;
                    if p <> p1 && q <> q1 then begin
                      r' := Set.add !r' (p, q)
                    end
                end;
                let state' = (c1', c2', !r') in
                let transition = Transition.Visible (p0, a, p1) in
                composite := Composite.add_transition !composite state (Some transition) state';
                Queue.enqueue worklist state'
              end
          end
        end;
        loop ()
  in
  loop ();
  !composite


