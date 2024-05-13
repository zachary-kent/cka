open Core

module type S = sig
  module State : Map.Key

  module Alphabet : Map.Key

  type t

  val add_transition : t -> State.t -> Alphabet.t option -> State.t -> t

  val create : State.t -> t

  val has_state : t -> State.t -> bool
end

module Make (State : Map.Key) (Alphabet : Map.Key) = struct
  module StateMap = Map.Make (State)
  module StateSet = Set.Make (State)

  module AlphabetMap = Map.Make (struct
    type t = Alphabet.t option [@@deriving sexp, compare]
  end)

  type t = {
    start : State.t;
    transitions : StateSet.t AlphabetMap.t StateMap.t;
    final : StateSet.t;
  }

  let add_transition ({ transitions; _ } as nfa) src label dst =
    {
      nfa with
      transitions =
        Map.update transitions src ~f:(function
          | None -> AlphabetMap.singleton label @@ StateSet.singleton dst
          | Some transitions ->
              Map.update transitions label ~f:(function
                | None -> StateSet.singleton dst
                | Some states -> Set.add states dst));
    }

  let create start =
    {
      start;
      transitions = StateMap.empty;
      final = StateSet.empty;
    }

  let has_state { transitions; _ } = Map.mem transitions
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
      type t = PetriNet.Place.t * char * PetriNet.Place.t
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
  let composite = ref @@ Composite.create start in
  let rec loop () =
    match Queue.dequeue worklist with
    | None -> ()
    | Some ((c1, c2, r) as state) ->
        if not (Composite.has_state !composite state) then begin
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
                composite := Composite.add_transition !composite state None state';
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
                let transition = (p0, a, p1) in
                composite := Composite.add_transition !composite state (Some transition) state';
                Queue.enqueue worklist state'
              end
          end
        end;
        loop ()
  in
  loop ();
  !composite
  
