open! Core

module Relation = struct
  module Pair = struct
    type t = PetriNet.Place.t * PetriNet.Place.t
    [@@deriving compare, sexp, hash]
  end

  include Set.Make (Pair)

  let hash_fold_t : t Hash.folder = Set.hash_fold_direct Pair.hash_fold_t
end

module Composite =
  Nfa.Make
    (struct
      type t = PetriNet.Places.t * PetriNet.Places.t * Relation.t [@@deriving compare, hash, sexp]
    end)
    (struct
      type t = PetriNet.Transition.t [@@deriving compare, hash, sexp]
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
    ( Places.singleton n1.initial_place,
      Places.singleton n2.initial_place,
      Relation.singleton (n1.initial_place, n2.initial_place) )
  in
  let worklist = Queue.singleton start in
  let alphabet = n1.transitions in
  let final (c1, c2, _) =
    Set.equal c1 (Places.singleton n1.final_place)
    && Set.equal c2 (Places.singleton n2.final_place)
  in
  let composite = Composite.create ~alphabet ~start ~final in
  (* let count = ref 0 in *)
  let rec loop () =
    match Queue.dequeue worklist with
    | None -> ()
    | Some ((c1, c2, r) as state) ->
        if not (Composite.has_state composite state) then begin
          (* incr count;
          if !count mod 2 = 0 then
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
                Composite.add_transition composite state (Some transition) state';
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
                        | None -> Places.singleton q
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
                Composite.add_transition composite state None state';
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
                Composite.add_transition composite state (Some transition) state';
                Queue.enqueue worklist state'
              end
          end
        end;
        loop ()
  in
  loop ();
  composite


module TransitionAutomaton =
  Nfa.Make
    (PetriNet.Places)
    (PetriNet.Transition)

let transition (n : PetriNet.t) =
  let open PetriNet in
  let open Transition in
  let worklist = Queue.singleton @@ Places.singleton n.initial_place in
  let start = Places.singleton n.initial_place in
  let final = Set.equal @@ Places.singleton n.final_place in
  let alphabet = n.transitions in
  let nfa = TransitionAutomaton.create ~alphabet ~start ~final in
  let rec loop () =
    match Queue.dequeue worklist with
    | None -> ()
    | Some c ->
      if not @@ TransitionAutomaton.has_state nfa c then begin
        List.iter n.transitions ~f:begin
          function
          | Silent (pre, post) as transition ->
              if Set.is_subset pre ~of_:c then begin
                (* If pre set is a subset of the configuration, the transition is active *)
                let c' = Set.(union (diff c pre) post) in
                Queue.enqueue worklist c';
                TransitionAutomaton.add_transition nfa c (Some transition) c'
              end
          | Visible (pre, a, post) as transition ->
              if Set.mem c pre then begin
                (* If pre set is a subset of the configuration, the transition is active *)
                let c' = Set.(add (remove c pre) post) in
                Queue.enqueue worklist c';
                TransitionAutomaton.add_transition nfa c (Some transition) c'
              end
        end
      end;
      loop ()
  in
  loop ();
  nfa

module Inclusion = Nfa.Inclusion (TransitionAutomaton) (Composite)

let refines e1 e2 =
  let n1 = PetriNet.of_expr e1 in
  let n2 = PetriNet.of_expr e2 in
  let transition = transition n1 in
  let composite = composite n1 n2 in
  Inclusion.inclusion transition composite
