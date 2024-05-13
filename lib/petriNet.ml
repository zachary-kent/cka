open Core
module Place = Int

module Transition = struct
  type t =
    | Silent of Place.Set.t * Place.Set.t
    | Visible of Place.t * char * Place.t
  [@@deriving sexp, compare]
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

let of_expr =
  let fresh =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
  in
  let open Place.Set in
  let rec aux =
    let open SeriesRational in
    let open Transition in
    function
    | Zero ->
        let initial_place = fresh () in
        let final_place = fresh () in
        {
          places = of_list [ initial_place; final_place ];
          transitions = [];
          initial_place;
          final_place;
        }
    | One ->
        let initial_place = fresh () in
        {
          places = singleton initial_place;
          transitions = [];
          initial_place;
          final_place = initial_place;
        }
    | Sym a ->
        let initial_place = fresh () in
        let final_place = fresh () in
        {
          places = of_list [ initial_place; final_place ];
          transitions = [ Visible (initial_place, a, final_place) ];
          initial_place;
          final_place;
        }
    | Alt (e1, e2) ->
        let n1 = aux e1 in
        let n2 = aux e2 in
        let initial_place = fresh () in
        let final_place = fresh () in
        let transitions =
          n1.transitions @ n2.transitions
          @ [
              Silent (singleton initial_place, singleton n1.initial_place);
              Silent (singleton initial_place, singleton n2.initial_place);
              Silent (singleton n1.final_place, singleton final_place);
              Silent (singleton n2.final_place, singleton final_place);
            ]
        in
        let places =
          union_list
            [ of_list [ initial_place; final_place ]; n1.places; n2.places ]
        in
        { places; initial_place; final_place; transitions }
    | Par (e1, e2) ->
        let n1 = aux e1 in
        let n2 = aux e2 in
        let initial_place = fresh () in
        let final_place = fresh () in
        let transitions =
          n1.transitions @ n2.transitions
          @ [
              Silent
                ( singleton initial_place,
                  of_list [ n1.initial_place; n2.initial_place ] );
              Silent
                ( of_list [ n1.final_place; n2.final_place ],
                  singleton final_place );
            ]
        in
        let places =
          union_list
            [ of_list [ initial_place; final_place ]; n1.places; n2.places ]
        in
        { places; initial_place; final_place; transitions }
    | Seq (e1, e2) ->
        let n1 = aux e1 in
        let n2 = aux e2 in
        {
          places = Set.union n1.places n2.places;
          initial_place = n1.initial_place;
          final_place = n2.final_place;
          transitions =
            Silent (singleton n1.final_place, singleton n2.initial_place)
            :: n1.transitions
            @ n2.transitions;
        }
    | Star e ->
        let n = aux e in
        let initial_place = fresh () in
        let final_place = fresh () in
        let transitions =
          n.transitions
          @ [
              Silent (singleton initial_place, singleton n.initial_place);
              Silent (singleton initial_place, singleton final_place);
              Silent (singleton n.final_place, singleton n.initial_place);
              Silent (singleton n.final_place, singleton final_place);
            ]
        in
        {
          places = Set.add (Set.add n.places initial_place) final_place;
          initial_place;
          final_place;
          transitions;
        }
  in
  aux
