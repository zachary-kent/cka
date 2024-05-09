open Core

module type S = sig
  module Elt : Set.Elt

  type t

  val empty : t
  val add : t -> Elt.t -> t
  val count : t -> Elt.t -> int
  val of_list : Elt.t list -> t
  val singleton : Elt.t -> t
  val union : t -> t -> t
  val equal : t -> t -> bool
end

module Make (Elt : Set.Elt) = struct
  module S = Map.Make (Elt)

  type t = int S.t

  let empty = S.empty
  let add = Map.update ~f:(function Some count -> count + 1 | None -> 1)
  let count m e = Option.value ~default:0 @@ Map.find m e
  let of_list = List.fold ~f:add ~init:empty
  let singleton x = of_list [ x ]

  let union : t -> t -> t =
    Map.merge ~f:(fun ~key:_ -> function
      | `Left c | `Right c -> Some c | `Both (l, r) -> Some (l + r))

  let equal : t -> t -> bool = Map.equal ( = )
end
