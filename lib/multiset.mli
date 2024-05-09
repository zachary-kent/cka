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

module Make (Elt : Set.Elt) : S with module Elt := Elt
