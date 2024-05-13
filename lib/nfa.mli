open Core

module S : sig
  module State : Comparable

  type t
  (** The abstract type of an NFA *)
end
