open Core

type t =
  | Zero
  | One
  | Sym of char
  | Seq of t * t
  | Par of t * t
  | Alt of t * t
  | Star of t
[@@deriving quickcheck, sexp]
