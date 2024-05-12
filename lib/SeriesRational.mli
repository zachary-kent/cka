(** The grammar of series-rational expressions *)
type t =
  | Zero
  | One
  | Sym of char
  | Seq of t * t
  | Par of t * t
  | Alt of t * t
  | Star of t
