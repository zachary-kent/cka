(** The grammar of series-rational expressions *)
type 'a t =
  | Zero
  | One
  | Sym of 'a
  | Seq of 'a t * 'a t
  | Par of 'a t * 'a t
  | Alt of 'a t * 'a t
  | Star of 'a t

val ewp : 'a t -> bool
(** [ewp e] is [true] iff [e] contains [1] *)
