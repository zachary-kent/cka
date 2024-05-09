open Core

type 'a t =
  | Zero
  | One
  | Sym of 'a
  | Seq of 'a t * 'a t
  | Par of 'a t * 'a t
  | Alt of 'a t * 'a t
  | Star of 'a t
[@@deriving compare, sexp, variants]

let rec ewp = function
  | Zero | Sym _ -> false
  | One | Star _ -> true
  | Seq (s, t) | Par (s, t) -> ewp s && ewp t
  | Alt (s, t) -> ewp s || ewp t

module Derivatives (Alphabet : Set.Elt) = struct
  module Elt = struct
    type nonrec t = Alphabet.t t

    let compare = compare Alphabet.compare
    let t_of_sexp = t_of_sexp Alphabet.t_of_sexp
    let sexp_of_t = sexp_of_t Alphabet.sexp_of_t
  end

  module TermSet = Set.Make (Elt)

  let rec ewp = function
    | Zero | Sym _ -> false
    | One | Star _ -> true
    | Seq (s, t) | Par (s, t) -> ewp s && ewp t
    | Alt (s, t) -> ewp s || ewp t

  let ( * ) e t = if ewp e then t else TermSet.empty
  let seq t e = TermSet.map ~f:(fun f -> Seq (f, e)) t

  let rec delta e a =
    match e with
    | Zero | One | Par _ -> TermSet.empty
    | Sym b ->
        if Alphabet.compare a b = 0 then TermSet.singleton One
        else TermSet.empty
    | Alt (e, f) -> Set.union (delta e a) (delta f a)
    | Seq (e, f) -> Set.union (seq (delta e a) f) (e * delta f a)
    | Star e -> seq (delta e a) (Star e)

  module M = Multiset.Make (Elt)

  let rec gamma e phi =
    match e with
    | Zero | One | Sym _ -> TermSet.empty
    | Alt (e, f) -> Set.union (gamma e phi) (gamma f phi)
    | Seq (e, f) -> Set.union (seq (gamma e phi) f) (e * gamma f phi)
    | Par (e, f) ->
        if M.equal phi (M.of_list [ e; f ]) then TermSet.singleton One
        else TermSet.empty
    | Star e -> seq (gamma e phi) (Star e)
end
