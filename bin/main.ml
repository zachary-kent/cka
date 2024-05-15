open Core
open Cka

let () =
  for i = 1 to 100 do
    let random = Splittable_random.State.create Random.State.default in
    let e1 =
      Quickcheck.Generator.generate SeriesRational.quickcheck_generator
        ~size:100 ~random
    in
    let e2 =
      Quickcheck.Generator.generate SeriesRational.quickcheck_generator
        ~size:100 ~random
    in
    print_s @@ SeriesRational.sexp_of_t e1;
    print_s @@ SeriesRational.sexp_of_t e2;
    let n1 = PetriNet.of_expr e1 in
    let n2 = PetriNet.of_expr e2 in
    ignore @@ Nfa.composite n1 n2
  done
