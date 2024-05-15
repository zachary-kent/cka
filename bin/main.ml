open Core
open Cka
open SeriesRational

let () =
  let lhs = Seq (Par (Sym 'a', Sym 'b'), Par (Sym 'c', Sym 'd')) in
  let rhs = Par (Seq (Sym 'b', Sym 'c'), Seq (Sym 'a', Sym 'd')) in
  printf "%b\n" @@ Refinement.refines lhs rhs

  (* for i = 1 to 100 do
    let random = Splittable_random.State.create Random.State.default in
    let e1 =
      Quickcheck.Generator.generate SeriesRational.quickcheck_generator
        ~size:100 ~random
    in
    let e2 =
      Quickcheck.Generator.generate SeriesRational.quickcheck_generator
        ~size:100 ~random
    in
    
  done *)
