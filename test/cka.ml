open Core
open OUnit2
open Cka
open SeriesRational

let parse s = Parser.expr Lexer.read @@ Lexing.from_string s

let equal_test name s t expected =
  name >:: fun _ ->
  let s = parse s in
  let t = parse t in
  assert_equal expected (Refinement.refines s t && Refinement.refines t s)

let refines_test name s t expected =
  name >:: fun _ ->
  let s = parse s in
  let t = parse t in
  assert_equal expected (Refinement.refines s t)

let exchange_lhs = "(a || b) (c || d)"
let exchange_rhs = "bc || ad"

let () =
  run_test_tt_main
    ("CKA test suite"
    >::: [
           equal_test "a** = a*" "a*" "a**" true;
           equal_test "(a + b)* = a*(ba*)*" "(a + b)*" "a*; (ba*)*" true;
           refines_test "exchange law" exchange_lhs exchange_rhs true;
         ])
