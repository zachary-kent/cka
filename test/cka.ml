open Core
open OUnit2
open Cka
open SeriesRational

let parse s = Parser.expr Lexer.read @@ Lexing.from_string s

let equal_test ?(expected = true) name s t =
  name >:: fun _ ->
  let s = parse s in
  let t = parse t in
  assert_equal expected (Refinement.refines s t && Refinement.refines t s)

let refines_test ?(expected = true) name s t =
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
           equal_test "double star elimination" "a*" "a**";
           equal_test "associativity of par" "a || (b || c)" "(a || b) || c";
           equal_test "commutativity of par" "a || b" "b || a";
           equal_test "1 is identity of par" "a || 1" "a";
           equal_test "0 annihilates over par" "a || 0" "0";
           equal_test "|| distributes over +" "a || (b + c)" "a || b + a || c";
           equal_test "0 is identity of +" "a + 0" "a";
           equal_test "idempotence of +" "a + a" "a";
           equal_test "commutativity of +" "a + b" "b + a";
           equal_test "associativity of +" "(a + b) + c" "a + (b + c)";
           equal_test "1 is left identity of seq" "1 e" "e";
           equal_test "1 is right identity of seq" "e 1" "e";
           equal_test "associtivity of sequence" "a(bc)" "(ab)c";
           equal_test "0 left annihilates over sequence" "0 a" "0";
           equal_test "0 right annihilates over sequence" "a 0" "0";
           equal_test "left distributivity of sequence over +" "a(b + c)"
             "ab + ac";
           equal_test "right distributivity of sequence over +" "(a + b)c"
             "ac + bc";
           equal_test "*-unfolding" "a*" "1 + aa*";
           refines_test "exchange law" exchange_lhs exchange_rhs;
         ])
