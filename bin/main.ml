open Core
open Cka
open SeriesRational

let parse s =
  Option.try_with @@ fun () -> Parser.expr Lexer.read @@ Lexing.from_string s

let rec input_term () =
  printf ">>> %!";
  let default () =
    print_endline "Sorry, that is not a valid term. Please try again.";
    input_term ()
  in
  Option.value_or_thunk ~default
  @@
  let open Option.Monad_infix in
  In_channel.input_line In_channel.stdin >>= parse

let () =
  print_endline "Welcome to my CKA decision procedure!";
  print_endline
    "Enter a series-rational (SR) term, then a newline, and then another SR \
     term.";
  print_endline
    {|SR expressions are simply regular expressions extended with parallel composition. For example, "a || b" is the parallel composition of "a" and "b"|};
  print_endline
    {|It will tell you whether the first refines the second; that is, whether the first is "more sequential" than the second, as governed by the exchange axiom|};
  print_endline "Interrupt the process at any time to exit\n";
  try
    while true do
      let s = input_term () in
      let t = input_term () in
      if Refinement.refines s t then
        print_endline "The first term you entered refines the second\n"
      else
        print_endline "The first term you entered does not refine the second\n"
    done
  with _ -> print_endline "Goodbye!"
