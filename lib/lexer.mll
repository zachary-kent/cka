{
open Core
open Parser
}

let letter = ['a'-'z']
let whitespace = [' ' '\n' '\t']

rule read =
  parse
  | whitespace { read lexbuf }
  | letter { CHAR (Lexing.lexeme_char lexbuf 0) }
  | "+" { ALT }
  | "||" { PAR }
  | "*" { STAR }
  | "0" { ZERO }
  | "1" { ONE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
