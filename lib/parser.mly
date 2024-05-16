%{
  open Core
  open SeriesRational
%}

%token <char> CHAR
%token ZERO ONE
%token ALT
%token PAR
%token STAR
%token LPAREN
%token RPAREN
%token EOF

%start <SeriesRational.t> expr

%%

expr:
  | e = alt_expr; EOF { e }

alt_expr:
  | s = alt_expr; ALT; t = par_expr { Alt (s, t) }
  | e = par_expr { e }

par_expr:
  | s = par_expr; PAR; t = seq_expr { Par (s, t) }
  | e = seq_expr { e }

seq_expr:
  | s = seq_expr; t = base_expr { Seq (s, t) }
  | e  = base_expr { e }

base_expr:
  | e = base_expr; STAR { Star e }
  | c = CHAR { Sym c }
  | ZERO { Zero }
  | ONE { One }
  | LPAREN; e = alt_expr; RPAREN { e }
