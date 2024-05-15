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

%left ALT
%left PAR
%nonassoc STAR

%start <SeriesRational.t> expr

%%

expr:
  | e = seq; EOF { e }

seq:
  | s = seq; t = sr { Seq (s, t) }
  | e  = sr { e }

sr:
  | s = sr; ALT; t = sr { Alt (s, t) }
  | s = sr; PAR; t = sr { Par (s, t) }
  | e = sr; STAR { Star e }
  | c = CHAR { Sym c }
  | ZERO { Zero }
  | ONE { One }
  | LPAREN; e = seq; RPAREN { e }
