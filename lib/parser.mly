%{
  open Core
  open SeriesRational

  let sequence es = List.fold_left (List.tl_exn es) ~init:(List.hd_exn es) ~f:(fun s t -> Seq (s, t))
%}

%token <char> CHAR
%token ZERO ONE
%token ALT
%token PAR
%token SEQ
%token STAR
%token LPAREN
%token RPAREN
%token EOF

%left ALT
%left PAR
%left SEQ
%nonassoc STAR


%start <SeriesRational.t> expr

%%

expr:
  | e = sr; EOF { e }

sr:
  | s = sr; SEQ; t = sr { Seq (s, t) }
  | s = sr; ALT; t = sr { Alt (s, t) }
  | s = sr; PAR; t = sr { Par (s, t) }
  | e = sr; STAR { Star e }
  | cs = nonempty_list(char) { sequence cs }
  | ZERO { Zero }
  | ONE { One }
  | es = nonempty_list(parens) { sequence es }

parens:
  | LPAREN; e = sr; RPAREN { e }

char:
  | c = CHAR { Sym c }
