%{
open Syntax
%}

%token <int> INT
%token <string> IDENT
%token LPAREN
%token RPAREN
%token FUN
%token THIN_ARROW
%token PLUS
%token MINUS
%token TIMES
%token FILTER
%token IN
%token EOF

%left PLUS MINUS
%left TIMES

%start <Syntax.Exp.t option> top

%%

top:
  | EOF { None }
  | e = exp; EOF { Some e }
  ;

exp:
  | i = INT { Exp.Int i }
  | x = IDENT  { Exp.Var x }
  | FUN; x = IDENT; THIN_ARROW; e = exp { Exp.Fun (x, e) }
  | e1 = exp; PLUS; e2 = exp { Exp.Add (e1, e2) }
  | e1 = exp; MINUS; e2 = exp { Exp.Sub (e1, e2) }
  | e1 = exp; TIMES; e2 = exp { Exp.Mul (e1, e2) }
  | e1 = exp; LPAREN; e2 = exp; RPAREN { Exp.App (e1, e2) }
  | LPAREN; e = exp; RPAREN { e }
  ;
