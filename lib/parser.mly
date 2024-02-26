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
%token EVAL
%token HIDE
%token PAUSE
%token DEBUG
%token DO
%token FOR
%token AT
%token ONE
%token ALL
%token DOLLAR_E
%token DOLLAR_V

%left PLUS MINUS
%left TIMES

%start <Syntax.Exp.t option> top

%%

top:
  | EOF { None }
  | e = exp; EOF { Some e }
  ;

act:
  | EVAL { Act.Eval }
  | PAUSE { Act.Pause }
  ;

gas:
  | ONE { Gas.One }
  | ALL { Gas.All }
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
  | FILTER; p = pat; DO; a = act; FOR; g = gas; IN; e = exp { Exp.Filter (p, a, g, e) }
  | DO; a = act; FOR; g = gas; AT; l = INT; IN; e = exp { Exp.Residue (a, g, l, e) }
  | EVAL; p = pat; IN; e = exp { Exp.Filter (p, Eval, All, e) }
  | HIDE; p = pat; IN; e = exp { Exp.Filter (p, Eval, One, e) }
  | PAUSE; p = pat; IN; e = exp { Exp.Filter (p, Pause, One, e) }
  | DEBUG; p = pat; IN; e = exp { Exp.Filter (p, Pause, All, e) }
  ;

pat:
  | DOLLAR_E { Pat.Any }
  | DOLLAR_V { Pat.Val }
  | i = INT { Pat.Int i }
  | FUN; x = IDENT; THIN_ARROW; e = exp { Pat.Fun (x, e) }
  | e1 = pat; PLUS; e2 = pat { Pat.Add (e1, e2) }
  | e1 = pat; MINUS; e2 = pat { Pat.Sub (e1, e2) }
  | e1 = pat; TIMES; e2 = pat { Pat.Mul (e1, e2) }
  | e1 = pat; LPAREN; e2 = pat; RPAREN { Pat.App (e1, e2) }
  | LPAREN; e = pat; RPAREN { e }
  ;
