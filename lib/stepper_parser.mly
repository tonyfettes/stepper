%{
open Stepper_syntax
%}

%token <int> INT
%token <string> IDENT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token FUN
%token FIX
%token THIN_ARROW
%token PLUS
%token MINUS
%token TIMES
%token EQ
%token EQEQ
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
%token DOLLAR_X
%token IF
%token THEN
%token ELSE
%token LET
%token REC

%right IN
%right ELSE
%right THIN_ARROW
%left EQEQ
%left PLUS MINUS
%left TIMES
%nonassoc LPAREN

%start <Stepper_syntax.Expr.t option> top

%%

top:
  | EOF { None }
  | e = expr; EOF { Some e }
  ;

act:
  | EVAL { Act.Eval }
  | PAUSE { Act.Pause }
  ;

gas:
  | ONE { Gas.One }
  | ALL { Gas.All }
  ;

expr:
  | i = INT { Expr.Int i }
  | x = IDENT  { Expr.Var x }
  | TRUE { Expr.Bool true }
  | FALSE { Expr.Bool false }
  | FUN x = IDENT THIN_ARROW e = expr { Expr.Fun (x, e) }
  | FIX x = IDENT THIN_ARROW e = expr { Expr.Fix (x, e) }
  | e1 = expr PLUS e2 = expr {  Expr.Add (e1, e2) }
  | e1 = expr MINUS e2 = expr { Expr.Sub (e1, e2) }
  | e1 = expr TIMES e2 = expr { Expr.Mul (e1, e2) }
  | e1 = expr EQEQ e2 = expr { Expr.Eq (e1, e2) }
  | e1 = expr LPAREN e2 = expr RPAREN { Expr.Ap (e1, e2) }
  | LET x = IDENT EQ e1 = expr IN e2 = expr { Expr.Ap (Fun (x, e2), e1) }
  | LET REC x = IDENT EQ e1 = expr IN e2 = expr { Expr.Ap (Fun (x, e2), Fix (x, e1)) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { Expr.If (e1, e2, e3) }
  | FILTER p = pat DO a = act FOR g = gas IN e = expr { Expr.Filter (p, a, g, e) }
  | DO a = act FOR g = gas AT l = INT IN e = expr { Expr.Residue (a, g, l, e) }
  | EVAL p = pat IN e = expr { Expr.Filter (p, Eval, All, e) }
  | HIDE p = pat IN e = expr { Expr.Filter (p, Eval, One, e) }
  | PAUSE p = pat IN e = expr { Expr.Filter (p, Pause, One, e) }
  | DEBUG p = pat IN e = expr { Expr.Filter (p, Pause, All, e) }
  | LPAREN e = expr RPAREN { e }
  ;

pat:
  | DOLLAR_E { Pat.Any }
  | DOLLAR_V { Pat.Val }
  | x = IDENT { Pat.Var x }
  | i = INT { Pat.Int i }
  | FUN x = IDENT THIN_ARROW e = expr { Pat.Fun (x, e) }
  | FUN DOLLAR_X THIN_ARROW e = expr { Pat.Fun_any e }
  | e1 = pat PLUS e2 = pat { Pat.Add (e1, e2) }
  | e1 = pat MINUS e2 = pat { Pat.Sub (e1, e2) }
  | e1 = pat TIMES e2 = pat { Pat.Mul (e1, e2) }
  | e1 = pat LPAREN e2 = pat RPAREN { Pat.Ap (e1, e2) }
  | LPAREN e = pat RPAREN { e }
  ;
