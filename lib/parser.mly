%{
open Syntax
%}
%token <int32> INT
%token <string> IDENT
%token TRUE FALSE LPAREN RPAREN FUN FIX THIN_ARROW
%token PLUS MINUS TIMES EQ EQEQ IN EOF
%token DEBUG EVAL HIDE STOP STEP DOLLAR_E DOLLAR_V
%token IF THEN ELSE LET REC
%right IN
%right ELSE
%right THIN_ARROW
%left EQEQ
%left PLUS MINUS
%left TIMES
%nonassoc LPAREN
%start top
%type <Syntax.Exp.t option> top
%%
top:
  EOF { None }
| exp EOF { Some $1 }
;
exp:
  INT { Exp.Int $1 }
| IDENT { Exp.Var $1 }
| TRUE { Exp.Bool true }
| FALSE { Exp.Bool false }
| FUN IDENT THIN_ARROW exp { Exp.Fun ($2, $4) }
| FIX IDENT THIN_ARROW exp { Exp.Fix ($2, $4) }
| exp PLUS exp { Exp.Add ($1, $3) }
| exp MINUS exp { Exp.Sub ($1, $3) }
| exp TIMES exp { Exp.Mul ($1, $3) }
| exp EQEQ exp { Exp.Eq ($1, $3) }
| exp LPAREN exp RPAREN { Exp.Ap ($1, $3) }
| LET IDENT EQ exp IN exp { Exp.Ap (Exp.Fun ($2, $6), $4) }
| LET REC IDENT EQ exp IN exp { Exp.Ap (Exp.Fun ($3, $7), Exp.Fix ($3, $5)) }
| IF exp THEN exp ELSE exp { Exp.If ($2, $4, $6) }
| DEBUG EVAL LPAREN pat RPAREN IN exp
    { Exp.Filter ({ pat = $4; act = Eval; gas = All }, $7) }
| DEBUG HIDE LPAREN pat RPAREN IN exp
    { Exp.Filter ({ pat = $4; act = Eval; gas = One }, $7) }
| DEBUG STOP LPAREN pat RPAREN IN exp
    { Exp.Filter ({ pat = $4; act = Pause; gas = One }, $7) }
| DEBUG STEP LPAREN pat RPAREN IN exp
    { Exp.Filter ({ pat = $4; act = Pause; gas = All }, $7) }
| LPAREN exp RPAREN { $2 }
;
pat:
  DOLLAR_E { Pat.Any }
| DOLLAR_V { Pat.Val }
| IDENT { Pat.Var $1 }
| INT { Pat.Int $1 }
| TRUE { Pat.Bool true }
| FALSE { Pat.Bool false }
| pat EQEQ pat { Pat.Eq ($1, $3) }
| pat PLUS pat { Pat.Add ($1, $3) }
| pat MINUS pat { Pat.Sub ($1, $3) }
| pat TIMES pat { Pat.Mul ($1, $3) }
| pat LPAREN pat RPAREN { Pat.Ap ($1, $3) }
| FUN IDENT THIN_ARROW pat { Pat.Fun ($2, $4) }
| FIX IDENT THIN_ARROW pat { Pat.Fix ($2, $4) }
| IF pat THEN pat ELSE pat { Pat.If ($2, $4, $6) }
| LPAREN pat RPAREN { $2 }
;
