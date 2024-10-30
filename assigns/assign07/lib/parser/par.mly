%{
  open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE LET IN FUN TRUE FALSE UNIT
%token PLUS MINUS TIMES DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token LPAREN RPAREN ARROW
%token EOF

%start prog
%type <Utils.prog> prog

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD

%%

prog:
  | expr EOF { $1 }

expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr { Let ($2, $4, $6) }
  | FUN VAR ARROW expr { Fun ($2, $4) }
  | expr2 { $1 }

expr2:
  | expr2 PLUS expr2 { Bop (Add, $1, $3) }
  | expr2 MINUS expr2 { Bop (Sub, $1, $3) }
  | expr2 TIMES expr2 { Bop (Mul, $1, $3) }
  | expr2 DIV expr2 { Bop (Div, $1, $3) }
  | expr2 MOD expr2 { Bop (Mod, $1, $3) }
  | expr2 LT expr2 { Bop (Lt, $1, $3) }
  | expr2 LTE expr2 { Bop (Lte, $1, $3) }
  | expr2 GT expr2 { Bop (Gt, $1, $3) }
  | expr2 GTE expr2 { Bop (Gte, $1, $3) }
  | expr2 EQ expr2 { Bop (Eq, $1, $3) }
  | expr2 NEQ expr2 { Bop (Neq, $1, $3) }
  | expr2 AND expr2 { Bop (And, $1, $3) }
  | expr2 OR expr2 { Bop (Or, $1, $3) }
  | expr3 { $1 }
  | expr3 expr3_tail { List.fold_left (fun acc e -> App (acc, e)) $1 $2 }

expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | NUM { Num $1 }
  | VAR { Var $1 }
  | LPAREN expr RPAREN { $2 }

expr3_tail:
  | expr3 expr3_tail { $1 :: $2 }
  | { [] }

bop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
