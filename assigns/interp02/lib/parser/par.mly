%{
  open Utils

  let rec mk_app e = function
    | [] -> e
    | x :: es -> mk_app (SApp (e, x)) es
%}

// constants
%token <int> NUM
%token <string> VAR

// operators
%token PLUS MINUS TIMES DIV MOD

// boolean
%token AND OR

// compare
%token LT LTE GT GTE NEQ

%token ASSERT
%token EOF
%token FUN
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET
%token REC
%token EQ
%token IN
%token IF
%token THEN
%token ELSE 
%token COLON ":"
%token TRUE
%token FALSE
%token INTTY "int"
%token BOOLTY "bool"

%token UNIT "()"
%token UNITTY "unit"

// precedence and associativity
%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%left APP

%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | LET x = VAR args = arg* ":" ty = ty EQ e = expr
    { {
        is_rec = false;
        name = x;
        args = args;
        ty = ty;
        value = e;
    } }
  | LET REC f = VAR args = arg+ ":" ty_out = ty EQ e = expr
     { {
        is_rec = true;
        name = f;
        args = args;
        ty = ty_out;
        value = e;
     } }

arg:
  | "(" x = VAR ":" ty = ty ")" { (x, ty) }

ty:
  | "int" { IntTy }
  | "bool" { BoolTy }
  | "unit" { UnitTy }
  | t1 = ty "->" t2 = ty { FunTy (t1, t2) }
  | "(" ty = ty ")" { ty }

expr:
  | LET x = VAR args = arg* ":" ty = ty EQ e1 = expr IN e2 = expr
        { SLet {
            is_rec = false;
            name = x;
            args = args;
            ty = ty;
            value = e1;
            body = e2;
        } }
  | LET REC f = VAR args = arg+ ":" ty_out = ty EQ e1 = expr IN e2 = expr
    { 
        SLet {
            is_rec = true;
            name = f;
            args = args;
            ty = ty_out;
            value = e1;
            body = e2;
        }
    }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { SIf (e1, e2, e3) }
  | FUN args = arg+ "->" e = expr {
    match args with
    | [] -> failwith "unannotated function"
    | arg :: rest -> SFun {
        arg = arg;
        args = rest;
        body = e;
    } }
  | e = expr2 { e }

expr2:
  | e1 = expr2 PLUS e2 = expr2 { SBop (Add, e1, e2) }
  | e1 = expr2 MINUS e2 = expr2 { SBop (Sub, e1, e2) }
  | e1 = expr2 TIMES e2 = expr2 { SBop (Mul, e1, e2) }
  | e1 = expr2 DIV e2 = expr2 { SBop (Div, e1, e2) }
  | e1 = expr2 MOD e2 = expr2 { SBop (Mod, e1, e2) }
  | e1 = expr2 LT e2 = expr2 { SBop (Lt, e1, e2) }
  | e1 = expr2 LTE e2 = expr2 { SBop (Lte, e1, e2) }
  | e1 = expr2 GT e2 = expr2 { SBop (Gt, e1, e2) }
  | e1 = expr2 GTE e2 = expr2 { SBop (Gte, e1, e2) }
  | e1 = expr2 EQ e2 = expr2 { SBop (Eq, e1, e2) }
  | e1 = expr2 NEQ e2 = expr2 { SBop (Neq, e1, e2) }
  | e1 = expr2 AND e2 = expr2 { SBop (And, e1, e2) }
  | e1 = expr2 OR e2 = expr2 { SBop (Or, e1, e2) }
  | ASSERT e = expr3 { SAssert e }
  | e = expr3 es = expr3* %prec APP { mk_app e es }

expr3:
  | x = VAR { SVar x }
  | n = NUM { SNum n }
  | "()" { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | "(" e = expr ")" { e }