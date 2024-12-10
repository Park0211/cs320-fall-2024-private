{
  open Par
  exception Eof
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = ['-']? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | ":" { COLON }
  | "int" { INTTY }
  | "bool" { BOOLTY }
  | "unit" { UNITTY }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "rec" { REC }
  | "()" { UNIT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "mod" { MOD }
  | "<=" { LTE }
  | "<" { LT }
  | ">=" { GTE }
  | ">" { GT }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "assert" { ASSERT }
  | num as n { NUM (int_of_string n) }
  | var as v { VAR v }
  | whitespace { read lexbuf }
  | eof { EOF }
  | _ { failwith "Unknown token" }