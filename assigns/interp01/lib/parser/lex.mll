{
  open Par
  exception Eof
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = ['-']? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace { read lexbuf }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "()" { UNIT }
  | num as n { NUM (int_of_string n) }
  | var as v { VAR v }
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
  | eof { EOF }
  | _ { failwith "Unknown token" }
