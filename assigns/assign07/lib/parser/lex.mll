{
  open Par
  exception Eof
}

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }  (* 공백 문자 무시 *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "()" { UNIT }
  | "mod" { MOD }
  | ['0'-'9']+ as lxm { NUM (int_of_string lxm) }
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { VAR lxm }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | eof { EOF }
  | _ { failwith "Unexpected character" }
