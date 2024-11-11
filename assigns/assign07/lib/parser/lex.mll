(* {
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
  | _ { failwith "Unexpected character" } *)


{
open Par
exception Eof 
}

rule read = parse 
  | [' ' '\t' '\r' '\n'] { read lexbuf } 
  |"if" { IF }
  |"then" { THEN }
  |"else" { ELSE }
  |"let" { LET }
  |"in" { IN }
  |"fun" { FUN }
  |"true" { TRUE }
  |"false" { FALSE }
  |"()" { UNIT }
  |"mod" { MOD }
  |['0'- '9'] + as lxm { NUM (int_of_string lxm) }   (* 하나 이상의 이전문자  12  1 4  int literal 로 해쥬고 num 이라는 토큰으로 바꿔줌 *)
  |['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { VAR lxm } (*  *가 나오면 올수도 있고 안 올수도 있지만 이런 패턴이 나와야함 *)
  |"+" { PLUS }
  |"-" { MINUS }
  |"*" { TIMES }
  |"/" { DIV }
  |"<" { LT }
  |"<=" { LTE }
  |">" { GT }
  |">=" { GTE }
  |"=" { EQ }
  |"<>" { NEQ }
  |"&&" { AND }
  |"||" { OR }
  |"(" { LPAREN }
  |")" { RPAREN }
  |"->" { ARROW }
  | eof { EOF }
  |_ { failwith "Unexpected character" }