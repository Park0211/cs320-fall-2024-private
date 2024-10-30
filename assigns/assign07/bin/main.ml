open Utils
open Printf
let string_of_bop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Lt -> "Lt"
  | Lte -> "Lte"
  | Gt -> "Gt"
  | Gte -> "Gte"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | And -> "And"
  | Or -> "Or"

let rec string_of_expr = function
  | Num n -> "Num " ^ string_of_int n
  | Var v -> "Var " ^ v
  | Unit -> "Unit"
  | True -> "True"
  | False -> "False"
  | App (e1, e2) -> sprintf "App (%s,  %s)" (string_of_expr e1) (string_of_expr e2)
  | Bop (b, e1, e2) -> sprintf "Bop (%s, %s, %s)" (string_of_bop b) (string_of_expr e1) (string_of_expr e2)
  | If (e1, e2, e3) -> sprintf "If (%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Let (v, e1, e2) -> sprintf "Let (%s, %s, %s)" v (string_of_expr e1) (string_of_expr e2)
  | Fun (v, e) -> sprintf "Fun (%s, %s)" v (string_of_expr e)

let string_of_prog (p: prog) = string_of_expr p

let string_of_prog_option = function
  | None -> "None"
  | Some p -> "Some (" ^ string_of_prog p ^ ")"

let () =
  let ( let* ) = Option.bind in
  let _ =
    let* input =
      (* check that an argument is given on the command line *)
      if Array.length Sys.argv > 1
      then
        let filename = Sys.argv.(1) in
        (* read in the file *)
        try Some (Stdlib320.read_file filename) with _ ->
          print_endline
            ("Error: Could not read \'" ^ filename ^ "\'");
          None
      else (
        print_endline "Error: No file given"
        ; None
      )
    in
    let parsed = string_of_prog_option (My_parser.parse input) in
    let _ = print_endline parsed in
    Some parsed
  in ()