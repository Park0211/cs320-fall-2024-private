open Utils

let lex (str: string): tok list option =
  let syntax_lst = split str in

  let rec lex_helper (lst: string list) (acc: tok list) =
    match lst with
      | [] -> Some acc
      | h :: t -> (
        match tok_of_string_opt h with
          | Some token -> lex_helper t (acc @ [token])
          | None -> None
      )
  in
  lex_helper syntax_lst [] 

