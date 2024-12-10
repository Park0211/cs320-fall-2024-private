open Utils

let parse = My_parser.parse;;

let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Num _ | True | False | Unit -> e
  | Var y -> if x = y then value_to_expr v else e
  | If (e1, e2, e3) ->
      If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      if x = y then
        Let (y, subst v x e1, e2)
      else
        Let (y, subst v x e1, subst v x e2)
  | Fun (y, e1) ->
      if x = y then
        Fun (y, e1)
      else
        Fun (y, subst v x e1)
  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)
  | Bop (b, e1, e2) ->
      Bop (b, subst v x e1, subst v x e2)

and value_to_expr (v : value) : expr =
  match v with
  | VNum n -> Num n
  | VBool b -> if b then True else False
  | VUnit -> Unit
  | VFun (y, e) -> Fun (y, e)
;;

let rec eval (e : expr) : (value, error) result =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var s -> Error (UnknownVar s)
  | If (e1, e2, e3) ->
      (match eval e1 with
       | Ok (VBool b) ->
           if b then eval e2 else eval e3
       | Ok _ -> Error InvalidIfCond
       | Error err -> Error err)
  | Let (x, e1, e2) ->
      (match eval e1 with
       | Ok v1 -> eval (subst v1 x e2)
       | Error err -> Error err)
  | Fun (x, e) -> Ok (VFun (x, e))
  | App (e1, e2) ->
      (match eval e1 with
       | Ok (VFun (x, e)) ->
           (match eval e2 with
            | Ok v2 -> eval (subst v2 x e)
            | Error err -> Error err)
       | Ok _ -> Error InvalidApp
       | Error err -> Error err)
  | Bop (b, e1, e2) ->
      (match eval e1 with
       | Ok v1 ->
           (match eval e2 with
            | Ok v2 ->eval_bop b v1 v2
            | Error err -> Error err)
       | Error err -> Error err)

and eval_bop b v1 v2 =
  match (b, v1, v2) with
  | (Add, VNum n1, VNum n2) -> Ok (VNum (n1 + n2))
  | (Sub, VNum n1, VNum n2) -> Ok (VNum (n1 - n2))
  | (Mul, VNum n1, VNum n2) -> Ok (VNum (n1 * n2))
  | (Div, VNum n1, VNum n2) ->
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | (Mod, VNum n1, VNum n2) ->
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | (Lt, VNum n1, VNum n2) -> Ok (VBool (n1 < n2))
  | (Lte, VNum n1, VNum n2) -> Ok (VBool (n1 <= n2))
  | (Gt, VNum n1, VNum n2) -> Ok (VBool (n1 > n2))
  | (Gte, VNum n1, VNum n2) -> Ok (VBool (n1 >= n2))
  | (Eq, VNum n1, VNum n2) -> Ok (VBool (n1 = n2))
  | (Neq, VNum n1, VNum n2) -> Ok (VBool (n1 <> n2))
  | (And, VBool b1, VBool b2) -> Ok (VBool (b1 && b2))
  | (Or, VBool b1, VBool b2) -> Ok (VBool (b1 || b2))
  | x, _, _ -> Error (InvalidArgs x)

let interp (s : string) : (value, error) result =
  match parse s with
  | Some prog -> eval prog
  | None -> Error ParseFail
;;
