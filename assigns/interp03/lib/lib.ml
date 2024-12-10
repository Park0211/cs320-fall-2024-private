open Utils
include My_parser

let rec free_vars ty =
  match ty with
  | TVar v -> [v]
  | TList t | TOption t -> free_vars t
  | TPair (t1, t2) | TFun (t1, t2) -> free_vars t1 @ free_vars t2
  | _ -> []

type 'a my_ref = { mutable contents: 'a }

let make_ref x = { contents = x }

let get_ref r = r.contents

let set_ref r v = r.contents <- v


let unify (ty: ty) (consts: constr list): ty_scheme option =
  let rec apply_subst subst ty =
    match ty with
    | TVar v ->
      if Env.mem v subst then Env.find v subst else ty
    | TList t -> TList (apply_subst subst t)
    | TOption t -> TOption (apply_subst subst t)
    | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)
    | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
    | _ -> ty
  in
  let rec unify_types subst constraints =
    match constraints with
    | [] -> Some subst
    | (t1, t2) :: rest ->
      let t1 = apply_subst subst t1 in
      let t2 = apply_subst subst t2 in
      if t1 = t2 then
        unify_types subst rest
      else
        match (t1, t2) with
        | (TVar v, _) ->
          if occurs_check v t2 then None
          else
            let subst = Env.add v t2 subst in
            unify_types subst rest
        | (_, TVar v) ->
          if occurs_check v t1 then None
          else
            let subst = Env.add v t1 subst in
            unify_types subst rest
        | (TFun (l1, r1), TFun (l2, r2)) ->
          unify_types subst ((l1, l2) :: (r1, r2) :: rest)
        | (TPair (l1, r1), TPair (l2, r2)) ->
          unify_types subst ((l1, l2) :: (r1, r2) :: rest)
        | (TList t1, TList t2) ->
          unify_types subst ((t1, t2) :: rest)
        | (TOption t1, TOption t2) ->
          unify_types subst ((t1, t2) :: rest)
        | _ -> None
    and occurs_check v ty =
      match ty with
      | TVar v' -> v = v'
      | TList t | TOption t -> occurs_check v t
      | TPair (t1, t2) | TFun (t1, t2) -> occurs_check v t1 || occurs_check v t2
      | _ -> false
  in
  match unify_types Env.empty consts with
  | Some subst ->
    let ty = apply_subst subst ty in
    let vars = free_vars ty in
    let forall_vars = Env.to_list subst |>  List.map fst |> List.filter (fun v -> List.mem v vars) in
    Some (Forall (forall_vars, ty))
  | None -> None

(* type_of 함수 *)
let type_of (env: stc_env) (expr: expr): ty_scheme option =
  let rec infer env expr =
    match expr with
    | Unit -> (TUnit, [])
    | True | False -> (TBool, [])
    | Int _ -> (TInt, [])
    | Float _ -> (TFloat, [])
    | Var x ->
      (match Env.find_opt x env with
      | Some ty_scheme ->
        let ty = instantiate ty_scheme in
        (ty, [])
      | None -> (TVar (gensym ()), []))
    | Fun (arg, arg_ty_opt, body) ->
      let arg_ty = match arg_ty_opt with
        | Some ty -> ty
        | None -> TVar (gensym ())
      in
      let env = Env.add arg (Forall ([], arg_ty)) env in
      let body_ty, constraints = infer env body in
      (TFun (arg_ty, body_ty), constraints)
    | App (e1, e2) ->
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let ret_ty = TVar (gensym ()) in
      (ret_ty, (t1, TFun (t2, ret_ty)) :: c1 @ c2)
    | Let { is_rec; name; value; body } ->
      let var_ty = TVar (gensym ()) in
      let env' = if is_rec then Env.add name (Forall ([], var_ty)) env else env in
      let value_ty, c1 = infer env' value in
      let scheme = generalize env value_ty in
      let env'' = Env.add name scheme env in
      let body_ty, c2 = infer env'' body in
      (body_ty, (var_ty, value_ty) :: c1 @ c2)
    | _ -> (TUnit, [])
  and instantiate (Forall (vars, ty)) =
    let subst = List.fold_left (fun acc var -> Env.add var (TVar (gensym ())) acc) Env.empty vars in
    let rec apply_subst ty =
      match ty with
      | TVar v -> if Env.mem v subst then Env.find v subst else ty
      | TList t -> TList (apply_subst t)
      | TOption t -> TOption (apply_subst t)
      | TPair (t1, t2) -> TPair (apply_subst t1, apply_subst t2)
      | TFun (t1, t2) -> TFun (apply_subst t1, apply_subst t2)
      | _ -> ty
    in
    apply_subst ty
  and generalize env ty =
    let env_vars = Env.to_list env |> List.map snd |> List.concat_map free_vars_scheme in
    let ty_vars = free_vars ty in
    let vars = List.filter (fun v -> not (List.mem v env_vars)) ty_vars in
    Forall (vars, ty)
  and free_vars_scheme (Forall (_, ty)) = free_vars ty
  in
  let ty, constraints = infer env expr in
  match unify ty constraints with
  | Some scheme -> Some scheme
  | None -> None

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let rec eval_expr (env: dyn_env) (expr: expr) : value =
  match expr with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x ->
    (match Env.find_opt x env with
    | Some v -> v
    | None -> failwith ("Unbound variable: " ^ x))
  | Fun (arg, _, body) ->
    VClos { name = None; arg; body; env }
  | App (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | VClos { name = _; arg; body; env = clos_env } ->
      let env' = Env.add arg v2 clos_env in
      eval_expr env' body
    | _ -> failwith "Attempting to apply a non-function")
  | Let { is_rec; name; value; body } ->
    if is_rec then
      let closure = make_ref VUnit in
      let rec_env = Env.add name (get_ref closure) env in
      set_ref closure (VClos { name = Some name; arg = name; body = value; env = rec_env });
      let v = eval_expr rec_env value in
      let env' = Env.add name v env in
      eval_expr env' body
    else
      let v = eval_expr env value in
      eval_expr (Env.add name v env) body
  | _ -> VUnit

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
