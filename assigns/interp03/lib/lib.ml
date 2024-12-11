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


let rec apply_subst (subst : ty env) (ty : ty) : ty =
  match ty with
  | TVar v ->
      (try Env.find v subst with Not_found -> ty)
  | TList t ->
      TList (apply_subst subst t)
  | TOption t ->
      TOption (apply_subst subst t)
  | TPair (t1, t2) ->
      TPair (apply_subst subst t1, apply_subst subst t2)
  | TFun (t1, t2) ->
      TFun (apply_subst subst t1, apply_subst subst t2)
  | _ -> ty

let rec occurs_in (v : string) (ty : ty) : bool =
  match ty with
  | TVar v' -> v = v'
  | TList t -> occurs_in v t
  | TOption t -> occurs_in v t
  | TPair (t1, t2) | TFun (t1, t2) ->
      occurs_in v t1 || occurs_in v t2
  | _ -> false

let unify (ty : ty) (consts : constr list) : ty_scheme option =
  let rec unify_types (subst : ty env) (constraints : constr list) : ty env option =
    match constraints with
    | [] -> Some subst
    | (t1, t2) :: rest ->
        let t1 = apply_subst subst t1 in
        let t2 = apply_subst subst t2 in
        if t1 = t2 then
          unify_types subst rest
        else
          (match (t1, t2) with
          | (TVar v, _) ->
              if occurs_in v t2 then None
              else
                let subst' = Env.add v t2 subst in
                unify_types subst' rest
          | (_, TVar v) ->
              if occurs_in v t1 then None
              else
                let subst' = Env.add v t1 subst in
                unify_types subst' rest
          | (TFun (l1, r1), TFun (l2, r2)) ->
              unify_types subst ((l1, l2) :: (r1, r2) :: rest)
          | (TPair (a1, b1), TPair (a2, b2)) ->
              unify_types subst ((a1, a2) :: (b1, b2) :: rest)
          | (TList a, TList b) ->
              unify_types subst ((a, b) :: rest)
          | (TOption a, TOption b) ->
              unify_types subst ((a, b) :: rest)
          | _ -> None)
  in
  match unify_types Env.empty consts with
  | Some subst ->
      let ty = apply_subst subst ty in
      let free_ty_vars = free_vars ty in
      let substituted_vars = Env.to_list subst |> List.map fst in
      let generalized_vars = List.filter (fun v -> List.mem v free_ty_vars) substituted_vars in
      Some (Forall (generalized_vars, ty))
  | None -> None

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
      let env'' = Env.add name scheme env' in
      let body_ty, c2 = infer env'' body in
      (body_ty, (var_ty, value_ty) :: c1 @ c2)
    | Assert e ->
      let ty, constraints = infer env e in
      (TUnit, (ty, TBool) :: constraints)
    | ESome e ->
      let ty, constraints = infer env e in
      (TOption ty, constraints)
    | Bop (op, e1, e2) ->
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let ret_ty, constraints = match op with
        | Add | Sub | Mul | Div | Mod -> (TInt, [(t1, TInt); (t2, TInt)])
        | AddF | SubF | MulF | DivF | PowF -> (TFloat, [(t1, TFloat); (t2, TFloat)])
        | Cons -> (TList t1, [(t2, TList t1)])
        | Concat -> (t1, [(t1, t2)])
        | Lt | Lte | Gt | Gte | Neq | Eq -> (TBool, [(t1, t2)])
        | And | Or -> (TBool, [(t1, TBool); (t2, TBool)])
        | Comma -> (TPair (t1, t2), [])
      in
      (ret_ty, constraints @ c1 @ c2)
    | If (e1, e2, e3) ->
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let t3, c3 = infer env e3 in
      (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let matched_ty, c1 = infer env matched in
      let alpha = TVar (gensym ()) in
      let env' = Env.add hd_name (Forall ([], alpha)) (Env.add tl_name (Forall ([], TList alpha)) env) in
      let cons_case_ty, c2 = infer env' cons_case in
      let nil_case_ty, c3 = infer env nil_case in
      (nil_case_ty, (matched_ty, TList alpha) :: (cons_case_ty, nil_case_ty) :: c1 @ c2 @ c3)
    | OptMatch { matched; some_name; some_case; none_case } ->
      let matched_ty, c1 = infer env matched in
      let alpha = TVar (gensym ()) in
      let env' = Env.add some_name (Forall ([], alpha)) env in
      let some_case_ty, c2 = infer env' some_case in
      let none_case_ty, c3 = infer env none_case in
      (none_case_ty, (matched_ty, TOption alpha) :: (some_case_ty, none_case_ty) :: c1 @ c2 @ c3)
    | PairMatch { matched; fst_name; snd_name; case } ->
        let matched_ty, c1 = infer env matched in
        let case_ty, c2 = infer (Env.add fst_name (Forall ([], TVar (gensym ()))) (Env.add snd_name (Forall ([], TVar (gensym ()))) env)) case in
        (case_ty, (matched_ty, TPair (TVar (gensym ()), TVar (gensym ()))) :: c1 @ c2)
    | Annot (e, ty) ->
      let inferred_ty, constraints = infer env e in
      (ty, (inferred_ty, ty) :: constraints)
    | Nil -> (TList (TVar (gensym ())), [])
    | ENone -> (TOption (TVar (gensym ())), [])
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
