type tm =
  | Type
  | Prod of TT.ty * TT.ty TT.binder
  | Lambda of TT.ty * TT.tm TT.binder
  | Spine of TT.var * TT.tm list

type ty = Ty of tm

(** A normalization strategy. *)
type strategy =
  | WHNF (** normalize to weak head-normal form *)
  | CBV (** call-by-value normalization *)

open Context.Monad

(** Normalize an expression using the given strategy. *)
let rec norm_tm' ~strategy e =
  match e with

  | TT.Type ->
     return e

  | TT.Var x ->
    begin
      Context.lookup_def x >>= function
      | None -> return e
      | Some e -> norm_tm' ~strategy e
    end

  | TT.Let (e1, t, e2) ->
     let* e1 =
       match strategy with
       | WHNF -> return e1
       | CBV -> norm_tm' ~strategy e1
     in
     let (v, e2) = TT.unbind e2 in
     Context.with_var v ~def:e1 t (norm_tm' ~strategy e2)

  | TT.Prod _ ->
     return e

  | TT.Lambda _ ->
     return e

  | TT.Apply (e1, e2) ->
    let* e1 = norm_tm' ~strategy e1 in
    let* e2 =
      begin
        match strategy with
        | WHNF -> return e2
        | CBV -> norm_tm' ~strategy e2
      end
    in
    begin
      match e1 with
      | TT.Lambda (_, e') ->
        norm_tm' ~strategy (Bindlib.subst e' e2)
      | _ ->
         return @@ TT.Apply (e1, e2)
    end

(** Normalize a type *)
let norm_ty' ~strategy (TT.Ty ty) =
  let* ty = norm_tm' ~strategy ty in
  return @@ TT.Ty ty

let eval_tm = norm_tm' ~strategy:CBV

let norm_tm e =
  norm_tm' ~strategy:WHNF e >>= function
  | TT.Let _ -> assert false

  | TT.Type -> return Type

  | TT.Prod (t, u) -> return (Prod (t, u))

  | TT.Lambda (t, e) -> return (Lambda (t, e))

  | TT.(Var _ | Apply _) as e ->
     let rec fold es = function
       | TT.Var x -> x, es
       | TT.Apply (e1, e2) -> fold (e2 :: es) e1
       | TT.(Let _ | Type | Prod _ | Lambda _) -> assert false
     in
     let x, es = fold [] e in
     return @@ Spine (x, es)

let norm_ty (TT.Ty t) =
  let* t = norm_tm t in
  return @@ Ty t

(** Normalize a type to a product. *)
let as_prod t =
  let* TT.Ty t' = norm_ty' ~strategy:WHNF t in
  match t' with
  | TT.Prod (t, u) -> return @@ Some (t, u)
  | _ -> return None
