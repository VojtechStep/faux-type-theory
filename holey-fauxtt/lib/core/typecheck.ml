(** Faux type checking. *)

module Syntax = Parsing.Syntax

module Location = Util.Location

(** Type errors *)
type type_error =
  | UnknownIdent of string
  | TypeExpected of TT.ty * TT.ty
  | TypeExpectedButFunction of TT.ty
  | FunctionExpected of TT.ty
  | CannotInferArgument of string
  | CannotInferHole of string

exception Error of type_error Location.t

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

let print_error ~penv err ppf =
  match err with

  | UnknownIdent x -> Format.fprintf ppf "unknown identifier %s" x

  | TypeExpected (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type %t but has type %t"
                        (Print.ty ~penv ty_expected)
                        (Print.ty ~penv ty_actual)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type %t"
                        (Print.ty ~penv ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type %t"
                        (Print.ty ~penv ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of %s" x

  | CannotInferHole x ->
    Format.fprintf ppf "unresolved hole ?%s" x

open Context.Monad

(** [infer_ e] infers the type [ty] of expression [e]. It returns
    the processed boxed expression [e] and its boxed type [ty]. *)
let rec infer_ {Location.data=e'; loc} : (TT.tm_ * TT.ty_) Context.m =
  match e' with

  | Syntax.Var x ->
     begin
       Context.lookup_ident x >>= function
       | None -> error ~loc (UnknownIdent x)
       | Some v ->
          let* _, t = Context.lookup_var v in
          return (TT.var_ v, TT.lift_ty t)
     end

  | Syntax.Let (x, e1, e2) ->
     let* (e1, t1) = infer_ e1 in
     Context.with_ident_ x ~def:e1 t1
       (fun v ->
         let* (e2, t2) = infer_ e2 in
         let t2 = TT.(lift_ty (Bindlib.subst (unbox (bind_var v t2)) (unbox e1))) in
         return TT.(let_ e1 t1 (bind_var v e2), t2))

  | Syntax.Type ->
     return TT.(type_, ty_type_)

  | Syntax.Prod ((x, u), t) ->
     let* u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let* t = check_ty_ t in
         return TT.(prod_ u (bind_var v t), ty_type_))

  | Syntax.Lambda ((x, Some u), e) ->
     let* u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let* (e, t) = infer_ e in
         return TT.(lambda_ u (bind_var v e), ty_prod_ u (bind_var v t)))

  | Syntax.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | Syntax.Apply (e1, e2) ->
     let* (e1_, t1_) = infer_ e1 in
     let t1 = TT.unbox t1_ in
     begin
       Norm.as_prod t1 >>= function
       | None -> error ~loc (FunctionExpected t1)
       | Some (u, t) ->
          let* e2_ = check_ e2 u in
          let e2 = Bindlib.unbox e2_ in
          return TT.(apply_ e1_ e2_, TT.lift_ty (Bindlib.subst t e2))
     end

  | Syntax.Ascribe (e, t) ->
     let* t = check_ty_ t in
     let* e = check_ e (TT.unbox t) in
     return (e, t)

  | Syntax.Hole x ->
     error ~loc (CannotInferHole x)


and check_ ({Location.data=e'; loc} as e) (ty : TT.ty) : TT.tm_ Context.m =
  match e' with

  | Syntax.Lambda ((x, None), e) ->
     begin
       Norm.as_prod ty >>= function
       | None -> error ~loc (TypeExpectedButFunction ty)
       | Some (t, u) ->
          Context.with_ident x t
            (fun v ->
              let u' = Bindlib.subst u (TT.Var v) in
              let* e = check_ e u' in
              return TT.(lambda_ (TT.lift_ty t) (bind_var v e)))
     end

  | Syntax.Let (x, e1, e2) ->
     let* (e1, t1) = infer_ e1 in
     Context.with_ident_ x ~def:e1 t1
       (fun v ->
         let* e2 = check_ e2 ty in
         return TT.(let_ e1 t1 (bind_var v e2)))

  | Syntax.Hole x ->
     let ty_ = TT.lift_ty ty in
     Context.with_meta_ x ty_ return

  (* Inferring terms *)
  | Syntax.Lambda ((_, Some _), _)
  | Syntax.Apply _
  | Syntax.Prod _
  | Syntax.Var _
  | Syntax.Type
  | Syntax.Ascribe _ ->
     begin
       let* (e, ty'_) = infer_ e in
       let ty' = TT.unbox ty'_ in
       Unify.unify_ty ty ty' >>= function
       | true -> return e
       | false -> error ~loc (TypeExpected (ty, ty'))
     end

and check_ty_ t =
  let* t = check_ t TT.(Ty Type) in
  return (TT.ty_ t)

(** [check e ty] checks that [e] has type [ty]. It returns the processed term [e]. *)
let check e t =
  let* e = check_ e t in
  return (Bindlib.unbox e)

(** [infer e] infers the type of [e]. It returns the processed term and type. *)
let infer e =
  let* (e_, t_) = infer_ e in
  return (TT.unbox e_, TT.unbox t_)

(** [check_ty ctx t] checks that [t] is a type in context [ctx]. It returns the processed
   type [t]. *)
let check_ty t =
  let* t_ = check_ty_ t in
  return (TT.unbox t_)

let rec resolve_tm = function

  | TT.(Var _ | Type) as e -> return e

  | TT.Meta v ->
     begin
       Context.lookup_meta v >>= function
       | None, _ -> error ~loc:Location.Nowhere (CannotInferHole (Bindlib.name_of v))
       | Some e, _ -> resolve_tm e
     end

  | TT.Let (e1, t, e2) ->
     let* e1 = resolve_tm e1 in
     let* t = resolve_ty t in
     let* e2 = resolve_binder_tm e2 in
     return TT.(Let (e1, t, e2))

  | TT.Prod (t1, t2) ->
     let* t1 = resolve_ty t1 in
     let* t2 = resolve_binder_ty t2 in
     return TT.(Prod (t1, t2))

  | TT.Apply (e1, e2) ->
     let* e1 = resolve_tm e1 in
     let* e2 = resolve_tm e2 in
     return TT.(Apply (e1, e2))

  | TT.Lambda (t, e) ->
     let* t = resolve_ty t in
     let* e = resolve_binder_tm e in
     return TT.(Lambda (t, e))

and resolve_ty (TT.Ty t) =
  let* t = resolve_tm t in
  return TT.(Ty t)

and resolve_binder_tm e =
  let (v, e) = Bindlib.unbind e in
  let* e = resolve_tm e in
  let e_ = TT.lift_tm e in
  return TT.(unbox (bind_var v e_))

and resolve_binder_ty t =
  let (v, t) = Bindlib.unbind t in
  let* t = resolve_ty t in
  let t_ = TT.lift_ty t in
  return TT.(unbox (bind_var v t_))

let rec toplevel ~quiet ctx {Location.data=tc; _} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function

  | Syntax.TopLoad file ->
     topfile ~quiet ctx file

  | Syntax.TopDefinition (x, None, e) ->
     let ctx, (e, ty) =
       Context.run ctx
         (let* (e, ty) = infer e in
          let* e = resolve_tm e in
          let* ty = resolve_ty ty in
          return (e, ty))
     in
     let ctx = Context.top_extend x ~def:e ty ctx in
     if not quiet then Format.printf "%s is defined.@." x ;
     ctx

  | Syntax.TopDefinition (x, Some ty, e) ->
     let ctx, (e, ty) =
       Context.run ctx
         (let* ty = check_ty ty in
          let* e = check e ty in
          let* ty = resolve_ty ty in
          let* e = resolve_tm e in
          return (e, ty))
     in
     let ctx = Context.top_extend x ~def:e ty ctx in
     if not quiet then Format.printf "%s is defined.@." x ;
     ctx

  | Syntax.TopInfer e ->
     let ctx, (e, ty) =
       Context.run ctx
         (let* (e, ty) = infer e in
          let* e = resolve_tm e in
          let* ty = resolve_ty ty in
          return (e, ty))
     in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (Print.tm ~penv:(Context.penv ctx) e)
       (Print.ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | Syntax.TopEval e ->
     let ctx, (e, ty) =
       Context.run ctx
         (let* (e, ty) = infer e in
          let* e = resolve_tm e in
          let* ty = resolve_ty ty in
          return (e, ty))
     in
     let ctx, e = Context.run ctx (Norm.eval_tm e) in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (Print.tm ~penv:(Context.penv ctx) e)
       (Print.ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | Syntax.TopAxiom (x, ty) ->
     let ctx, ty =
       Context.run ctx (let* ty = check_ty ty in resolve_ty ty) in
     let ctx = Context.top_extend x ty ctx in
     if not quiet then Format.printf "%s is assumed.@." x ;
     ctx

and topfile ~quiet ctx file =
  let rec fold ctx = function
    | [] -> ctx
    | top_cmd :: lst ->
       let ctx = toplevel ~quiet ctx top_cmd in
       fold ctx lst
  in
  let cmds = Parsing.Lexer.read_file Parsing.Parser.file file in
  fold ctx cmds
