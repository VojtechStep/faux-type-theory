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

let print_error ?(penv=Bindlib.empty_ctxt) err ppf =
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
    Format.fprintf ppf "cannot infer the type of ?%s" x

(** [infer_ e] infers the type [ty] of expression [e]. It returns
    the processed boxed expression [e] and its boxed type [ty]. *)
let rec infer_ {Location.data=e'; loc} : TT.tm_ * TT.ty_ =
  match e' with

  | Syntax.Var x ->
     begin
       match Context.lookup_ident x with
       | None -> error ~loc (UnknownIdent x)
       | Some v ->
          let _, t = Context.lookup_var v in
          (TT.var_ v, TT.lift_ty t)
     end

  | Syntax.Let (x, e1, e2) ->
     let (e1, t1) = infer_ e1 in
     Context.with_ident_ x ~def:e1 t1
       (fun v ->
         let (e2, t2) = infer_ e2 in
         let t2 = TT.(lift_ty (Bindlib.subst (unbox (bind_var v t2)) (unbox e1))) in
         TT.(let_ e1 t1 (bind_var v e2), t2))

  | Syntax.Type ->
     TT.(type_, ty_type_)

  | Syntax.Prod ((x, u), t) ->
     let u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let t = check_ty_ t in
         TT.(prod_ u (bind_var v t), ty_type_))

  | Syntax.Lambda ((x, Some u), e) ->
     let u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let (e, t) = infer_ e in
         TT.(lambda_ u (bind_var v e), ty_prod_ u (bind_var v t)))

  | Syntax.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | Syntax.Apply (e1, e2) ->
     let (e1_, t1_) = infer_ e1 in
     let t1 = TT.unbox t1_ in
     begin
       match Norm.as_prod t1 with
       | None -> error ~loc (FunctionExpected t1)
       | Some (u, t) ->
          let e2_ = check_ e2 u in
          let e2 = Bindlib.unbox e2_ in
          TT.(apply_ e1_ e2_, TT.lift_ty (Bindlib.subst t e2))
     end

  | Syntax.Ascribe (e, t) ->
     let t = check_ty_ t in
     let e = check_ e (TT.unbox t) in
     (e, t)

  | Syntax.Hole x ->
     error ~loc (CannotInferHole x)


and check_ ({Location.data=e'; loc} as e) (ty : TT.ty) : TT.tm_ =
  match e' with

  | Syntax.Lambda ((x, None), e) ->
     begin
       match Norm.as_prod ty with
       | None -> error ~loc (TypeExpectedButFunction ty)
       | Some (t, u) ->
          Context.with_ident x t
            (fun v ->
              let u' = Bindlib.subst u (TT.Var v) in
              let e = check_ e u' in
              TT.(lambda_ (TT.lift_ty t) (bind_var v e)))
     end

  | Syntax.Let (x, e1, e2) ->
     let (e1, t1) = infer_ e1 in
     Context.with_ident_ x ~def:e1 t1
       (fun v ->
         let e2 = check_ e2 ty in
         TT.(let_ e1 t1 (bind_var v e2)))

  | Syntax.Hole x ->
     let ty_ = TT.lift_ty ty in
     Context.with_meta_ x ty_ (fun u -> u)

  (* Inferring terms *)
  | Syntax.Lambda ((_, Some _), _)
  | Syntax.Apply _
  | Syntax.Prod _
  | Syntax.Var _
  | Syntax.Type
  | Syntax.Ascribe _ ->
     begin
       let (e, ty'_) = infer_ e in
       let ty' = TT.unbox ty'_ in
       match Unify.unify_ty ty ty' with
       | true -> e
       | false -> error ~loc (TypeExpected (ty, ty'))
     end

and check_ty_ t =
  let t = check_ t TT.(Ty Type) in
  (TT.ty_ t)

(** [check e ty] checks that [e] has type [ty]. It returns the processed term [e]. *)
let check e t =
  let e = check_ e t in
  Bindlib.unbox e

(** [infer e] infers the type of [e]. It returns the processed term and type. *)
let infer e =
  let (e_, t_) = infer_ e in
  (TT.unbox e_, TT.unbox t_)

(** [check_ty ctx t] checks that [t] is a type in context [ctx]. It returns the processed
   type [t]. *)
let check_ty t =
  let t_ = check_ty_ t in
  TT.unbox t_

let rec resolve_tm = function

  | TT.(Var _ | Type) as e -> e

  | TT.Meta v ->
     begin
       match Context.lookup_meta v with
       | None, _ -> error ~loc:Location.Nowhere (CannotInferHole (Bindlib.name_of v))
       | Some e, _ -> resolve_tm e
     end

  | TT.Let (e1, t, e2) ->
     let e1 = resolve_tm e1 in
     let t = resolve_ty t in
     let e2 = resolve_binder_tm e2 in
     TT.(Let (e1, t, e2))

  | TT.Prod (t1, t2) ->
     let t1 = resolve_ty t1 in
     let t2 = resolve_binder_ty t2 in
     TT.(Prod (t1, t2))

  | TT.Apply (e1, e2) ->
     let e1 = resolve_tm e1 in
     let e2 = resolve_tm e2 in
     TT.(Apply (e1, e2))

  | TT.Lambda (t, e) ->
     let t = resolve_ty t in
     let e = resolve_binder_tm e in
     TT.(Lambda (t, e))

and resolve_ty (TT.Ty t) =
  let t = resolve_tm t in
  TT.(Ty t)

and resolve_binder_tm e =
  let (v, e) = Bindlib.unbind e in
  let e = resolve_tm e in
  let e_ = TT.lift_tm e in
  TT.(unbox (bind_var v e_))

and resolve_binder_ty t =
  let (v, t) = Bindlib.unbind t in
  let t = resolve_ty t in
  let t_ = TT.lift_ty t in
  TT.(unbox (bind_var v t_))

let rec toplevel ~quiet {Location.data=tc; _} : unit =
  toplevel' ~quiet tc

and toplevel' ~quiet : _ -> unit = function

  | Syntax.TopLoad file ->
     topfile ~quiet file

  | Syntax.TopDefinition (x, None, e) ->
     Context.handle_metas
       (fun () ->
         let (e, ty) = infer e in
         let e = resolve_tm e in
         let ty = resolve_ty ty in
         Context.top_extend x ~def:e ty ;
         if not quiet then Format.printf "%s is defined.@." x
       )

  | Syntax.TopDefinition (x, Some ty, e) ->
     Context.handle_metas
       (fun () ->
         let ty = check_ty ty in
         let e = check e ty in
         let ty = resolve_ty ty in
         let e = resolve_tm e in
         Context.top_extend x ~def:e ty ;
         if not quiet then Format.printf "%s is defined.@." x
       )

  | Syntax.TopInfer e ->
     Context.handle_metas
       (fun () ->
         let (e, ty) = infer e in
         let e = resolve_tm e in
         let ty = resolve_ty ty in
         Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
           (Print.tm e)
           (Print.ty ty))

  | Syntax.TopEval e ->
     Context.handle_metas
       (fun () ->
         let (e, ty) = infer e in
         let e = resolve_tm e in
         let ty = resolve_ty ty in
         let e = Norm.eval_tm e in
         Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
           (Print.tm e)
           (Print.ty ty))

  | Syntax.TopAxiom (x, ty) ->
     Context.handle_metas
       (fun () ->
         let ty = check_ty ty in
         let ty = resolve_ty ty in
         Context.top_extend x ty ;
         if not quiet then Format.printf "%s is assumed.@." x
       )

and topfile ~quiet file =
  let rec fold = function
    | [] -> ()
    | top_cmd :: lst ->
       toplevel ~quiet top_cmd ;
       fold lst
  in
  let cmds = Parsing.Lexer.read_file Parsing.Parser.file file in
  fold cmds
