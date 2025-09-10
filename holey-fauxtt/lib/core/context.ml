(** Typing context and definitional equalities. *)

module IdentMap = Map.Make(struct
                    type t = string
                    let compare = String.compare
                  end)

module VarMap = Map.Make(struct
                    type t = TT.var
                    let compare = Bindlib.compare_vars
                  end)

(** A typing context comprises two maps, the first one mapping strings to [Bindlib] variables,
    and the second mapping variables to their types and optional definitions. *)
type t =
  { idents : TT.var IdentMap.t
  ; vars : (TT.tm option * TT.ty) VarMap.t
  ; locals : TT.var list
  ; metas : (TT.tm option * TT.ty) VarMap.t
  }

type 'a m = t -> t * 'a

module Monad =
struct
  let ( let* ) : 'a 'b . 'a m -> ('a -> 'b m) -> 'b m =
    fun c1 c2 (ctx : t) ->
    let ctx, v1 = c1 ctx in
    c2 v1 ctx

  let ( >>= ) = ( let* )

  let return : 'a . 'a -> 'a m =
    fun v t -> (t, v)

  (* Monadic conjunction *)
  let ( &&& ) c1 c2 =
    let* b = c1 in
    if b then c2 else return false

  (* Monadic disjunction *)
  let ( ||| ) c1 c2 =
    let* b = c1 in
    if b then return true else c2
end

(** The initial, empty typing context. *)
let initial =
  { idents = IdentMap.empty
  ; vars = VarMap.empty
  ; locals = []
  ; metas = VarMap.empty
  }

let run ctx c = c ctx

let penv _ = Bindlib.empty_ctxt

let _extend_var x v def ty ctx =
  { ctx with
    idents = (match x with None -> ctx.idents | Some x -> IdentMap.add x v ctx.idents)
  ; locals = v :: ctx.locals
  ; vars = VarMap.add v (def, ty) ctx.vars
  }

let _extend_meta v ty ctx =
  { ctx with metas = VarMap.add v (None, ty) ctx.metas }

let top_extend x ?def ty ctx =
  let v = TT.fresh_var x in
  { ctx with
    idents = IdentMap.add x v ctx.idents
  ; vars = VarMap.add v (def, ty) ctx.vars
  }

let extend x ?def ty ctx =
  let v = TT.fresh_var x in
  _extend_var (Some x) v def ty ctx, v

let lookup_ident x ctx =
  ctx, IdentMap.find_opt x ctx.idents

let lookup_var v ctx =
  ctx, VarMap.find v ctx.vars

let lookup_meta v ctx =
  ctx, VarMap.find v ctx.metas

let with_var v ?def t (c : 'a m) ctx =
  let local_ctx = _extend_var None v def t ctx in
  let {metas;_}, r = c local_ctx in
  { ctx with metas }, r

let with_ident x ?def ty (c : TT.var -> 'a m) ctx =
  let local_ctx, v = extend x ?def ty ctx in
  let {metas;_}, r = c v local_ctx in
  { ctx with metas }, r

let with_ident_ x ?def ty_ (c : TT.var -> 'a m) =
  let ty = TT.unbox ty_ in
  let def = Option.map TT.unbox def in
  with_ident x ?def ty c

let define v e ctx =
  match VarMap.find v ctx.metas with

  | Some _, _ ->
    (* We need proper error reporting. *)
    assert false

  | None, ty ->
     let ctx = { ctx with metas = VarMap.add v (Some e, ty) ctx.metas } in
     ctx, ()

let with_meta_ x t_ c ctx =
  let mv = TT.fresh_var x in
  let rec fold t_ = function
    | [] -> TT.meta_ mv, t_
    | v :: vs ->
       begin match VarMap.find v ctx.vars with
       | Some _, _ ->
          (* let e_ = TT.(lift_tm (Bindlib.subst (unbox (bind_var v e_)) e')) in *)
          (* let t_ = TT.(lift_ty (Bindlib.subst (unbox (bind_var v t_)) e')) in *)
          fold t_ vs
       | None, u ->
          let u_ = TT.lift_ty u in
          let t_ = TT.(ty_ (prod_ u_ (bind_var v t_))) in
          let e_, t_ = fold t_ vs in
          TT.(apply_ e_ (var_ v)), t_
       end
  in
  let e_, t_ = fold t_ ctx.locals in
  let t = TT.unbox t_ in
  let ctx = { ctx with metas = VarMap.add mv (None, t) ctx.metas } in
  c e_ ctx

let with_meta x ty c =
  let ty_ = TT.lift_ty ty in
  with_meta_ x ty_ (fun e_ -> c (TT.unbox e_))

let close_tm_ e_ ctx =
  let rec fold : TT.var list -> TT.tm_ option = function
    | [] -> Some e_
    | v :: vs ->
       begin match VarMap.find v ctx.vars with
       | Some e', t ->
          if Bindlib.occur v e_ then
            begin match fold vs with
            | None -> None
            | Some e_ ->
               let e'_ = TT.lift_tm e' in
               let t_ = TT.lift_ty t in
               Some TT.(let_ e'_ t_ (bind_var v e_))
            end
          else
            fold vs
       | None, _ ->
          (* Check that the variable does not appear in e_ *)
          if Bindlib.occur v e_ then
            None
          else
            fold vs
       end
  in
  ctx, fold (List.rev ctx.locals)
