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
type entry =
  | Free
  | Defined of TT.tm
  | Meta of (TT.tm -> bool)

type t =
  { idents : TT.var IdentMap.t
  ; vars : (entry * TT.ty) VarMap.t
  }

type 'a m = t -> t * 'a

module Monad =
struct
  let ( let* ) : 'a 'b . 'a m -> ('a -> 'b m) -> 'b m =
    fun c1 c2 ctx ->
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
  }

let run ctx c = c ctx

let penv _ = Bindlib.empty_ctxt

let _extend_ident_var x v ent ty {idents; vars} =
  { idents = IdentMap.add x v idents
  ; vars = VarMap.add v (ent, ty) vars
  }

let _extend_var v ent ty ctx =
  { ctx with vars = VarMap.add v (ent, ty) ctx.vars }

let extend x ?def ty ctx =
  let v = TT.fresh_var x in
  let ent =
    match def with
    | None -> Free
    | Some e -> Defined e
  in
  _extend_ident_var x v ent ty ctx, v

let lookup_ident x ctx =
  ctx, IdentMap.find_opt x ctx.idents

let lookup v {vars; _} = VarMap.find v vars

let lookup_ty v ctx = ctx, snd (lookup v ctx)

let lookup_def v ctx =
  match fst (lookup v ctx) with
  | Free | Meta _ -> ctx, None
  | Defined e -> ctx, Some e

let with_var v ?def t (c : 'a m) ctx =
  let ent = match def with None -> Free | Some e -> Defined e in
  let local_ctx = _extend_var v ent t ctx in
  c local_ctx

let with_ident_ x ?def ty_ (c : TT.var -> 'a m) ctx =
  let v = TT.fresh_var x in
  let ent =
    match def with
    | None -> Free
    | Some e_ -> Defined (TT.unbox e_)
  in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v ent ty ctx in
  c v local_ctx

let with_meta_ x ty_ ~chk c ctx =
  let v = TT.fresh_var x in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v (Meta chk) ty ctx in
  c v local_ctx

let with_ident x ?def ty (c : TT.var -> 'a m) ctx =
  let local_ctx, v = extend x ?def ty ctx in
  c v local_ctx

let define v def ctx =
  match VarMap.find v ctx.vars with

  | (Free | Defined _), _ ->
    (* We need proper error reporting. *)
    assert false

  | Meta chk, ty ->
    if chk def then
      let ctx = { ctx with vars = VarMap.add v (Defined def, ty) ctx.vars } in
      ctx, true
    else
      ctx, false
