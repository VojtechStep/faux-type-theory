(** Typing context and definitional equalities. *)

open Effect
open Effect.Deep

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
  }

type 'a m = 'a

module Monad =
struct
  let ( let* ) : 'a 'b . 'a m -> ('a -> 'b m) -> 'b m =
    fun c1 c2 -> c2 c1

  let ( >>= ) = ( let* )

  let return : 'a . 'a -> 'a m =
    fun v -> v

  (* Monadic conjunction *)
  let ( &&& ) c1 c2 =
    let* b = c1 in
    if b then c2 else return false

  (* Monadic disjunction *)
  let ( ||| ) c1 c2 =
    let* b = c1 in
    if b then return true else c2
end

type _ Effect.t +=
    | LookupVar : TT.var -> (TT.tm option * TT.ty) Effect.t
    | LookupIdent : string -> TT.var option Effect.t
    | LookupMeta : TT.var -> (TT.tm option * TT.ty) Effect.t
    | TopExtend : (string * TT.tm option * TT.ty) -> unit Effect.t
    (* | Define : TT.var * TT.tm -> bool Effect.t *)

(** The initial, empty typing context. *)
let initial =
  { idents = IdentMap.empty
  ; vars = VarMap.empty
  }

let penv _ = Bindlib.empty_ctxt

let _extend_var x v def ty ctx =
  { idents = (match x with None -> ctx.idents | Some x -> IdentMap.add x v ctx.idents)
  ; vars = VarMap.add v (def, ty) ctx.vars
  }

(* let _extend_meta v ty ctx = *)
(*   { ctx with metas = VarMap.add v (None, ty) ctx.metas } *)

let top_extend x ?def ty =
  perform (TopExtend (x, def, ty))

(* let extend x ?def ty ctx = *)
(*   let v = TT.fresh_var x in *)
(*   _extend_var (Some x) v def ty ctx, v *)

let lookup_ident x =
  perform (LookupIdent x)

let lookup_var v =
  perform (LookupVar v)

let lookup_meta v =
  perform (LookupMeta v)

let with_var v ?def t (c : unit -> 'a m) =
  try
    c ()
  with
  | effect (LookupVar w), k when Bindlib.eq_vars v w ->
     continue k (def, t)

let with_ident x ?def t (c : TT.var -> 'a m) =
  let v = TT.fresh_var x in
  try
    c v
  with
  | effect (LookupVar w), k when Bindlib.eq_vars v w ->
     continue k (def, t)
  | effect (LookupIdent y), k when String.equal x y ->
     continue k (Some v)

let with_ident_ x ?def ty_ (c : TT.var -> 'a m) =
  let ty = TT.unbox ty_ in
  let def = Option.map TT.unbox def in
  with_ident x ?def ty c

let run c = c

(* let define v e ctx = *)
(*   match VarMap.find v ctx.metas with *)

(*   | Some _, _ -> *)
(*     (\* We need proper error reporting. *\) *)
(*     assert false *)

(*   | None, ty -> *)
(*      let ctx = { ctx with metas = VarMap.add v (Some e, ty) ctx.metas } in *)
(*      ctx, () *)

let with_meta_ _x _t_ _c =
  failwith "with_meta_ not implemented"
(* let with_meta_ x t_ c ctx = *)
(*   let mv = TT.fresh_var x in *)
(*   let rec fold t_ = function *)
(*     | [] -> TT.meta_ mv, t_ *)
(*     | v :: vs -> *)
(*        begin match VarMap.find v ctx.vars with *)
(*        | Some _, _ -> *)
(*           (\* let e_ = TT.(lift_tm (Bindlib.subst (unbox (bind_var v e_)) e')) in *\) *)
(*           (\* let t_ = TT.(lift_ty (Bindlib.subst (unbox (bind_var v t_)) e')) in *\) *)
(*           fold t_ vs *)
(*        | None, u -> *)
(*           let u_ = TT.lift_ty u in *)
(*           let t_ = TT.(ty_ (prod_ u_ (bind_var v t_))) in *)
(*           let e_, t_ = fold t_ vs in *)
(*           TT.(apply_ e_ (var_ v)), t_ *)
(*        end *)
(*   in *)
(*   let e_, t_ = fold t_ ctx.locals in *)
(*   let t = TT.unbox t_ in *)
(*   let ctx = { ctx with metas = VarMap.add mv (None, t) ctx.metas } in *)
(*   c e_ ctx *)

let with_meta _x _ty _c =
  failwith "with_meta not implemented"
(* let with_meta x ty c = *)
(*   let ty_ = TT.lift_ty ty in *)
(*   with_meta_ x ty_ (fun e_ -> c (TT.unbox e_)) *)

let close_tm_ _e_ =
  failwith "close_tm_ not implemented"
(* let close_tm_ e_ ctx = *)
(*   let rec fold : TT.var list -> TT.tm_ option = function *)
(*     | [] -> Some e_ *)
(*     | v :: vs -> *)
(*        begin match VarMap.find v ctx.vars with *)
(*        | Some e', t -> *)
(*           if Bindlib.occur v e_ then *)
(*             begin match fold vs with *)
(*             | None -> None *)
(*             | Some e_ -> *)
(*                let e'_ = TT.lift_tm e' in *)
(*                let t_ = TT.lift_ty t in *)
(*                Some TT.(let_ e'_ t_ (bind_var v e_)) *)
(*             end *)
(*           else *)
(*             fold vs *)
(*        | None, _ -> *)
(*           (\* Check that the variable does not appear in e_ *\) *)
(*           if Bindlib.occur v e_ then *)
(*             None *)
(*           else *)
(*             fold vs *)
(*        end *)
(*   in *)
(*   ctx, fold (List.rev ctx.locals) *)

let with_context ctx c =
  let ctx = ref ctx in
  try
    c ()
  with
    | effect (LookupVar v), k ->
       continue k (VarMap.find v !ctx.vars)

    | effect (LookupIdent x), k ->
       continue k (IdentMap.find_opt x !ctx.idents)

   | effect (TopExtend (x, def, ty)), k ->
      let v = TT.fresh_var x in
      ctx := { idents = IdentMap.add x v !ctx.idents ; vars = VarMap.add v (def, ty) !ctx.vars } ;
      continue k ()
