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

type _ Effect.t +=
    | LookupVar : TT.var -> (TT.tm option * TT.ty) Effect.t
    | LookupIdent : string -> TT.var option Effect.t
    | LookupMeta : TT.var -> (TT.tm option * TT.ty) Effect.t
    | FreshMeta_ : string * TT.ty_ -> TT.tm_ Effect.t
    | SetMeta_ : TT.var * TT.tm_ -> bool Effect.t
    | TopExtend : (string * TT.tm option * TT.ty) -> unit Effect.t

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

let top_extend x ?def ty =
  perform (TopExtend (x, def, ty))

let lookup_ident x =
  perform (LookupIdent x)

let lookup_var v =
  perform (LookupVar v)

let lookup_meta v =
  perform (LookupMeta v)

let with_ident_var x v ?def t c =
  try
    c ()
  with
  | effect (LookupVar w), k when Bindlib.eq_vars v w ->
     continue k (def, t)

  | effect (LookupIdent y), k when String.equal x y ->
     continue k (Some v)

  | effect (FreshMeta_ (x, u_)), k ->
     let t_ = TT.lift_ty t in
     let u_ = TT.(ty_ (prod_ t_ (bind_var v u_))) in
     let e_ = perform (FreshMeta_ (x, u_)) in
     continue k TT.(apply_ e_ (var_ v))

  | effect (SetMeta_ (mv, e_)), k ->
     if not (Bindlib.occur v e_) then
       begin
         let b = perform (SetMeta_ (mv, e_)) in
         continue k b
       end
     else
       begin
         match def with
         | None -> continue k false
         | Some e' ->
            let e'_ = TT.lift_tm e' in
            let t_ = TT.lift_ty t in
            let b = perform (SetMeta_ (mv, TT.(let_ e'_ t_ (bind_var v e_)))) in
            continue k b
       end

let with_ident x ?def t (c : TT.var -> 'a) =
  let v = TT.fresh_var x in
  with_ident_var x v ?def t (fun () -> c v)

let with_var v ?def t (c : unit -> 'a) =
  let x = Util.Name.anonymous () in
  with_ident_var x v ?def t c

let with_ident_ x ?def ty_ (c : TT.var -> 'a) =
  let ty = TT.unbox ty_ in
  let def = Option.map TT.unbox def in
  with_ident x ?def ty c

let set_meta_ v e_ =
  perform (SetMeta_ (v, e_))

let fresh_meta_ x ty_ =
  perform (FreshMeta_ (x, ty_))

let with_meta_ x ty_ c =
  let e_ = fresh_meta_ x ty_ in
  c e_

let with_meta x ty c =
  let ty_ = TT.lift_ty ty in
  with_meta_ x ty_ (fun e_ -> c (TT.unbox e_))

(* let close_tm_ e_ = *)
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

let handle_context c =
  let ctx = ref initial in
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

exception UnresolvedMeta of TT.var

let handle_metas c =
  let metas = ref (VarMap.empty : (TT.tm option * TT.ty) VarMap.t) in
  try
    let r = c () in
    VarMap.iter (fun mv -> function
        | Some _, _ -> ()
        | None, _ -> raise (UnresolvedMeta mv))
    !metas ;
    r
  with

    | effect (FreshMeta_ (x, ty_)), k ->
       let mv = TT.fresh_var x in
       let ty = TT.unbox ty_ in
       metas := VarMap.add mv (None, ty) !metas ;
       continue k (TT.meta_ mv)

    | effect (LookupMeta v), k ->
       continue k (VarMap.find v !metas)

    | effect (SetMeta_ (v, e_)), k ->
       begin match VarMap.find v !metas with

       | None, ty ->
          let e = TT.unbox e_ in
          metas := VarMap.add v (Some e, ty) !metas ;
          continue k true

       | Some _, _ -> assert false

       end

    | UnresolvedMeta mv ->
       Util.Print.error "unresolved hole %s@" (Bindlib.name_of mv)
