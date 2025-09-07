(** Equality and normalization. *)

open Context.Monad

(* Monadic conjunction *)
let ( &&& ) c1 c2 =
  let* b = c1 in
  if b then c2 else return false

(* Monadic disjunction *)
let ( ||| ) c1 c2 =
  let* b = c1 in
  if b then return true else c2

(** Compare expressions [e1] and [e2] at type [ty]? *)
let rec equal_tm_at e1 e2 ty =
  (* short-circuit *)
  return (e1 == e2) |||
  begin
    (* The type directed phase *)
    let* Norm.Ty ty' = Norm.norm_ty ty in
    match  ty' with

    | Norm.Prod (t, u) ->
       (* Apply function extensionality. *)
       let (x, u) = TT.unbind u in
       Context.with_var x t
         (let e1 = TT.(Apply (e1, Var x))
          and e2 = TT.(Apply (e2, Var x)) in
          equal_tm_at e1 e2 u)

    | Norm.(Spine _ | Type) ->
       (* Type-directed phase is done, we compare normal forms. *)
       equal_tm e1 e2

    | Norm.(Lambda _) ->
      (* A type should never normalize to an abstraction or a let-binding *)
      assert false
  end

(** Structurally compare weak head-normal forms of terms [e1] and [e2]. *)
and equal_tm e1 e2 =
  let* e1 = Norm.norm_tm e1 in
  let* e2 = Norm.norm_tm e2 in
  match e1, e2 with

  | Norm.Type, Norm.Type ->
     return true

  | Norm.Prod (t1, u1), Norm.Prod (t2, u2)  ->
    equal_ty t1 t2 &&&
    begin
      let (x, u1, u2) = Bindlib.unbind2 u1 u2 in
      Context.with_var x t1 (equal_ty u1 u2)
    end

  | Norm.Lambda _, Norm.Lambda _  ->
    (* We should never have to compare two lambdas, as that would mean that the
       type-directed phase did not figure out that these have product types. *)
    assert false

  | Norm.Spine (x1, es1), Norm.Spine (x2, es2) ->
     equal_spine x1 es1 x2 es2

  | Norm.(Type | Prod _ | Lambda _ | Spine _), _ ->
    return false

and equal_spine x1 es1 x2 es2 =
  let rec fold t es1 es2 =
    match es1, es2 with
    | [], [] -> return true

    | ([], _::_) | (_::_, []) -> return false

    | e1 :: es1, e2 :: es2 ->
       begin
         Norm.as_prod t >>= function
         | None -> return false
         | Some (t, u) -> (equal_tm_at e1 e2 t) &&& (fold (Bindlib.subst u e1) es1 es2)
       end
  in

  (return @@ Bindlib.eq_vars x1 x2) &&&
  begin
    let* (_, t) = Context.lookup_var x1 in
    fold t es1 es2
  end

(** Compare two types. *)
and equal_ty (TT.Ty ty1) (TT.Ty ty2) =
  equal_tm_at ty1 ty2 TT.(Ty Type)
