(** Equality and normalization. *)

open Context.Monad

(** Unify expressions [e1] and [e2] at type [ty]? *)
let rec unify_tm_at e1 e2 ty =
  (* short-circuit *)
  return (e1 == e2) |||
  begin
    (* The type directed phase *)
    let* (Norm.Ty ty') = Norm.norm_ty ty in
    match  ty' with

    | Norm.Prod (t, u) ->
       (* Apply function extensionality. *)
       let (x, u) = TT.unbind u in
       Context.with_var x t
         (fun () ->
           let e1 = TT.(Apply (e1, Var x))
           and e2 = TT.(Apply (e2, Var x)) in
           unify_tm_at e1 e2 u)

    | Norm.(Type | Spine _) ->
       (* Type-directed phase is done, we compare normal forms. *)
       unify_tm e1 e2

    | Norm.Lambda _ ->
      (* A type should never normalize to an abstraction or a let-binding *)
      assert false
  end

(** Structurally unify weak head-normal forms of terms [e1] and [e2]. *)
and unify_tm e1 e2 : bool Context.m =
  let* e1' = Norm.norm_tm e1 in
  let* e2' = Norm.norm_tm e2 in
  match e1', e2' with

  | Norm.Type, Norm.Type ->
     return true

  | Norm.Prod (t1, u1), Norm.Prod (t2, u2)  ->
    unify_ty t1 t2 &&&
    begin
      let (x, u1, u2) = Bindlib.unbind2 u1 u2 in
      Context.with_var x t1 (fun () -> unify_ty u1 u2)
    end

  | Norm.Spine (Var x1, es1), Norm.Spine (Var x2, es2) when Bindlib.eq_vars x1 x2 ->
     let* _, t = Context.lookup_var x1 in
     unify_spine t es1 es2

  | Norm.Spine (Meta x1, es1), Norm.Spine (Meta x2, es2) ->
     if Bindlib.eq_vars x1 x2 then
       let* _, t = Context.lookup_meta x1 in
       unify_spine t es1 es2
     else
       (unify_meta x1 es1 e2) ||| (unify_meta x2 es2 e1)

  | Norm.Spine (Meta x1, es1), Norm.(Type | Prod _ | Spine (Var _, _)) ->
     unify_meta x1 es1 e2

  | Norm.(Type | Prod _ | Spine (Var _, _)), Norm.Spine (Meta x2, es2) ->
     unify_meta x2 es2 e1

  | Norm.Lambda _, _ | _, Norm.Lambda _  ->
    (* We should never have to compare two lambdas, as that would mean that the
       type-directed phase did not figure out that these have product types. *)
    assert false

  | Norm.(Type | Prod _ | Spine _), Norm.(Type | Prod _ | Spine _) ->
    return false

and unify_ty (TT.Ty ty1) (TT.Ty ty2) =
  unify_tm_at ty1 ty2 TT.(Ty Type)

and unify_spine t es1 es2 =
  let rec fold t es1 es2 =
    match es1, es2 with

    | ([], _::_) | (_::_, []) -> return false

    | [], [] -> return true

    | e1 :: es1, e2 :: es2 ->
       Norm.as_prod t >>= function
       | None -> return false
       | Some (t, u) ->
          begin
            unify_tm_at e1 e2 t >>= function
            | false -> return false
            | true -> fold (Bindlib.subst u e1) es1 es2
          end
  in
  fold t es1 es2

and unify_meta _mv _es _e' =
  failwith "unify_meta not implemented"
  (* let rec abstract t ys = function *)
  (*   | [] -> return @@ Some (TT.lift_tm e') *)
  (*   | e :: es -> *)
  (*     begin *)
  (*       Norm.as_prod t >>= function *)
  (*         | None -> assert false *)
  (*         | Some (u, t) -> *)
  (*           Norm.as_var e >>= function *)
  (*           | None -> return None *)
  (*           | Some y -> *)
  (*             if List.exists (Bindlib.eq_vars y) ys then *)
  (*               return None *)
  (*             else begin *)
  (*               abstract (Bindlib.subst t (TT.Var y)) (y :: ys) es >>= function *)
  (*               | None -> return None *)
  (*               | Some e' -> *)
  (*                 let e' = TT.lambda_ (TT.lift_ty u) (Bindlib.bind_var y e') in *)
  (*                 return (Some e') *)
  (*             end *)

  (*     end *)
  (* in *)
  (* let* _, t = Context.lookup_meta mv in *)
  (* abstract t [] es >>= function *)
  (* | None -> return false *)
  (* | Some e_ -> *)
  (*    begin *)
  (*      Context.close_tm_ e_ >>= function *)
  (*      | None -> return false *)
  (*      | Some e_ -> *)
  (*         let e = TT.unbox e_ in *)
  (*         let* _ = Context.define mv e in *)
  (*         return true *)
  (*    end *)
