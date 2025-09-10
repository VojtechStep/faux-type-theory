(** Unify terms at a type. *)
val unify_tm_at : TT.tm -> TT.tm -> TT.ty -> bool Context.m

(** Unifty types *)
val unify_ty : TT.ty -> TT.ty -> bool Context.m
