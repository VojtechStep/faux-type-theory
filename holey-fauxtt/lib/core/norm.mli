(** Weak-head normal term. *)
type head =
  | Var of TT.var
  | Meta of TT.var

type tm =
  | Type
  | Prod of TT.ty * TT.ty TT.binder
  | Lambda of TT.ty * TT.tm TT.binder
  | Spine of head * TT.tm list

type ty = Ty of tm

(** Evaluate a term using the call-by-value strategy *)
val eval_tm : TT.tm -> TT.tm Context.m

(** Normalize a term *)
val norm_tm : TT.tm-> tm Context.m

(** Normalize a type *)
val norm_ty : TT.ty -> ty Context.m

(** Convert a type to a product *)
val as_prod : TT.ty -> (TT.ty * TT.ty TT.binder) option Context.m

(** Convert a term to a variable *)
val as_var : TT.tm -> TT.var option Context.m
