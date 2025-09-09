type t

(* The monad for computing in a typing context *)
type 'a m

(** Monadic interface to contexts. *)
module Monad : sig

  (** Thread context state through a computation *)
  val ( let* ) : 'b m -> ('b -> 'c m) -> 'c m

  (** Synonym for [let*] *)
  val ( >>= ) : 'b m -> ('b -> 'c m) -> 'c m

  (** Return a pure value *)
  val return : 'b -> 'b m

  (** Monadic conjunction *)
  val (&&&) : bool m -> bool m -> bool m

  (** Monadic disjunction *)
  val (|||) : bool m -> bool m -> bool m

end

(** The initial, empty typing context. *)
val initial : t

(** Run a computation in the given context. *)
val run : t -> 'a m -> t * 'a

(** Assign a value to a meta-variable and report whether the assignment
   succeeded. It is an error to attempt to assign a variable which
   is not a meta-variable. *)
val define : TT.var -> TT.tm -> bool m

(** Extend the context with a variable and return it *)
val extend : string -> ?def:TT.tm -> TT.ty -> t -> t * TT.var

(** The identifiers which should not be used for printing bound variables. *)
val penv : t -> Bindlib.ctxt

(** Lookup the type of a variable *)
val lookup_ty : TT.var -> TT.ty m

(** Lookup the definition associated with a variable, if any. *)
val lookup_def : TT.var -> TT.tm option m

(** Lookup the variable which corresponds to a concrete name. *)
val lookup_ident : string -> TT.var option m

(** Run a computation in a context extended with a variable, passing it the newly
    created variable. It is the callers responsibility that the result be valid in
    the original context. *)

val with_ident : string -> ?def:TT.tm -> TT.ty -> (TT.var -> 'a m) -> 'a m

val with_ident_ : string -> ?def:TT.tm_ -> TT.ty_ -> (TT.var -> 'a m) -> 'a m

val with_var : TT.var -> ?def:TT.tm -> TT.ty -> 'a m -> 'a m

val with_meta_ : string -> TT.ty_ -> chk:(TT.tm -> bool) -> (TT.var -> 'a m) -> 'a m
