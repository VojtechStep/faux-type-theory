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

(** Assign a value to a meta-variable. It is the caller's responsbility to assign a value of correct type. It is error
    to assign a value to an already defined meta. *)
val define : TT.var -> TT.tm -> unit m

(** Extend the context with a top-level definition and return it *)
val top_extend : string -> ?def:TT.tm -> TT.ty -> t -> t

(** The identifiers which should not be used for printing bound variables. *)
val penv : t -> Bindlib.ctxt

(** Lookup the definition associated with a variable, if any. *)
val lookup_var : TT.var -> (TT.tm option * TT.ty) m

(** Lookup the definition associated with a meta-variable, if any. *)
val lookup_meta : TT.var -> (TT.tm option * TT.ty) m

(** Lookup the variable which corresponds to a concrete name. *)
val lookup_ident : string -> TT.var option m

(** Run a computation in a context extended with a variable, passing it the newly
    created variable. It is the callers responsibility that the result be valid in
    the original context. *)

val with_ident : string -> ?def:TT.tm -> TT.ty -> (TT.var -> 'a m) -> 'a m

val with_ident_ : string -> ?def:TT.tm_ -> TT.ty_ -> (TT.var -> 'a m) -> 'a m

val with_var : TT.var -> ?def:TT.tm -> TT.ty -> 'a m -> 'a m

val with_meta : string -> TT.ty -> (TT.tm -> 'a m) -> 'a m

val with_meta_ : string -> TT.ty_ -> (TT.tm_ -> 'a m) -> 'a m

val close_tm_ : TT.tm_ -> TT.tm_ option m
