(** Global context. *)
type t

(** The initial global typing context. *)
val initial : t

(** Assign a value to a meta-variable. It is the caller's responsbility to assign a value of correct type.
    It is an error to assign a value to an already defined meta. *)
val set_meta_ : TT.var -> TT.tm_ -> bool

(** Extend the context with a top-level definition and return it *)
val top_extend : string -> ?def:TT.tm -> TT.ty -> unit

(** The identifiers which should not be used for printing bound variables. *)
val penv : t -> Bindlib.ctxt

(** Lookup the definition associated with a variable, if any. *)
val lookup_var : TT.var -> TT.tm option * TT.ty

(** Lookup the definition associated with a meta-variable, if any. *)
val lookup_meta : TT.var -> TT.tm option * TT.ty

(** Lookup the variable which corresponds to a concrete name. *)
val lookup_ident : string -> TT.var option

(** Run a computation in a context extended with a variable, passing it the newly
    created variable. It is the callers responsibility that the result be valid in
    the original context. *)

val handle_context : (unit -> 'a) -> 'a

val handle_metas : (unit -> unit) -> unit

val with_ident : string -> ?def:TT.tm -> TT.ty -> (TT.var -> 'a) -> 'a

val with_ident_ : string -> ?def:TT.tm_ -> TT.ty_ -> (TT.var -> 'a) -> 'a

val with_var : TT.var -> ?def:TT.tm -> TT.ty -> (unit -> 'a) -> 'a

val with_meta : string -> TT.ty -> (TT.tm -> 'a) -> 'a

val with_meta_ : string -> TT.ty_ -> (TT.tm_ -> 'a) -> 'a
