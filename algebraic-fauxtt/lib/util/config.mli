(** Configuration parameters that control how fauxtt works. *)

(** Should the interactive shell be started. *)
val interactive_shell : bool ref

(** How deeply should large expressions be printed. *)
val max_boxes : int ref

(** How many columns should be used for printing expressions. *)
val columns : int ref

(** How verbose should the output be. *)
val verbosity : int ref

(** Should we restrict to ASCII-only output. *)
val ascii : bool ref
