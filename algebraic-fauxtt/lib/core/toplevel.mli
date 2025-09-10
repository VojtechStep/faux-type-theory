(** The top-level state of the proof assistant *)
type state = Context.t

(** Initial top-level state. *)
val initial : state

(** Read a top-level command from the standard input and execute it. *)
val exec_interactive : unit -> unit

(** Load the contents of a file and execute it. *)
val load_file : quiet:bool -> string -> unit

(** Names of bound variables, used for printing de Bruijn indices. *)
val penv : state -> Bindlib.ctxt
