(** Top-level processing. *)

type state = Context.t

let initial = Context.initial

let penv = Context.penv

let exec_interactive () =
  let e = Parsing.Lexer.read_toplevel Parsing.Parser.commandline () in
  Typecheck.toplevel ~quiet:false e

let load_file ~quiet fn =
  Typecheck.topfile ~quiet fn
