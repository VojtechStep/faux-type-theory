(** The main executable. *)

open Util

(** The usage message. *)
let usage = "Usage: fauxtt [option] ... [file] ..."

(** A list of files to be loaded and run, together with information on whether they should
    be loaded in interactive mode. *)
let files = ref []

(** Add a file to the list of files to be loaded, and record whether it should
    be processed in interactive mode. *)
let add_file quiet filename = (files := (filename, quiet) :: !files)

(** Command-line options *)
let options = Arg.align [

    ("--columns",
     Arg.Set_int Config.columns,
     " Set the maximum number of columns of pretty printing");

    ("--ascii",
     Arg.Set Config.ascii,
     " Use ASCII characters only");

    ("-V",
     Arg.Set_int Config.verbosity,
     "<n> Set printing verbosity to <n>");

    ("-n",
     Arg.Clear Config.interactive_shell,
     " Do not run the interactive toplevel");

    ("-l",
     Arg.String (fun str -> add_file true str),
     "<file> Load <file> into the initial environment");
  ]

(* Print the error message corresponding to an exception. *)
let print_error ?(penv=Bindlib.empty_ctxt) = function
  | Parsing.Ulexbuf.Error {Location.data=err; Location.loc} ->
     Print.error "Lexical error at %t:@ %t" (Location.print loc) (Parsing.Ulexbuf.print_error err)

  | Core.Typecheck.Error {Location.data=err; Location.loc} ->
     Print.error "Typechecking error at %t:@ %t"
       (Location.print loc)
       (Core.Typecheck.print_error ~penv err)

  | Sys.Break ->
     Print.error "Interrupted." ;

  | e ->
     raise e

(* Interactive toplevel. *)
let interactive_shell () =
  Format.printf "Faux type theory implemented with algebraic effects 1.2@." ;

  let rec loop () =
    (try
      Core.Toplevel.exec_interactive ()
    with
    | e -> print_error e) ;
    loop ()
  in
  try
    loop ()
  with
    End_of_file -> ()

(* The main program. *)
let _main =
  Sys.catch_break true ;

  (* Parse the arguments. *)
  Arg.parse
    options
    (fun str -> add_file false str ; Config.interactive_shell := false)
    usage ;

  (* Files were accumulated in the wrong order, so we reverse them *)
  files := List.rev !files ;

  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes !Config.max_boxes ;
  Format.set_margin !Config.columns ;
  Format.set_ellipsis_text "..." ;

  Core.Context.with_context Core.Toplevel.initial
    (fun () ->
      try
        List.iter (fun (fn, quiet) -> Core.Toplevel.load_file ~quiet fn) !files ;
        if !Config.interactive_shell then
          interactive_shell ()
      with
      | e -> print_error e
    )
