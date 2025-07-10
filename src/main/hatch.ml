(** This file is the main entry point to our compilation toolchain.  It can be
    used as the "main" routine for a command-line tool that will compile;
    assemble; and link the user's code into an executable binary. *)

open Batteries;;
open HatchCompiler;;

(* This is the "main" for the hatch program. *)
let () =
  (* Get the name of the file. *)
  if Array.length Sys.argv <> 2 then begin
    prerr_endline ("You must provide a single argument: "^
                   "the name of the file to compile.");
    Stdlib.exit 1
  end;
  let filename = Sys.argv.(1) in
  try
    ignore @@ Builder.build filename
  with
  | Builder.BuildFailure s ->
    prerr_endline s;
    Stdlib.exit 1
