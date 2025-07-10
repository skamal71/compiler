(* This file contains utilities for running other programs and collectin the
   results. *)

open Batteries;;

open Printf;;

(** Indicates that a subprocess failed. *)
exception SubprocessFailure of string;;

(** Erases all subprocess log files from previous runs. *)
let clear_logs () =
  FileUtils.rmdir "logs";
;;

let with_cleanup (cleanup : unit -> unit) (operation : unit -> 'a) =
  try
    let answer = operation () in
    cleanup ();
    answer
  with exc ->
    cleanup ();
    raise exc
;;

(**
   Executes a subprocess using the provided command.  Standard output and
   standard error are logged to files.

   By default, a non-zero exit code causes this function to raise a
   SubprocessFailure exception.  This behavior may be suppressed by setting the
   optional fail_on_exit argument to false.

   By default, this function always returns two empty strings for the program
   ouptut and error streams.  If the optional get_output argument is set to
   true, this function will instead open the written log files, retrieve the
   contents, and provide them as the program output and error.

   By default, this function runs the subprocess in the current working
   directory.  If the optional argument pwd is set, this function runs in that
   directory instead.
*)
let run_and_log_command
    ?fail_on_exit:(fail_on_exit=true)
    ?get_output:(get_output=false)
    ?pwd:(pwd=None)
    (name : string)
    (command : string list)
  : int * string * string =
  (* Make sure log directory exists. *)
  begin
    try
      Unix.mkdir "logs" 0o700
    with
    | Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end;
  (* Open the appropriate log files. *)
  let stdout_name = Filename.concat "logs" (name ^ ".stdout.log") in
  let stderr_name = Filename.concat "logs" (name ^ ".stderr.log") in
  let stdout_fd, stdout_input =
      let r,w = Unix.pipe () in
      (w, Unix.input_of_descr ~cleanup:true r)
  in
  with_cleanup (fun () ->
      try Unix.close stdout_fd with Unix.Unix_error(Unix.EBADF, _, _) -> ()
    ) @@ fun _ ->
  let stderr_fd =
    Unix.openfile stderr_name [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600
  in
  with_cleanup (fun () -> Unix.close stderr_fd) @@ fun _ ->
  let (stdin_read_fd, stdin_write_fd) = Unix.pipe () in
  Unix.close stdin_write_fd;
  with_cleanup (fun () -> Unix.close stdin_read_fd) @@ fun _ ->
  (* Change directories if appropriate. *)
  let current_directory = Unix.getcwd () in begin
    match pwd with
    | None -> ()
    | Some d -> Unix.chdir d
  end;
  (* Start the subprocess. *)
  let subprocess_pid =
    Unix.create_process (List.first command) (Array.of_list command)
      stdin_read_fd stdout_fd stderr_fd
  in
  (* Change back to our real directory. *)
  Unix.chdir current_directory;
  (* After the fork, we need to close our copy of the write side of the stdout
     pipe. *)
  Unix.close stdout_fd;
  (* If necessary, soak up all of the process's output. *)
  let program_output = IO.read_all stdout_input in
  (* Write the output to a log file. *)
  FileUtils.write_file stdout_name program_output;
  (* Wait for the subprocess to terminate. *)
  let (_,status) = Unix.waitpid [] subprocess_pid in
  (* Set up the output we will report for this program. *)
  let program_output =
    if get_output then
      FileUtils.read_file stdout_name
    else
      ""
  in
  let program_error =
    if get_output then
      FileUtils.read_file stderr_name
    else
      ""
  in
  (* Check the exit status and produce a result. *)
  match status with
  | Unix.WEXITED(exitcode) ->
    if fail_on_exit && exitcode <> 0 then
      raise (SubprocessFailure(
          sprintf "%s command failed with exit code %d; see logs for details."
            name exitcode))
    else
      (exitcode,program_output,program_error)
  | _ ->
    raise (SubprocessFailure(
        sprintf "%s command failed; see logs for details." name))
;;

type runtime_sort =
  | Windows
  | MacOSX
  | Linux
  | Cygwin
;;

(** Determines the sort of OCaml runtime for this system.  Returns None if the
    runtime is unrecognized. *)
let determine_runtime_sort () : runtime_sort option =
  match Sys.os_type with
  | "Unix" ->
    begin
      let ic = Unix.open_process_in "uname" in
      let uname = input_line ic in
      IO.close_in ic;
      match uname with
      | "Linux" -> Some Linux
      | "Darwin" -> Some MacOSX
      | _ -> None
    end
  | "Win32" -> Some Windows
  | "Cygwin" -> Some Cygwin
  | _ -> None
;;
