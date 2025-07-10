(* This file contains utilities for handling I/O. *)

open Batteries;;

(* Reads all of the contents of a file and returns it as a string. *)
let read_file (filename : string) : string =
  File.with_file_in filename IO.read_all
;;

(* Writes the given string as the only contents of a file. *)
let write_file (filename : string) (contents : string) : unit =
  File.with_file_out filename (fun inp -> IO.nwrite inp contents)
;;

(* Recursively deletes all files and directories in the provided directory. *)
let rec rmdir (filename : string) : unit =
  if Sys.file_exists filename then begin
    if Sys.is_directory filename then begin
      Sys.readdir filename
      |> Array.iter
        (fun f ->
           let newname = Filename.concat filename f in
           rmdir newname
        );
      Unix.rmdir filename
    end else
      Sys.remove filename
  end
;;
