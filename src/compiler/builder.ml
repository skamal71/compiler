(** This file is the main entry point to our compilation toolchain.  It can be
    used as the "main" routine for a command-line tool that will compile,
    assemble, and link the user's code into an executable binary. *)

open Batteries;;
open HatchLanguage;;
open Asts;;

open Printf;;
open SubprocessUtils;;

(* An exception representing a controlled failure of the hatching process. *)
exception BuildFailure of string;;

(* A list of the runtime resources we need to link in to build an executable.
   These are .c files which should be compiled into .o files and then linked at
   the end.  They are compiled in the order listed here. *)
let runtime_code = ["driver.c"; "printer.c"; "error.c"];;

(* A utility function which, while compiling an AST into assembly, translates
   compiler-specific exceptions into build error messages. *)
let compile_to_assembly_code (program : program) : string =
  try
    Compiler.compile_to_assembly_code program
  with
    | Compiler.BuildFailure msg -> raise (BuildFailure msg)

(* Compiles, assembles, and links a single source file into a program.  If
   anything goes wrong, raises BuildFailure with a message.  On success, returns
   the name of the compiled executable file. *)
let build (filename : string) : string =
  (* Verify that the filename ends in ".bird" *)
  if not (String.ends_with filename ".bird") then
    raise (BuildFailure "I only know how to hatch .bird files.");
  let basename = Filename.chop_extension @@ Filename.basename filename in
  (* Create the output directory. *)
  let output_directory = "output" in
  begin
    try
      Unix.mkdir output_directory 0o700
    with
    | Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end;
  (* Read in the contents of the provided file. *)
  let contents = FileUtils.read_file filename in
  (* Parse it into an AST. *)
  let ast =
    try
      ParserTool.parse filename contents
    with
    | ParserTool.ParseFailure msg -> raise (BuildFailure msg)
  in
  (* Compile it into assembly code. *)
  let assembly_code = compile_to_assembly_code ast in
  (* Write those assembly instructions to a file. *)
  let asm_filename = Filename.concat output_directory @@ basename ^ ".s" in
  FileUtils.write_file asm_filename assembly_code;
  (* Determine appropriate binary format for this system. *)
  let format =
    match determine_runtime_sort () with
    | Some Linux -> "elf64"
    | Some MacOSX -> "macho64"
    | _ ->
      raise (BuildFailure
               "Could not determine appropriate binary format for this system.")
  in
  (* Bizarre things happen on some systems in some corner cases if we don't
     flush before launching subprocesses (like failed subprocesses getting a
     copy of our buffer. *)
  try
    (* Assemble the generated assembly into a program object. *)
    let program_o_filename =
      Filename.concat output_directory @@ basename ^ ".o"
    in
    let (_,output,error) =
      run_and_log_command ~get_output:true ("nasm_" ^ basename)
        ["nasm"; "-g"; "-f"; format; "-o"; program_o_filename; asm_filename]
    in
    (* nasm can't be trusted to complain via error code about warnings and the
       warnings it generates are often *really* important.  We'll fix that by
       bailing if we see any output from nasm at all. *)
    if not (String.is_empty output && String.is_empty error) then begin
      raise (BuildFailure(
          "nasm generated the following warning/error output:\n" ^
          output ^ "\n" ^ error))
    end;
    (* Compile the runtime into object files. *)
    let object_files =
      runtime_code
      |> List.map
        (fun filename ->
           let o_filename_base = Filename.chop_extension filename ^ ".o" in
           let o_filename = Filename.concat "output" o_filename_base in
           ignore @@ run_and_log_command
             ~pwd:(Some "resources")
             ("clang_compile_" ^ Filename.chop_extension filename)
             ["clang"; "-g"; "-mstackrealign"; "-c"; "-o";
              Filename.concat ".." o_filename; filename];
           o_filename)
    in
    (* Link the driver and the program object together. *)
    let program_run_filename =
      Filename.concat output_directory @@ basename ^ ".run"
    in
    ignore @@ run_and_log_command ("clang_link_" ^ basename)
      (["clang"; "-no-pie"; "-g"; "-z"; "noexecstack";
        "-o"; program_run_filename; program_o_filename] @ object_files);
    program_run_filename
  with
  | SubprocessUtils.SubprocessFailure msg -> raise (BuildFailure msg)
