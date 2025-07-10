(* This module contains utilities to ease the creation of tests for your
   compiler. *)

open Batteries;;
open HatchCompiler;;

open OUnit2;;

open Printf;;

(* This function creates a test of a program which is expected to compile and
   run successfully.  The arguments are the name of the file and the expected
   output. *)
let test_success (filename : string) (expected : string) =
  (* Pick a name for this test. *)
  let test_name = sprintf "execute_%s" (Filename.basename filename) in
  (* Create the test itself. *)
  test_name >:: fun _ ->
    (* Compile the program. *)
    let executable_filename =
      try
        Builder.build filename
      with
      | Builder.BuildFailure msg ->
        assert_failure (sprintf "Failed to compile %s: %s" filename msg)
    in
    (* Run the program and grab its output. *)
    let (_, output, _) =
      SubprocessUtils.run_and_log_command
        ~get_output:true
        test_name
        [executable_filename]
    in
    (* Determine if this output is what we expected. *)
    let expected_output = String.trim expected in
    let actual_output = String.trim output in
    assert_equal ~printer:(fun x -> x) expected_output actual_output
;;

(* This function creates a test of a program which is expected to fail to
   compile.  The arguments are the name of the file and the expected error
   message. *)
let test_compile_failure (filename : string) (expected : string) =
  (* Pick a name for this test. *)
  let test_name = sprintf "compile_failure_%s" (Filename.basename filename) in
  (* Create the test itself. *)
  test_name >:: fun _ ->
    (* Compile the program. *)
    begin
      try
        ignore @@ Builder.build filename;
        assert_failure
          (sprintf "Compilation of %s succeeded but was expected to fail." filename)
      with
      | Builder.BuildFailure msg ->
        (* Verify that this is the error we expected. *)
        assert_equal
          ~printer:(fun x -> x)
          (String.trim expected)
          (String.trim msg)
    end
;;

(* This function creates a test of a program which is expected to compile but
   fail at runtime.  This test passes only if the program compiles and, when
   run, produces the provided exit code. *)
let test_runtime_failure (filename : string) (expected_exit_code : int) =
  (* Pick a name for this test. *)
  let test_name = sprintf "execute_failure_%s" (Filename.basename filename) in
  (* Create the test itself. *)
  test_name >:: fun _ ->
    (* Compile the program. *)
    let executable_filename =
      try
        Builder.build filename
      with
      | Builder.BuildFailure msg ->
        assert_failure (sprintf "Failed to compile %s: %s" filename msg)
    in
    (* Run the program and expect it to fail. *)
    let (exitcode, _, _) =
      SubprocessUtils.run_and_log_command
        ~fail_on_exit:false
        test_name
        [executable_filename]
    in
    (* Determine if this output is what we expected. *)
    assert_equal ~printer:string_of_int expected_exit_code exitcode
;;
