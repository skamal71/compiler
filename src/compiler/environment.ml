(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
  let (next_free, dict) = env in 
  let new_addr = ArgMemory (AddrByRegisterOffset (RBP, next_free)) in
  let new_dict = Map.String.add name new_addr dict in 
  let new_env = (next_free - 8, new_dict) in 
  new_env
;;

(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
   let (_, dict) = env in 
   match Map.String.find_opt name dict with 
   | Some addr -> addr
   | None -> 
       try
         ArgLabelOffset("closure_of_" ^ name, 1)
       with _ -> 
         raise (UnboundVariable (name, env))
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let (next_free, dict) = env in 
  let temp_addr = ArgMemory (AddrByRegisterOffset (RBP, next_free)) in 
  (temp_addr, (next_free - 8, dict))
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;