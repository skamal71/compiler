open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;

(* Check for duplicate parameters *)
let check_duplicate_parameters (func_name : string) (params : string list) : string list =
  let rec check seen params =
    match params with
    | [] -> []
    | param :: rest ->
        if List.mem param seen then
          ["Function " ^ func_name ^ " has duplicate parameter " ^ param ^ "."]
          @ check seen rest
        else
          check (param :: seen) rest
  in
  check [] params

(* Check function declarations *)
let check_function_declarations (decls : declaration list) : string list =
  let rec check seen_funcs decls =
    match decls with
    | [] -> []
    | DFunction(name, _, _) :: rest ->
        if List.mem name seen_funcs then
          ["Duplicate definition of function " ^ name ^ "."]
          @ check seen_funcs rest
        else
          check (name :: seen_funcs) rest
  in
  check [] decls

(* Check for unbound variables *)
let rec check_unbound_variables (env : string list) (e : expr) : string list =
  match e with
  | EInt _ | EBool _ -> []
  | EVar name ->
      if List.mem name env then [] else ["Unbound variable " ^ name ^ "."]
  | EUnaryOp (_, e1)
  | EParallel e1
  | EReceive e1
  | ESleep e1 ->
      check_unbound_variables env e1
  | EBinaryOp (_, e1, e2)
  | ESend (e1, e2)
  | EAppl (e1, e2) ->
      check_unbound_variables env e1 @ check_unbound_variables env e2
  | ELet (name, e1, e2) ->
      check_unbound_variables env e1
      @ check_unbound_variables (name :: env) e2
  | EIf (cond, then_e, else_e) ->
      check_unbound_variables env cond
      @ check_unbound_variables env then_e
      @ check_unbound_variables env else_e
  | ETuple exprs ->
      List.flatten (List.map (check_unbound_variables env) exprs)


(* Simplified function call checking - no argument count or existence checks *)
let check_function_calls (_ : declaration list) (e : expr) : string list =
  let rec check e =
    match e with
    | EAppl (e1, e2)        -> check e1 @ check e2
    | EBinaryOp (_, e1, e2)
    | ESend (e1, e2)        -> check e1 @ check e2
    | EUnaryOp (_, e1)
    | EParallel e1
    | EReceive e1
    | ESleep e1             -> check e1
    | ELet (_, e1, e2)      -> check e1 @ check e2
    | EIf (c, t, el)        -> check c @ check t @ check el
    | ETuple exprs          -> List.flatten (List.map check exprs)
    | _                     -> []
  in
  check e


(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  match p with
  | Program (decls, main_expr) ->
      let dup_func_errors = check_function_declarations decls in
      let dup_param_errors =
        List.flatten (
          List.map (fun decl ->
            match decl with
            | DFunction (name, params, _) ->
                check_duplicate_parameters name params
          ) decls
        )
      in
      (* Gather all declared function names into a list. *)
      let top_func_names =
        List.map (function
          | DFunction (fname, _, _) -> fname
        ) decls
      in
      (* For each function, check unbound variables.  Environment is top-func-names plus that function's parameters. *)
      let fun_body_errors =
        List.flatten (
          List.map (function
            | DFunction (_, params, body) ->
                check_unbound_variables (top_func_names @ params) body
          ) decls
        )
      in
      (* Now check the main expression with the environment that knows all top-level names. *)
      let main_expr_errors =
        check_unbound_variables top_func_names main_expr
      in
      let unbound_var_errors = fun_body_errors @ main_expr_errors in

      let func_call_errors = check_function_calls decls main_expr in

      dup_func_errors @ dup_param_errors @ unbound_var_errors @ func_call_errors


(* This function will check a program for compile-time errors. If any errors
   are found, an IllFormed exception is thrown. Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;