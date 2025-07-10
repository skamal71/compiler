(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;
open Freshening;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Wellformedness;;

exception BuildFailure of string;;

let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))
(*For EAppl and EParallel*)
let compile_call_sequence (temp_clo : argument) (temp_arg : argument) : instruction list =
  let label_grow = fresh_name "grow_closure" in
  let label_call = fresh_name "call_func" in
  let label_done = fresh_name "done_appl" in

  (* 1) unpack closure pointer and read metadata *)
  let get_closure_metadata = [
    AsmMov (ArgRegister R11, temp_clo);
    AsmAnd (ArgRegister R11, ArgConstant "0xFFFFFFFFFFFFFFF8");
    AsmMov (ArgRegister R10, ArgMemory   (AddrByRegisterOffset (R11, 0)));  
    AsmMov (ArgRegister R13, ArgConstant "0x7FFFFFFFFFFFFFFF");
    AsmMov (ArgRegister R12, ArgRegister R13);
    AsmAnd (ArgRegister R10, ArgRegister R12);
    AsmMov (ArgRegister R8,  ArgMemory   (AddrByRegisterOffset (R11, 8))); 
    AsmMov (ArgRegister R9,  ArgMemory   (AddrByRegisterOffset (R11, 16)));  
  ] in

  (* 2) compare current+1 vs required *)
  let compare_counts = [
    AsmMov (ArgRegister R12, ArgRegister R10);
    AsmAdd (ArgRegister R12, ArgConstant "1");
    AsmCmp (ArgRegister R12, ArgRegister R8);
    AsmJl label_grow;
    AsmJe label_call;
  ] in

  (* 3) grow a new closure on the heap *)
  let closure_grow = [
    AsmLabel label_grow;

    (* bytes = (args+3)*8 *)
    AsmMov (ArgRegister R13, ArgRegister R12);
    AsmAdd (ArgRegister R13, ArgConstant "3");
    AsmShl (ArgRegister R13, ArgConstant "3");

    (* bump heap_cursor *)
    AsmMov (ArgRegister R14, ArgMemory (AddressByLabel "heap_cursor"));
    AsmAdd (ArgRegister R14, ArgRegister R13);
    AsmMov (ArgMemory (AddressByLabel "heap_cursor"), ArgRegister R14);
    AsmSub (ArgRegister R14, ArgRegister R13);

    (* copy old closure words *)
    AsmMov (ArgRegister RCX, ArgRegister R10);
    AsmAdd (ArgRegister RCX, ArgConstant "3");
    AsmMov (ArgRegister RSI, ArgRegister R11);
    AsmMov (ArgRegister RDI, ArgRegister R14);
    AsmRepMovsq;

    (* store the new argument *)
    AsmMov (ArgRegister RAX, ArgRegister R10);
    AsmAdd (ArgRegister RAX, ArgConstant "3");
    AsmShl (ArgRegister RAX, ArgConstant "3");
    AsmAdd (ArgRegister RAX, ArgRegister R14);
    AsmMov (ArgRegister R15, temp_arg);
    AsmMov (ArgMemory (AddrByRegister RAX), ArgRegister R15);

    (* update arg count word *)
    AsmMov (ArgRegister RAX, ArgRegister R10);
    AsmAdd (ArgRegister RAX, ArgConstant "1");
    AsmQOr (ArgRegister RAX, ArgMemory (AddressByLabel "big_const_0x8000000000000000"));
    AsmMov (ArgMemory (AddrByRegister R14), ArgRegister RAX);

    (* return new closure pointer *)
    AsmMov (ArgRegister RAX, ArgRegister R14);
    AsmOr (ArgRegister RAX, ArgConstant "1");
    AsmJmp label_done;
  ] in

  (* 4) saturated: push stack args & call *)
  let call_function = [
    AsmLabel label_call;

    (* reserve stack for args *)
    AsmMov (ArgRegister R15, ArgRegister R8);
    AsmShl (ArgRegister R15, ArgConstant "3");
    AsmSub (ArgRegister RSP, ArgRegister R15);

    (* copy pointer words *)
    AsmMov (ArgRegister RSI, ArgRegister R11);
    AsmAdd (ArgRegister RSI, ArgConstant "24");
    AsmMov (ArgRegister RDI, ArgRegister RSP);
    AsmMov (ArgRegister RCX, ArgRegister R10);
    AsmRepMovsq;

    (* place the single new argument *)
    AsmMov (ArgRegister RAX, temp_arg);
    AsmMov (ArgMemory (AddrByRegister RDI), ArgRegister RAX);

    (* indirect call *)
    AsmMov (ArgRegister RAX, ArgRegister R9);
    AsmCall ( "rax" );

    (* restore stack *)
    AsmAdd (ArgRegister RSP, ArgRegister R15);
  ] in

  get_closure_metadata
  @ get_closure_metadata
  @ compare_counts
  @ closure_grow
  @ call_function
  @ [ AsmLabel label_done ]


let build_channel_in_rax : instruction list = [
  (* channel = ((read<<32)|write) << 3 | 3 *)
  AsmShl (ArgRegister RDX, ArgConstant "32");          (* read<<32 in RDX   *)
  AsmOr  (ArgRegister RDX, ArgRegister RAX);           (* (read<<32)|write  *)
  AsmMov (ArgRegister RAX, ArgRegister RDX);
  AsmShl (ArgRegister RAX, ArgConstant "3");           (* <<3               *)
  AsmOr  (ArgRegister RAX, ArgConstant "3");           (* … | 3             *)
]

let check_rax_is_channel () : instruction list =
  let lbl_ok   = fresh_name "ischannel_ok" in
  let lbl_fail = fresh_name "ischannel_fail" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmAnd (ArgRegister RAX, ArgConstant "7");
    AsmCmp (ArgRegister RAX, ArgConstant "3");
    AsmJe  lbl_ok;
    AsmLabel lbl_fail;
    AsmMov (ArgRegister RDI, ArgConstant "8");
    AsmCall "stopWithError";
    AsmLabel lbl_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

let check_rax_is_bool () : instruction list =
  let label_ok = fresh_name "checkbool_ok" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmAnd (ArgRegister RAX, ArgConstant "3");
    AsmCmp (ArgRegister RAX, ArgConstant "3");
    AsmJe label_ok;

    AsmMov (ArgRegister RDI, ArgConstant "2");
    AsmCall "stopWithError";

    AsmLabel label_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

let check_rax_is_int () : instruction list =
  let label_ok = fresh_name "checkint_ok" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmAnd (ArgRegister RAX, ArgConstant "1");
    AsmCmp (ArgRegister RAX, ArgConstant "0");
    AsmJe label_ok;

    AsmMov (ArgRegister RDI, ArgConstant "1");
    AsmCall "stopWithError";

    AsmLabel label_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

(* helper: accept int (LSB=0)   or   bool (LSB=11)  else -> error 9 *)
let check_rax_int_or_bool () : instruction list =
  let lbl_ok       = fresh_name "ib_ok" in
  let lbl_try_bool = fresh_name "ib_try_bool" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);

    (* quick int test: LSB = 0 *)
    AsmAnd (ArgRegister RAX, ArgConstant "1");
    AsmCmp (ArgRegister RAX, ArgConstant "0");
    AsmJe  lbl_ok;

    (* ─── not int; maybe boolean? ─── *)
    AsmMov (ArgRegister RAX, ArgRegister R11);          (* restore *)
    AsmAnd (ArgRegister RAX, ArgConstant "7");
    AsmCmp (ArgRegister RAX, ArgConstant "3");
    AsmJne lbl_try_bool;                                (* low bits not 011 *)

    (* load the two legal boolean patterns into R12 and compare *)
    AsmMov (ArgRegister R12, ArgConstant "0xFFFFFFFFFFFFFFFF");      
    AsmCmp (ArgRegister R11, ArgRegister R12);
    AsmJe  lbl_ok;

    AsmMov (ArgRegister R12, ArgConstant "0x7FFFFFFFFFFFFFFF");
    AsmCmp (ArgRegister R11, ArgRegister R12);
    AsmJe  lbl_ok;

    AsmLabel lbl_try_bool;
    AsmMov (ArgRegister RDI, ArgConstant "9");     
    AsmCall "stopWithError";

    AsmLabel lbl_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

let check_rax_is_tuple () : instruction list =
  let label_ok = fresh_name "checktuple_ok" in
  let label_fail = fresh_name "checktuple_fail" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    (* Check that low 3 bits = 1 *)
    AsmAnd (ArgRegister RAX, ArgConstant "7");
    AsmCmp (ArgRegister RAX, ArgConstant "1");
    AsmJne label_fail;

    (* Read the top bit in the first word to see if it is 0 *)
    AsmMov (ArgRegister RAX, ArgRegister R11);
    AsmAnd (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFF8"); 
    AsmMov (ArgRegister RAX, ArgMemory (AddrByRegister RAX));
    AsmShr (ArgRegister RAX, ArgConstant "63");  (* top bit => bit 0 *)
    AsmCmp (ArgRegister RAX, ArgConstant "0");
    AsmJne label_fail;

    AsmJmp label_ok;

    AsmLabel label_fail;
    AsmMov (ArgRegister RDI, ArgConstant "3");  (* "tuple expected" *)
    AsmCall "stopWithError";

    AsmLabel label_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

let check_rax_is_closure () : instruction list =
  let label_ok = fresh_name "checkclosure_ok" in
  let label_fail = fresh_name "checkclosure_fail" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    (* 1) Check the pointer tag (lowest 3 bits) == 1. If not => fail. *)
    AsmAnd (ArgRegister RAX, ArgConstant "7");
    AsmCmp (ArgRegister RAX, ArgConstant "1");
    AsmJne label_fail;

   (* Read the top bit in the first word to see if it is 0 *)
    AsmMov (ArgRegister RAX, ArgRegister R11);
    AsmAnd (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFF8");
    AsmMov (ArgRegister RAX, ArgMemory (AddrByRegister RAX));
    AsmShr (ArgRegister RAX, ArgConstant "63");
    AsmCmp (ArgRegister RAX, ArgConstant "1");
    AsmJne label_fail;
    (* If both checks pass, jump to label_ok. *)
    AsmJmp label_ok;
    (* Label for failing with error code 5 => "closure expected." *)
    AsmLabel label_fail;
    AsmMov (ArgRegister RDI, ArgConstant "5");
    AsmCall "stopWithError";

    AsmLabel label_ok;
    AsmMov (ArgRegister RAX, ArgRegister R11);
  ]

let rec compile_expression (env : environment) (e : expr): instruction list =
  match e with
  | EInt n ->
    [AsmMov (ArgRegister RAX, ArgConstant (string_of_twice_int n))]

  | EVar name ->
    let addr = find_named_variable name env in
    [AsmMov (ArgRegister RAX, addr)]

  | EBool b ->
    if b then
      [AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF")]
    else
      [AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF")]
  
  | EUnaryOp (OpPrint, expr) ->
    let code_exp = compile_expression env expr in
    let call_code = [
      AsmPush (ArgRegister RAX);
      AsmMov (ArgRegister RDI, ArgRegister RAX);
      AsmCall "printValue";
      AsmPop (ArgRegister RAX);
    ] in
    code_exp @ call_code

  | EUnaryOp (OpAfter, expr) ->
    let code_exp = compile_expression env expr in
    code_exp 
      @ check_rax_is_int ()
      @ [AsmAdd (ArgRegister RAX, ArgConstant "2");]

  | EUnaryOp (OpBefore, expr) ->
    let code_exp = compile_expression env expr in
    code_exp 
      @ check_rax_is_int ()
      @ [AsmSub (ArgRegister RAX, ArgConstant "2")]

  | EUnaryOp (OpIsInt, expr) ->
    let code_exp = compile_expression env expr in
    let label_false = fresh_name "isint_false" in
    let label_end   = fresh_name "isint_end" in
    code_exp @ [
      AsmAnd (ArgRegister RAX, ArgConstant "1");
      AsmXor (ArgRegister RAX, ArgConstant "1");

      AsmCmp (ArgRegister RAX, ArgConstant "0");
      AsmJe label_false;  

      AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");
      AsmJmp label_end;

      AsmLabel label_false;
      AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");

      AsmLabel label_end
    ]
    
  | EUnaryOp (OpIsBool, expr) ->
    let code_exp = compile_expression env expr in
    let label_not_bool = fresh_name "isbool_not_bool" in
    let label_is_bool = fresh_name "isbool_is_bool" in
    let label_end  = fresh_name "isbool_end" in
    code_exp @ [
      AsmMov(ArgRegister R11, ArgRegister RAX);
      AsmAnd(ArgRegister RAX, ArgConstant "3");
      AsmCmp(ArgRegister RAX, ArgConstant "3");
      AsmJne label_not_bool;

      AsmMov(ArgRegister RAX, ArgRegister R11);
      AsmMov(ArgRegister R10, ArgConstant "0xFFFFFFFFFFFFFFFF"); 
      AsmCmp(ArgRegister RAX, ArgRegister R10);
      AsmJe label_is_bool;

      AsmMov(ArgRegister R10, ArgConstant "0x7FFFFFFFFFFFFFFF"); 
      AsmCmp(ArgRegister RAX, ArgRegister R10);
      AsmJe label_is_bool;

      AsmLabel label_not_bool;
      AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");
      AsmJmp label_end;

      AsmLabel label_is_bool;
      AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");

      AsmLabel label_end
    ]

  | EUnaryOp (OpIsTuple, expr) ->
  let code_exp = compile_expression env expr in
  let label_false = fresh_name "istuple_false" in
  let label_end  = fresh_name "istuple_end" in
  code_exp @ [
    (* We'll preserve the original pointer in R11. *)
    AsmMov(ArgRegister R11, ArgRegister RAX);

    (* Check low 3 bits = 1 *)
    AsmAnd(ArgRegister RAX, ArgConstant "7");
    AsmCmp(ArgRegister RAX, ArgConstant "1");
    AsmJne label_false;

    (* Clear tag, read top word from memory, shift >> 63 *)
    AsmMov(ArgRegister RAX, ArgRegister R11);
    AsmAnd(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFF8");
    AsmMov(ArgRegister RAX, ArgMemory(AddrByRegister RAX));
    AsmShr(ArgRegister RAX, ArgConstant "63");
    AsmCmp(ArgRegister RAX, ArgConstant "0");
    AsmJne label_false;

    (* If here, it's definitely a tuple. Return “true”. *)
    AsmMov(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");  (* 0xFFFFFFFFFFFFFFFF *)
    AsmJmp label_end;

    AsmLabel label_false;
    (* Not a tuple => “false”. Build 0x7FFFFFFFFFFFFFFF 
       with the same 2-instruction approach: *)
    AsmMov(ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");

    AsmJmp label_end;

    AsmLabel label_end
  ]
  | ESleep expr ->
    let code_exp = compile_expression env expr in
    code_exp
    @ check_rax_is_int ()
    @ [
      (*for two words*)
      AsmSub (ArgRegister RSP, ArgConstant "16");
      AsmMov (ArgMemory (AddrByRegisterOffset (RSP, 0)), ArgRegister RAX);
      AsmMov (ArgMemory (AddrByRegisterOffset (RSP, 8)), ArgConstant "0");
      AsmMov (ArgRegister RAX, ArgConstant "35"); 
      AsmMov (ArgRegister RDI, ArgRegister RSP);
      AsmMov (ArgRegister RSI, ArgConstant "0");
      AsmSyscall;
      AsmAdd (ArgRegister RSP, ArgConstant "16");
      AsmMov (ArgRegister RAX, ArgConstant "0");
    ]

  | ELet (name, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let env2 = allocate_named_variable name env in
    let addr = find_named_variable name env2 in
    let store_code = [AsmMov (addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    code1 @ store_code @ code2

  | EBinaryOp (OpPlus, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1 = check_rax_is_int () in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in

    let code2 = compile_expression env2 expr2 in
    let check2 = check_rax_is_int () in
    let code_add = [AsmAdd (ArgRegister RAX, temp_addr)] in
    code1 @ check1 @ store_left @ code2 @check2 @ code_add


  | EBinaryOp (OpMinus, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1 = check_rax_is_int () in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in

    let code2 = compile_expression env2 expr2 in
    let check2 = check_rax_is_int () in
    let code_sub = [
      AsmMov (ArgRegister R10, ArgRegister RAX);
      AsmMov (ArgRegister RAX, temp_addr);
      AsmSub (ArgRegister RAX, ArgRegister R10)
    ] in
    code1 @ check1 @store_left @ code2 @check2 @ code_sub

  | EBinaryOp (OpTimes, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1 = check_rax_is_int () in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_code = [AsmMov (temp_addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    let check2 = check_rax_is_int () in
    code1 @ check1 @ store_code @ code2 @check2 @ 
    [AsmSar (ArgRegister RAX, ArgConstant "1");
     AsmIMul (ArgRegister RAX, temp_addr)]

  | EBinaryOp (OpLessThan, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1 = check_rax_is_int () in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    let check2 = check_rax_is_int () in
    let label_true = fresh_name "lt_true" in
    let label_done = fresh_name "lt_done" in
    let code_cmp = [
      AsmCmp (temp_addr, ArgRegister RAX);
      AsmJl label_true;    
      (*false*)       
      AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");  
      AsmJmp label_done;
      AsmLabel label_true;
      (*true*)
      AsmMov (ArgRegister RAX, ArgConstant"0xFFFFFFFFFFFFFFFF"); 
      AsmLabel label_done
      ]
    in
    code1 @ check1 @ store_left @ code2 @check2 @ code_cmp

  | EBinaryOp (OpGreaterThan, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1 = check_rax_is_int () in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    let check2 = check_rax_is_int () in
    let label_true = fresh_name "gt_true" in
    let label_done = fresh_name "gt_done" in
    let code_cmp = [
      (*temp_addr < rax*)
      (*4 > 3*)
      AsmCmp (temp_addr, ArgRegister RAX);
      AsmJg label_true;    
      (*false*)       
      AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");  
      AsmJmp label_done;
      AsmLabel label_true;
      (*true*)
      AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF"); 
      AsmLabel label_done
      ]
    in
    code1 @ check1 @ store_left @ code2 @check2 @ code_cmp

  | EBinaryOp (OpEqualTo, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    let else_label = fresh_name "if_else" in
    let end_label = fresh_name "if_end" in
    let code_cmp = [
      AsmCmp (temp_addr, ArgRegister RAX);
      (*true*)     
      AsmJne else_label;    
      AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");  
      AsmJmp end_label;
      (*false*)
      AsmLabel else_label;
      AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF"); 
      AsmLabel end_label
      ]
    in
    code1 @ store_left @ code2 @ code_cmp

  | EBinaryOp (OpAnd, expr1, expr2) ->
    let code1   = compile_expression env expr1 in
    let check1  = check_rax_is_bool () in

    let (temp_addr, env2) = allocate_temp_variable env in
    let store_left = [AsmMov (temp_addr, ArgRegister RAX)] in

    let code2   = compile_expression env2 expr2 in
    let check2  = check_rax_is_bool () in

    let code_and = [AsmAnd (ArgRegister RAX, temp_addr)] in

    code1 @ check1 @ store_left @ code2 @ check2 @ code_and

  | EBinaryOp (OpOr, expr1, expr2) ->
    let code1 = compile_expression env expr1 in
    let check1  = check_rax_is_bool () in

    let (temp_addr, env2) = allocate_temp_variable env in
    let store_code = [AsmMov (temp_addr, ArgRegister RAX)] in
    let code2 = compile_expression env2 expr2 in
    let check2  = check_rax_is_bool () in

    let code_or = [AsmOr (ArgRegister RAX, temp_addr)] in
    code1 @ check1 @store_code @ code2 @ check2 @ code_or

  | EBinaryOp (OpTupleIndex, tuple_expr, index_expr) ->
    let compile_tuple = compile_expression env tuple_expr in
    let check_is_tuple = check_rax_is_tuple () in
    
    let (temp_tuple_addr, env2) = allocate_temp_variable env in
    let save_tuple_ptr = [AsmMov(temp_tuple_addr, ArgRegister RAX)] in

    let compile_index = compile_expression env2 index_expr in
    let check_is_int = check_rax_is_int () in

    let label_get_element = fresh_name "get_element" in
    let label_index_error = fresh_name "index_error" in

    let index_logic = [
      AsmMov(ArgRegister R11, temp_tuple_addr); 
      AsmAnd(ArgRegister R11, ArgConstant "0xFFFFFFFFFFFFFFF8");
      AsmMov(ArgRegister R10, ArgRegister RAX);
      AsmSar(ArgRegister R10, ArgConstant "1");
      AsmCmp(ArgRegister R10, ArgConstant "0");
      AsmJl label_index_error;

      AsmMov(ArgRegister R9, ArgMemory(AddrByRegister R11)); 
      AsmCmp(ArgRegister R10, ArgRegister R9);
      AsmJg label_index_error;
      AsmJe label_index_error;
      AsmJmp label_get_element;
      
      AsmLabel label_index_error;
      AsmMov(ArgRegister RDI, ArgConstant "4");
      AsmCall "stopWithError";

      AsmLabel label_get_element;
      AsmAdd(ArgRegister R10, ArgConstant "1");
      AsmMov(ArgRegister RAX, ArgMemory(AddressByRegisterProductOffset(R11, R10, 8)))
    ] in

    compile_tuple @ 
    check_is_tuple @ 
    save_tuple_ptr @ 
    compile_index @ 
    check_is_int @ 
    index_logic
  
  | ESend (chan_expr, data_expr) ->
    let code_chan     = compile_expression env chan_expr in
    let check_chan    = check_rax_is_channel () in
    let (tmp_chan, env1) = allocate_temp_variable env in
    let save_chan     = [AsmMov (tmp_chan, ArgRegister RAX)] in

    let code_data     = compile_expression env1 data_expr in
    let check_dat     = check_rax_int_or_bool () in
    let (tmp_dat, _env2) = allocate_temp_variable env1 in
    let save_dat      = [AsmMov (tmp_dat, ArgRegister RAX)] in

    let send_logic = [
      (* scratch buffer on stack *)
      AsmSub (ArgRegister RSP, ArgConstant "8");
      AsmMov (ArgMemory (AddrByRegister RSP), ArgRegister RAX);

      (* write_fd ← (channel>>3)&0xFFFFFFFF *)
      AsmMov (ArgRegister R10, tmp_chan);
      AsmShr (ArgRegister R10, ArgConstant "3");
      AsmMov (ArgRegister R12, ArgConstant "0xFFFFFFFF");
      AsmAnd (ArgRegister R10, ArgRegister R12);

      AsmMov (ArgRegister RAX, ArgConstant "1");      (* write() *)
      AsmMov (ArgRegister RDI, ArgRegister R10);      (* fd      *)
      AsmMov (ArgRegister RSI, ArgRegister RSP);      (* buf ptr *)
      AsmMov (ArgRegister RDX, ArgConstant "8");      (* 8 bytes *)
      AsmSyscall;

      AsmAdd (ArgRegister RSP, ArgConstant "8");
      AsmMov (ArgRegister RAX, tmp_dat);              (* send returns data *)
    ] in
    code_chan @ check_chan @ save_chan @
    code_data @ check_dat  @ save_dat  @ send_logic

  | EReceive chan_expr ->
    let code_chan  = compile_expression env chan_expr in
    let check_chan = check_rax_is_channel () in
    let (tmp_chan, _env1) = allocate_temp_variable env in
    let save_chan  = [AsmMov (tmp_chan, ArgRegister RAX)] in

    let recv_logic = [
      AsmSub (ArgRegister RSP, ArgConstant "8");

      (* read_fd ← channel>>35 *)
      AsmMov (ArgRegister R10, tmp_chan);
      AsmShr (ArgRegister R10, ArgConstant "35");

      AsmMov (ArgRegister RAX, ArgConstant "0");      (* read() *)
      AsmMov (ArgRegister RDI, ArgRegister R10);      (* fd      *)
      AsmMov (ArgRegister RSI, ArgRegister RSP);      (* buf ptr *)
      AsmMov (ArgRegister RDX, ArgConstant "8");      (* 8 bytes *)
      AsmSyscall;

      AsmMov (ArgRegister RAX, ArgMemory (AddrByRegister RSP));
      AsmAdd (ArgRegister RSP, ArgConstant "8");
    ] in
    code_chan @ check_chan @ save_chan @ recv_logic

  | EIf (cond, then_expr, else_expr) ->
    let else_label = fresh_name "if_else" in
    let end_label = fresh_name "if_end" in
    let cond_code = compile_expression env cond in
    let cond_check  = check_rax_is_bool () in

    let then_code = compile_expression env then_expr in

    let else_code = compile_expression env else_expr in

    let test_code = [
      AsmMov(ArgRegister R11, ArgConstant"0xFFFFFFFFFFFFFFFF");
      AsmCmp (ArgRegister RAX, ArgRegister R11);
      AsmJne else_label
    ] in
    let then_jump = [AsmJmp end_label] in
    let else_label_code = [AsmLabel else_label] in
    let end_label_code = [AsmLabel end_label] in
    
    cond_code @ cond_check @ test_code @ then_code @ then_jump @ else_label_code @ else_code @ end_label_code
  
  | EAppl (left_expr, right_expr) -> 
    let code_left = compile_expression env left_expr in (*func/ closure*)
    let check_closure = check_rax_is_closure() in
    let (temp_left, env2) = allocate_temp_variable env in 
    let save_left = [AsmMov (temp_left, ArgRegister RAX)] in

    let code_right = compile_expression env2 right_expr in (*argument*)
    let (temp_right, _env3) = allocate_temp_variable env2 in
    let save_right = [AsmMov (temp_right, ArgRegister RAX)] 

    in 
    code_left @ check_closure @ save_left @ code_right @ save_right @ 
    compile_call_sequence temp_left temp_right

  | EParallel expr_clo ->
    let code_clo      = compile_expression env expr_clo in
    let check_closure = check_rax_is_closure () in

    let (temp_clo, env1) = allocate_temp_variable env in
    let save_clo         = [AsmMov (temp_clo, ArgRegister RAX)] in
    let (temp_arg, _env2) = allocate_temp_variable env1 in   

    (* allocate 16 bytes scratch for two pipe() results *)
    let alloc_scratch = [AsmSub (ArgRegister RSP, ArgConstant "16")] in

    (* first pipe  → [RSP] *)
    let make_pipe1 = [
      AsmMov (ArgRegister RAX, ArgConstant "22");      (* pipe() *)
      AsmMov (ArgRegister RDI, ArgRegister RSP);       (* buf1   *)
      AsmSyscall;
    ] in
    (* second pipe → [RSP+8] *)
    let make_pipe2 = [
      AsmMov (ArgRegister RAX, ArgConstant "22");      (* pipe() *)
      AsmMov (ArgRegister RDI, ArgRegister RSP);
      AsmAdd (ArgRegister RDI, ArgConstant "8");
      AsmSyscall;
    ] in

    let lbl_child = fresh_name "par_child" in
    let lbl_done  = fresh_name "par_done"  in
    let fork_and_split = [
      AsmMov (ArgRegister RAX, ArgConstant "57");    (* fork() *)
      AsmSyscall;
      AsmCmp (ArgRegister RAX, ArgConstant "0");
      AsmJe  lbl_child;                              (* 0 → child *)
      (* ---------- parent ---------- *)
      (* load read1 / write2 *)
      AsmMov (ArgRegister RDX, ArgMemory (AddrByRegister RSP));   
      AsmMov (ArgRegister R12, ArgConstant "0xFFFFFFFF");       (* combo1 *)
      AsmAnd (ArgRegister RDX, ArgRegister R12);                (* read1  *)

      AsmMov (ArgRegister RAX, ArgMemory (AddrByRegisterOffset(RSP,8))); (* combo2 *)
      AsmShr (ArgRegister RAX, ArgConstant "32");                         (* write2 *)
    ] @ build_channel_in_rax @ [
      AsmAdd (ArgRegister RSP, ArgConstant "16");     (* pop scratch *)
      AsmJmp lbl_done;
      (* ---------- child ---------- *)
      AsmLabel lbl_child;
      (* load read2 / write1 *)
      AsmMov (ArgRegister RDX, ArgMemory (AddrByRegisterOffset(RSP,8))); (* combo2 *)
      AsmMov (ArgRegister R12, ArgConstant "0xFFFFFFFF");       (* combo1 *)
      AsmAnd (ArgRegister RDX, ArgRegister R12);                 (* read2  *)

      AsmMov (ArgRegister RAX, ArgMemory (AddrByRegister RSP));          (* combo1 *)
      AsmShr (ArgRegister RAX, ArgConstant "32");                         (* write1 *)
    ] @ build_channel_in_rax @ [
      AsmMov (temp_arg, ArgRegister RAX);           (* arg = channel *)
      AsmAdd (ArgRegister RSP, ArgConstant "16");   (* pop scratch *)
    ] @ compile_call_sequence temp_clo temp_arg @ [
      AsmMov (ArgRegister RDI, ArgConstant "0");    (* exit(0) flushes *)
      AsmCall "exit";
    ] @ [
      AsmLabel lbl_done
    ] in code_clo @ check_closure @ save_clo @ alloc_scratch @ make_pipe1 @ make_pipe2
      @ fork_and_split


  | ETuple exprs ->
    let n = List.length exprs in
    if n = 0 then
      [ AsmMov (ArgRegister RDI, ArgConstant "5");
        AsmCall "stopWithError" ]
    else
      let size_bytes = (n + 1) * 8 in
      let (eval_instrs, temp_addrs, _) =
        List.fold_left (fun (instrs, addrs, current_env) expr ->
          let expr_code = compile_expression current_env expr in
          let (temp_addr, next_env) = allocate_temp_variable current_env in
          (instrs @ expr_code @ [AsmMov(temp_addr, ArgRegister RAX)], addrs @ [temp_addr], next_env) 
        ) ([], [], env) exprs 
      in
      
      let populate_instrs = [
        AsmMov(ArgRegister R11, ArgMemory(AddressByLabel("heap_cursor")));       
        AsmAdd(ArgRegister R11, ArgConstant(string_of_int size_bytes));               
        AsmMov(ArgMemory(AddressByLabel("heap_cursor")), ArgRegister R11); 
        AsmSub(ArgRegister R11, ArgConstant(string_of_int size_bytes));
        AsmMov(ArgMemory(AddrByRegister R11), ArgConstant(string_of_int n))       
      ] in
      let store_elements = List.mapi (fun i temp_addr -> [
          AsmMov(ArgRegister RAX, temp_addr);
          AsmMov(ArgMemory(AddrByRegisterOffset(R11, (i + 1) * 8)), ArgRegister RAX)
        ]) temp_addrs |> List.flatten 
      in
      let return_ptr = [
        AsmMov(ArgRegister RAX, ArgRegister R11);
        AsmOr(ArgRegister RAX, ArgConstant "1")
      ] in
      eval_instrs @ populate_instrs @ store_elements @ return_ptr
      
let generate_closure_data (name : string) (param_count : int) : string =
  sprintf "align 8\nclosure_of_%s:\n  dq 0x8000000000000000, %d, %s\n"
    name param_count name

let rec stack_memory_of_instruction (instr : instruction) : int =
  match instr with
  | AsmMov (ArgMemory (AddrByRegisterOffset (RBP, offset)), _)
  | AsmMov (_, ArgMemory (AddrByRegisterOffset (RBP, offset))) -> abs offset  
  | AsmPush _ -> 8  
  | AsmPop _ -> -8  
  | _ -> 0  
;;


let max_stack_memory_usage (instrs : instruction list) : int =
  let rec helper instrs max_val =
    match instrs with
    | [] -> max_val
    | hd :: tl ->
        let stack_usage = stack_memory_of_instruction hd in
        let new_max = max max_val stack_usage in
        helper tl new_max
  in
  helper instrs 0  
;;

let compile_function_declaration (func_name : string) (params : string list) (body : expr) : instruction list =
  let param_env = 
    let rec add_params params env offset =
      match params with
      | [] -> env
      | param :: rest ->
          let param_addr = ArgMemory(AddrByRegisterOffset(RBP, 8 * (offset + 2))) in
          let new_env = Map.String.add param param_addr (snd env) in
          add_params rest (fst env, new_env) (offset + 1)
    in
    add_params params empty_environment 0
  in
  let body_instrs = compile_expression param_env body in
  let max_stack = max_stack_memory_usage body_instrs in

  (* Align max_stack to 16 bytes after accounting for pushed registers (32 bytes) *)
  let aligned_max_stack = 
    if max_stack > 0 then 
      let total = max_stack + 32 in  (* 4 registers * 8 bytes = 32 *)
      if total mod 16 = 0 then max_stack
      else max_stack + (16 - (total mod 16))
    else 0
  in

  let prologue = [
    AsmLabel(func_name);
    AsmPush(ArgRegister RBP);
    AsmMov(ArgRegister RBP, ArgRegister RSP);
    AsmSub(ArgRegister RSP, ArgConstant (string_of_int aligned_max_stack));
    AsmPush(ArgRegister R12);
    AsmPush(ArgRegister R13);
    AsmPush(ArgRegister R14);
    AsmPush(ArgRegister R15);
  ] in

  let epilogue = [
    AsmPop(ArgRegister R15);
    AsmPop(ArgRegister R14);
    AsmPop(ArgRegister R13);
    AsmPop(ArgRegister R12);
    AsmAdd(ArgRegister RSP, ArgConstant (string_of_int aligned_max_stack));
    AsmMov(ArgRegister RSP, ArgRegister RBP);
    AsmPop(ArgRegister RBP);
    AsmRet
  ] in

  prologue @ body_instrs @ epilogue

let compile_program (p : program) : string =
  match p with
  | Program (decls, main_expr) ->
    let closure_data = List.map (fun decl ->
      match decl with
      | DFunction (name, params, _) ->
          generate_closure_data name (List.length params)
    ) decls |> String.concat "\n"
    in
    
    (* First add all function names to the environment *)
    let env_with_funcs = List.fold_left (fun env decl ->
      match decl with
      | DFunction (name, _, _) -> 
          let (_, dict) = env in
          (fst env, Map.String.add name (ArgLabelOffset("closure_of_" ^ name, 1)) dict)
    ) empty_environment decls in
    
    (* Compile function declarations *)
    let func_instrs = List.flatten (List.map (fun decl ->
      match decl with
      | DFunction (name, params, body) ->
          compile_function_declaration name params body
    ) decls) in

    (* Rest of the compilation code remains the same *)
    let main_body = compile_expression env_with_funcs main_expr in
    let max_stack = max_stack_memory_usage main_body in

    (* Stack allocation for temporaries in main *)
    let stack_allocation =
      if max_stack > 0 then [AsmSub(ArgRegister RSP, ArgConstant (string_of_int max_stack))] else []
    in

    let stack_deallocation =
      if max_stack > 0 then [AsmAdd(ArgRegister RSP, ArgConstant (string_of_int max_stack))] else []
    in

    (* Main prologue and epilogue with stack handling *)
    let main_prologue = [
      AsmLabel("bird_main");
      AsmPush(ArgRegister RBP);
      AsmMov(ArgRegister RBP, ArgRegister RSP);
      AsmPush(ArgRegister R12);
      AsmPush(ArgRegister R13);
      AsmPush(ArgRegister R14);
      AsmPush(ArgRegister R15);
    ] @ stack_allocation @ [
      AsmMov(ArgMemory(AddressByLabel("heap_cursor")), ArgRegister RDI)
    ] in

    let main_epilogue = stack_deallocation @ [
      AsmPop(ArgRegister R15);
      AsmPop(ArgRegister R14);
      AsmPop(ArgRegister R13);
      AsmPop(ArgRegister R12);
      AsmMov(ArgRegister RSP, ArgRegister RBP);
      AsmPop(ArgRegister RBP);
      AsmRet
    ] in

    (* Combine all instructions *)
    let all_instrs = func_instrs @ main_prologue @ main_body @ main_epilogue in
    (* Emit final assembly *)
    "section .data\n" ^
    "align 8\n" ^
    "heap_cursor:\n" ^
    "    dq 0\n" ^
    "big_const_0x8000000000000000:\n" ^
    "    dq 0x8000000000000000\n" ^
    "section .text\n" ^
    closure_data ^ "\n" ^
    "extern stopWithError\n" ^
    "extern printValue\n" ^
    "extern exit\n" ^  
    "global bird_main\n\n" ^
    code_of_instruction_list all_instrs

let compile_to_assembly_code (p : program) : string =
  try
    check_well_formed p;
    compile_program p
  with
    | IllFormed errors ->
      raise (BuildFailure (String.concat "\n" errors))