(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | R10
  | R11
  | RBP
  | RBX
  | R12
  | R13
  | R14
  | R15
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddressByLabel of string
  | AddressByRegisterProductOffset of register * register * int
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * int
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmShl of argument * argument 
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmQOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmJl of string
  | AsmJg of string
  | AsmJge of string 
  | AsmCall of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmSection of string
  | AsmAlign of int
  | AsmDq of string list
  | AsmRepMovsq
  | AsmSyscall          
  | AsmRet
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with 
  | RAX -> "rax"
  | RSP -> "rsp"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | RBP -> "rbp"
  | RDI -> "rdi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | RSI -> "rsi"
  | RBX -> "rbx"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with 
  | AddrByRegister reg -> Printf.sprintf "[%s]" (code_of_register reg)
  | AddrByRegisterOffset (reg, offset) ->
      if offset < 0 then 
        Printf.sprintf "[%s-%d]" (code_of_register reg) (-offset)
      else 
        Printf.sprintf "[%s+%d]" (code_of_register reg) offset
  | AddressByLabel label_name ->  Printf.sprintf "[%s]" label_name
  | AddressByRegisterProductOffset(r1, r2, scale) ->
      Printf.sprintf "[%s+%s*%d]" (code_of_register r1) (code_of_register r2) scale
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with 
  | ArgConstant const -> const
  | ArgRegister reg -> code_of_register reg
  | ArgMemory mem -> code_of_address mem
  | ArgLabelOffset(lbl, off) ->
      Printf.sprintf "%s + %d" lbl off
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
  *)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | AsmAdd (arg1, arg2) -> 
      (match arg1 with
       | ArgMemory _ -> Printf.sprintf "  add qword %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
       | _ -> Printf.sprintf "  add %s, %s\n" (code_of_argument arg1) (code_of_argument arg2))
  | AsmIMul (arg1, arg2) -> 
      (match arg1 with
       | ArgMemory _ -> Printf.sprintf "  imul qword %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
       | _ -> Printf.sprintf "  imul %s, %s\n" (code_of_argument arg1) (code_of_argument arg2))
  | AsmMov (arg1, arg2) -> 
      (match arg1, arg2 with
       | ArgMemory _, _ -> Printf.sprintf "  mov qword %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
       | _, ArgMemory _ -> Printf.sprintf "  mov %s, qword %s\n" (code_of_argument arg1) (code_of_argument arg2)
       | _, ArgConstant const when String.length const >= 2 && String.sub const 0 2 = "0x" && String.length const > 10 ->
           Printf.sprintf "  mov %s, %s\n" (code_of_argument arg1) const
       | _ -> Printf.sprintf "  mov %s, %s\n" (code_of_argument arg1) (code_of_argument arg2))
  | AsmSub (arg1, arg2) -> 
      (match arg1 with
       | ArgMemory _ -> Printf.sprintf "  sub qword %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
       | _ -> Printf.sprintf "  sub %s, %s\n" (code_of_argument arg1) (code_of_argument arg2))
  | AsmShl (arg1, arg2) -> Printf.sprintf "  shl %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmShr (arg1, arg2) -> Printf.sprintf "  shr %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmSal (arg1, arg2) -> Printf.sprintf "  sal %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmSar (arg1, arg2) -> Printf.sprintf "  sar %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmAnd (arg1, arg2) -> Printf.sprintf "  and %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmOr (arg1, arg2) -> Printf.sprintf "  or %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmQOr (arg1, arg2) ->
    Printf.sprintf "  or qword %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmXor (arg1, arg2) -> Printf.sprintf "  xor %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmLabel str -> Printf.sprintf "%s:\n" str
  | AsmCmp (arg1, arg2) -> Printf.sprintf "  cmp %s, %s\n" (code_of_argument arg1) (code_of_argument arg2)
  | AsmJmp str -> Printf.sprintf "  jmp %s\n" str
  | AsmJe str -> Printf.sprintf "  je %s\n" str
  | AsmJne str -> Printf.sprintf "  jne %s\n" str
  | AsmJl str -> Printf.sprintf "  jl %s\n" str
  | AsmJg str -> Printf.sprintf "  jg %s\n" str
  | AsmJge str -> Printf.sprintf "  jge %s\n" str 
  | AsmCall f -> Printf.sprintf "  call %s\n" f
  | AsmPush arg -> Printf.sprintf "  push %s\n" (code_of_argument arg)
  | AsmPop arg -> Printf.sprintf "  pop %s\n" (code_of_argument arg)
  | AsmSection s -> Printf.sprintf "%s\n" s
  | AsmAlign n -> Printf.sprintf "  align %d\n" n
  | AsmDq vals -> Printf.sprintf "  dq %s\n" (String.concat ", " vals)
  | AsmRepMovsq -> "  rep movsq\n"
  | AsmRet -> "  ret\n"
  | AsmSyscall   -> "  syscall\n"  
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let code_of_instruction_list (instruction_list : instruction list) : string =
  String.concat "" (List.map code_of_instruction instruction_list)
;;