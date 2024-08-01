(* file that defines types to represent the assembly *)
(* also contains functions to convert the assembly to strings *)
open Printf

(* registers *)
type reg =
   | Rax
   | Rsp

(* arguments to instructions like add or mov *)
type arg =
   | Const of int
   | Reg of reg
   | RegOffset of int * reg

(* type for assembly instructions *)
type instr =
   (* mov: source, dest *)
   | IMov of arg * arg
   (* add: to-add, dest *)
   | IAdd of arg * arg
   (* sub: to-sub, dest *)
   | ISub of arg * arg
   (* mul: to-mul, dest *)
   | IMul of arg * arg
   (* return *)
   | IRet

(* calculates the offset from the stack pointer *)
let stackloc (i: int) : arg = RegOffset(-8 * i, Rsp)

(* -----------------------------------------------------------*)

(* HELPER FUNCTIONS TO TURN ASSEMBLY INTO STRINGS *)

(* turn a register into a string *)
let reg_to_string (r: reg) : string =
   match r with
   | Rax -> "%rax"
   | Rsp -> "%rsp"
;;

(* turn an argument to an instruction into a string *)
let arg_to_string (a: arg) : string =
   match a with
   | Const(n) -> sprintf "$%d" n
   | Reg(r) -> reg_to_string r
   | RegOffset(n,r) -> sprintf "%d(%s)" n (reg_to_string r)
;;

(* turn an instruction into a string *)
let instr_to_string (i:instr) : string =
   match i with
   | IMov(s,d) -> sprintf "mov %s, %s" (arg_to_string s) (arg_to_string d)
   | IAdd(to_add, dest) -> sprintf "add %s, %s" (arg_to_string to_add) (arg_to_string dest)
   | ISub(to_sub, dest) -> sprintf "sub %s, %s" (arg_to_string to_sub) (arg_to_string dest)
   | IMul(to_mul, dest) -> sprintf "imul %s, %s" (arg_to_string to_mul) (arg_to_string dest)
   | IRet -> "ret"
;;

(* converts list of instructions into one nice assembly string *)
let rec instrs_to_string (is: instr list) : string =
   match is with
   | [] -> ""
   | hd :: tl -> (instr_to_string hd) ^ "\n  "  ^ (instrs_to_string tl)
;;

