(* compiler that has increment, decrement, binary +, - and * *)
(* also let bindings - can bind multiple vars at a time *)

open Printf
open Expr
open Asm

(*** functions to help us deal with the stack ***)

(* environment that maps variables to stack indices which store their value *)
type tenv = (string * int) list

(* function to find if a variable is in the environment *)
let rec find (env: tenv) (x: string) : int option = 
   match env with 
   | [] -> None
   | (y, i) :: rest -> 
     if y = x then Some(i) else find rest x
;;

(*** Actual compiler starts here  ***)  

(* code generation for the AST *)
(* takes in the environment and stack index  *)
let rec expr_to_instrs (e: expr) (env: tenv) (si: int) : instr list = 
  match e with
  (* just move the value into register rax *)
  | ENum(i) -> [IMov(Const(i), Reg(Rax))]
  | EId(x) -> (* lookup the variable in the environment *)
              (match find env x with
              (* if can't find it, compiler error *)
              | None -> failwith ("Unbound variable identifier " ^ x)
              (* i represent the offset from the stack *)
              (* move from that location to rax *)
              | Some(i) -> [IMov(stackloc i,Reg(Rax))]) 
  | EUnop(op, e1) -> unop_helper op e1 env si
  | EBinop(op, e1, e2) -> binop_helper op e1 e2 env si 
  | ELet(binding, expr) -> let_helper binding expr env si
  
  and let_helper (binding : (string * expr) list) (expr : expr) (env: tenv) (si: int) : instr list =
    (* stores value in rax onto the stack *)
    let store : instr list =
      [IMov(Reg(Rax), stackloc si)] in
    match binding with
    | [] -> expr_to_instrs expr env si
    | (str, e1) :: tl -> (* variable with stack location *)
                          let var : (string * int) = (str, si) in
                          (* recurses on expression of the vairbale binding *)
                          (* recurses on rest of binding list as well *)
                          expr_to_instrs e1 (var :: env) (si + 1) @ store 
                          @ let_helper tl expr (var :: env) (si + 2)
                          
  and unop_helper (op: unop) (e': expr) (env: tenv) (si: int) : instr list = 
     let e'_instrs : instr list  = expr_to_instrs e' env si in 
     let op_instr : instr list = 
       match op with 
       | Inc -> [IAdd(Const(1),Reg(Rax))] 
       | Dec -> [ISub(Const(1),Reg(Rax))]  
     in e'_instrs @ op_instr

and binop_helper (op: binop) (e1: expr) (e2: expr) (env: tenv) (si: int) : instr list = 
    let e1_instrs : instr list = expr_to_instrs e1 env si in 
    let e2_instrs : instr list = expr_to_instrs e2 env (si + 1) in
    let store_e1 : instr list = [IMov(Reg(Rax), stackloc si)] in
    let op_instr : instr list =
     match op with
     | Plus -> [IAdd(stackloc si, Reg(Rax))]
     | Minus -> [ISub(Reg(Rax), stackloc si); IMov(stackloc si, Reg(Rax))]
     | Times -> [IMul(stackloc si, Reg(Rax))]
    in
    e1_instrs @ store_e1 @ e2_instrs @ op_instr
;;          

(* compiles an ast to an x86 string *)
let compile (ast: expr) : string = 
  (* generate instructions for the expression *)
  let instrs : instr list = expr_to_instrs ast [] 1 in
  (* make a nice assembly string out of the list of instructions *)
  let instrs_str : string = instrs_to_string (instrs @ [IRet]) in 
  (* add the boilerplate to instructions to make it work *)
  sprintf "
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  %s
  \n" instrs_str
;;




