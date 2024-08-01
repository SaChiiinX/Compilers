open Printf
open Expr
open Asm

(*** functions to help us deal with the stack ***)

(* environment that maps variables to stack indices which store their value *)
type tenv = (string * int) list

(* function to find if a variable is in either the variable stack environment or the type environment *)
let rec find (env: (string * 'a) list) (x: string) : 'a option = 
   match env with 
   | [] -> None
   | (y, i) :: rest -> 
     if y = x then Some(i) else find rest x

(*** Type Checker **)

(* a type environment is a list of string * typ  pairs tracking variables and their types *)
(* NOTE: this is distinct from the environment that tracks variables and their stack location *)
type typeenv = (string * typ) list 

(* the type checker *)
let rec type_check_expr (e : expr) (env: typeenv) : typ =
  match e with
  | EBool(_) -> TBool
  | ENum(_) -> TNum
  | EId(x) -> (match find env x with
              | None -> failwith ("Unbound variable identifier " ^ x)
              | Some(i) -> i) 
  | EUnop(op, e1) -> is_of_type TNum (type_check_expr e1 env)
  | EBinop(op, e1, e2, elist) -> tbinop_helper (e1 :: e2 :: elist) env 
  | ELet(binding, expr_list) -> tlet_helper binding expr_list env
  | EIf(c,t,f) -> tif_helper c t f env 
  | EComp(comp,e1,e2) -> tcomp_helper comp e1 e2 env 
  | ESet(var, expr) -> tset_helper var expr env
  | EWhile(e1, expr_list) -> twhile_helper e1 expr_list env
  | EDo(e1, expr_list) -> twhile_helper e1 expr_list env
  | EFor((var, expr), delta, cond, expr_list) -> 
                let _ : typ = is_of_type TNum (type_check_expr expr env) in
                tfor_helper delta cond expr_list ((var, TNum) :: env)
  (*| ESwitch(_,_,_) -> failwith "Not Implemented"*)

and tfor_helper (delta: expr) (cond: expr) (expr_list: expr list) (env: typeenv) : typ =
  (* Ensures delta is a set that updates variable in env*)
  let _ : typ =
  match delta with
  | ESet(_,_) -> type_check_expr delta env
  | _ -> failwith "Type Mismatch"
  in
  let _ : typ = check_condition cond env in
  let _ : typ = texpr_list_helper expr_list env in
  TBool 

and tset_helper (var: string) (expr: expr) (env: typeenv) : typ =
  let var_typ : typ = type_check_expr (EId(var)) env in
  let check_expr : typ = type_check_expr expr env in 
  is_of_type var_typ check_expr

and tif_helper (cond: expr) (t: expr) (f: expr) (env: typeenv) : typ = 
  let t_type : typ = type_check_expr t env in
  let f_type : typ = type_check_expr f env in
  let _ : typ = check_condition cond env in
  is_of_type t_type f_type

and twhile_helper (cond: expr) (expr_list: expr list) (env: typeenv) : typ =
  let _ : typ = check_condition cond env in
  let _ : typ = texpr_list_helper expr_list env in  
  TBool

and tlet_helper (binding: (string * expr) list) (expr_list: expr list) (env: typeenv) : typ =
  match binding with
  | [] -> texpr_list_helper expr_list env 
  | (var, e1) :: rest -> (* adds each var type to the enviornment*)
                         let check_expr : typ = type_check_expr e1 env in
                         let tuple : (string * typ) = (var, check_expr) in
                         tlet_helper rest expr_list (tuple :: env)

and tcomp_helper (comp: comp) (e1: expr) (e2: expr) (env: typeenv) : typ =
  let e1_typ : typ = type_check_expr e1 env in
  let e2_typ : typ = type_check_expr e2 env in 
    match comp with
    | Eq -> (is_of_type e1_typ e2_typ)
    | _ -> (is_of_type (is_of_type TNum e1_typ) e2_typ)

and tbinop_helper (elist: expr list) (env: typeenv) : typ =
  match elist with
  | [] -> failwith "Type Mismatch"
  | [expr] -> is_of_type TNum (type_check_expr expr env)
  | expr :: rest -> let _ : typ = is_of_type TNum (type_check_expr expr env) in
                    tbinop_helper rest env

(* Typechecks a lists of expressions 
   Returns type of last expression *)
and texpr_list_helper (expr_list: expr list) (env: typeenv) : typ =
  match expr_list with
  | [] -> failwith "Empty expression list"
  | [expr] -> type_check_expr expr env 
  | expr :: rest -> let _ : typ = type_check_expr expr env in
                    texpr_list_helper rest env

(* Check if conditon is type matched and excludes some while *)
and check_condition (cond: expr) (env: typeenv) : typ =
  let check_expr : typ = type_check_expr cond env in
  match cond with
  | EId(_) -> is_of_type TBool check_expr
  | EBool(_) | EComp(_,_,_) -> check_expr
  | _-> failwith "Type Mismatch"

(* Check if two types are equal*)
and is_of_type (typ1: typ) (typ2: typ) : typ =
  if (typ1 = typ2) then
    typ1 else
    failwith "Type Mismatch"
;; 

(*** Actual compiler starts here  ***)  

(* code generation for the AST *)
let rec expr_to_instrs (e: expr) (env: tenv) (si: int) : instr list = 
  match e with
  | EBool(true) -> [IMov(Const(1), Reg(Rax))]
  | EBool(false) -> [IMov(Const(0), Reg(Rax))]
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
  | EBinop(op, e1, e2, elist) -> binop_helper op e1 (e2 :: elist) env si 
  | ELet(binding, expr_list) -> let_helper binding expr_list env si
  | EIf(c,t,f) -> if_helper c t f env si
  | EComp(comp,e1,e2) -> comp_helper comp e1 e2 env si
  | ESet(str, expr) -> set_helper str expr  env si
  | EWhile(expr, expr_list) -> while_helper expr expr_list env si true
  | EDo(expr, expr_list) -> while_helper expr expr_list env si false
  | EFor(var, delta, cond, expr_list) -> for_helper var delta cond expr_list env si
  
  (*| ESwitch(_,_,_) -> failwith "Not Implemented"

  and switch_helper (var: string) (elist_list: (expr * expr list) list) (elist: expr list) (env: tenv) (si: int)
  : instr list =
  let var_instr : instr list = expr_to_instrs (EId(var)) env si in
  let end_string : string = new_label "end" in
  let default_string : string = new_label "default" in
  let end_label : instr list = [ILab(end_string)] in
  let default_label : instr list = [ILab(default_string)] in
  var_instr @ elist_list_helper elist_list @ default_label @ seq_helper elist env (si + 1) @ end_label 

  and elist_list_helper (elist_list: (expr * expr list) list) (end_label: string) (env: tenv) (si: int) : instr list =
    match elist_list with
    | [] ->  []
    | (expr, elist) :: rest ->  let case_stirng : string = new_label "case" in
                                let case_label : instr list = [ILab(case_stirng)] in
                                case_label @ [expr_to_instrs expr env si] @ elist_helper elist end_label env (si + 1) 
                                @ elist_list_helper rest end_label env (si + 2) 

  and elist_helper (elist: expr list) (end_label: string) (env: tenv) (si: int) : instr list = 
    match elist with
    | [] -> []
    | [EBreak] -> [IJmp end_label]
    | expr :: rest -> [expr_to_instrs expr env si] @ elist_helper rest end_label env (si + 1) *)

  and for_helper (var: (string * expr)) (delta: expr) (cond: expr) (expr_list: expr list) (env: tenv) (si: int)
  : instr list =
  let new_elist : expr list = expr_list @ [delta] in
  let loop : expr = EWhile(cond, new_elist) in
  let built_let : expr = ELet([var], loop :: expr_list) in
  expr_to_instrs built_let env si

  and set_helper (str: string) (expr: expr) (env: tenv) (si: int) : instr list =
    let expr_instr : instr list = expr_to_instrs expr env si in
    let store : instr list =
    match find env str with
    | None -> failwith ("Unbound vairable identifier " ^ str)
    | Some(i) -> [IMov(Reg(Rax), stackloc i)]
    in
    expr_instr @ store
  
  and while_helper (cond: expr) (expr_list: expr list) (env: tenv) (si: int) (is_while: bool) : instr list =
    let cond_instrs : instr list = expr_to_instrs cond env si in
    let cond_string : string = new_label "cond" in
    let break_string : string = new_label "break" in
    let cond_label : instr list = [ILab(cond_string)] in
    let break_label : instr list = [ILab(break_string)] in
    let expr_list_instrs : instr list = seq_helper expr_list env (si + 1) in
    let condjump : instr list = 
      if (is_while) then
      [IJmp(cond_string)] else
      [IJne(cond_string)] in
    let breakjump : instr list = [IJe(break_string)] in
    let comp : instr list = [ICmp(Const(0),Reg(Rax))] in
    if (is_while) then
    cond_label @ cond_instrs @ comp @ breakjump @ expr_list_instrs @ condjump @ break_label else
      cond_label @ expr_list_instrs @ cond_instrs @ comp @ condjump

and comp_helper (comp: comp) (e1: expr) (e2: expr) (env: tenv) (si: int): instr list = 
  let e1_instrs : instr list = expr_to_instrs e1 env si in 
  let e2_instrs : instr list = expr_to_instrs e2 env (si + 1) in 
  let store_e1 : instr list = [IMov(Reg(Rax), stackloc si)] in
  let comp_instrs : instr list = [ICmp(Reg(Rax), stackloc si)] in
  let t_instrs : instr list = [IMov(Const(1), Reg(Rax))] in 
  let f_instrs : instr list = [IMov(Const(0), Reg(Rax))] in 
  let both : string = new_label "both" in 
  let both_label : instr list = [ILab(both)] in 
  let uptojump : instr list = e1_instrs @ store_e1 @ e2_instrs 
               @ comp_instrs @ f_instrs in 
  let afterjump : instr list = t_instrs @ both_label in 
  let jump : instr list = 
      match comp with 
      | Eq -> [IJne(both)]
      | Less -> [IJge(both)] 
      | Great -> [IJle(both)] 
  in uptojump @ jump @ afterjump 

and if_helper (c: expr) (t: expr) (f: expr) (env: tenv) (si: int) : instr list = 
  let cond_instrs : instr list = expr_to_instrs c env si in 
  let t_instrs : instr list = expr_to_instrs t env si in 
  let f_instrs : instr list = expr_to_instrs f env si in 
  let e_string : string  = new_label "else" in
  let b_string : string = new_label "both" in  
  let e_label : instr list = [ILab(e_string)] in
  let b_label : instr list = [ILab(b_string)] in
  let comp : instr list = [ICmp(Const(0),Reg(Rax))] in
  let both_jump : instr list = [IJmp(b_string)] in
  let else_jump : instr list = [IJe(e_string)] in
  cond_instrs @ comp @ else_jump @ t_instrs @ both_jump @ 
      e_label @ f_instrs @ b_label 

and let_helper (binding : (string * expr) list) (expr_list : expr list) (env: tenv) (si: int) : instr list =
  (* stores value in rax onto the stack *)
  let store : instr list =
    [IMov(Reg(Rax), stackloc si)] in
  match binding with
  | [] -> seq_helper expr_list env si
  | (str, e1) :: rest -> (* variable with stack location *)
                        let var : (string * int) = (str, si) in
                        (* recurses on expression of the vairbale binding *)
                        (* recurses on rest of binding list as well *)
                        expr_to_instrs e1 (var :: env) si @ store 
                        @ let_helper rest expr_list (var :: env) (si + 1)

(* go through and generate instructions for each expression in a list *) 
and seq_helper (body_list: expr list) (env: tenv) (si: int) : instr list =
   match body_list with
   | [] -> []
   | expr :: rest -> expr_to_instrs expr env si @ seq_helper rest env (si + 1) 

and unop_helper (op: unop) (e': expr) (env: tenv) (si: int) : instr list = 
(* recurses on e'*)
let e'_instrs : instr list  = expr_to_instrs e' env si in 
(* specialize assembly instruction to operation *)
let op_instr : instr list = 
  match op with 
  | Inc -> [IAdd(Const(1),Reg(Rax))] 
  | Dec -> [ISub(Const(1),Reg(Rax))]
(* combines instructions created *)  
in e'_instrs @ op_instr
  
and binop_helper (op: binop) (expr: expr) (elist: expr list) (env: tenv) (si: int) : instr list =
  let e_instrs : instr list = expr_to_instrs expr env (si + 1) in
  let store : instr list = [IMov(Reg(Rax), stackloc si)] in
  let op_1 : instr list =
  match op with
  | Plus | Minus-> [IMov(Const(0), stackloc si);IAdd(stackloc si, Reg(Rax))]
  | Times -> [IMov(Const(1), stackloc si);IMul(stackloc si, Reg(Rax))]
  in
  let op_2 : instr list =
  match op with 
  | Plus -> [IAdd(stackloc si, Reg(Rax))]
  | Minus -> [ISub(stackloc si, Reg(Rax))]
  | Times -> [IMul(stackloc si, Reg(Rax))]
  in
  let elist_instrs : instr list = binop_list_helper elist store op_1 env si in
  elist_instrs @ e_instrs @ op_2

and binop_list_helper (elist: expr list) (store: instr list) (op_1: instr list) (env: tenv) (si: int) : instr list =
  match elist with
  | [] -> []
  | [expr] -> (expr_to_instrs expr env si) @ op_1 @ store
  | expr :: rest -> (expr_to_instrs expr env si) @ op_1 @ store @ (binop_list_helper rest store op_1 env si)
;;          

(* TODO: implement me *)

(* compiles an ast to an x86 string *)
let compile (ast: expr) : string = 
  (* before we generate the instructions, typecheck the code with an empty type environment *)
  let _  = type_check_expr ast [] in
  (* generate instructions for the AST *)
  let instrs : instr list = expr_to_instrs ast [] 1 in
  (* make instructions into a string *)
  let instrs_str : string = instrs_to_string (instrs @ [IRet]) in 
  (* add the boilerplate to instructions to make it work *)
  sprintf "
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  %s
  \n" instrs_str
;;