(* compiler that has increment, decrement, binary +, - and * *)
(* if, comparisons, booleans *)

open Printf
open Expr
open Asm 

(* an association list to keep track of the variable environment - maps strings to ints *)
type tenv = (string * int) list

(* function to find if a variable is in either the variable stack environment or the type environment *)
let rec find (env: (string * 'a) list) (x: string) : 'a option = 
   match env with 
   | [] -> None
   | (y, i) :: rest -> 
     if y = x then Some(i) else find rest x
;;

let rec find_def (defs: def list) (d: string) : def option = 
   match defs with 
   | [] -> None
   | DFun(fname, arglist, typ, body) :: rest -> 
           if fname = d then Some(DFun(fname, arglist, typ, body))
           else find_def rest d
 ;;
 
 (*** Type Checker **)

(* a type environment is a list of string * typ  pairs tracking variables and their types *)
(* NOTE: this is distinct from environment that tracks variables and their stack location *)
type typeenv = (string * typ) list 

(* function that type checks an expression *)
(* either returns a type or fails with a Type Mismatch error *)
let rec type_check_expr (e : expr) (env: typeenv) (defs: def list) : typ =
  match e with
  | EBool(_) -> TBool
  | ENum(_) -> TNum
  | EId(x) -> (match find env x with
              | None -> failwith ("Unbound variable identifier " ^ x)
              | Some(i) -> i) 
  | EUnop(op, e1) -> is_of_type TNum (type_check_expr e1 env defs)
  | EBinop(op, elist) -> tbinop_helper elist env defs
  | ELet(binding, expr_list) -> tlet_helper binding expr_list env defs
  | EIf(c,t,f) -> tif_helper c t f env defs
  | EComp(comp,e1,e2) -> tcomp_helper comp e1 e2 env defs 
  | ESet(var, expr) -> tset_helper var expr env defs
  | EWhile(e1, expr_list) -> twhile_helper e1 expr_list env defs
  | EDo(e1, expr_list) -> twhile_helper e1 expr_list env defs
  | EFor((var, expr), delta, cond, expr_list) -> 
                let _ : typ = is_of_type TNum (type_check_expr expr env defs) in
                tfor_helper delta cond expr_list ((var, TNum) :: env) defs
  | EApp(fname, elist) -> tapp_helper fname elist env defs 

and tapp_helper (fname: string) (elist: expr list) (env: typeenv) (defs: def list) : typ =  
  match find_def defs fname with
  | None -> failwith ("Unbound function " ^ fname)
  | Some(DFun(fname, arglist, typ, body)) -> 
    let (_,typlist) : (string list * typ list) = List.split arglist in
    let typlist2 = List.map (fun e -> type_check_expr e env defs) elist in
    if List.length elist <> List.length arglist then
      failwith "Type Mismatch"
    else if typlist = typlist2 then
      typ else
    failwith "Type Mismatch"

and tfor_helper (delta: expr) (cond: expr) (expr_list: expr list) (env: typeenv) (defs: def list) : typ =
  (* Ensures delta is a set that updates variable in env*)
  let _ : typ =
  match delta with
  | ESet(_,_) -> type_check_expr delta env defs
  | _ -> failwith "Type Mismatch"
  in
  let _ : typ = check_condition cond env defs in
  let _ : typ = texpr_list_helper expr_list env defs in
  TBool 

and tset_helper (var: string) (expr: expr) (env: typeenv) (defs: def list) : typ =
  let var_typ : typ = type_check_expr (EId(var)) env defs in
  let check_expr : typ = type_check_expr expr env defs in 
  is_of_type var_typ check_expr

and tif_helper (cond: expr) (t: expr) (f: expr) (env: typeenv) (defs: def list) : typ = 
  let t_type : typ = type_check_expr t env defs in
  let f_type : typ = type_check_expr f env defs in
  let _ : typ = check_condition cond env defs in
  is_of_type t_type f_type

and twhile_helper (cond: expr) (expr_list: expr list) (env: typeenv) (defs: def list) : typ =
  let _ : typ = check_condition cond env defs in
  let _ : typ = texpr_list_helper expr_list env defs in  
  TBool

and tlet_helper (binding: (string * expr) list) (expr_list: expr list) (env: typeenv) (defs: def list) : typ =
  match binding with
  | [] -> texpr_list_helper expr_list env defs
  | (var, e1) :: rest -> (* adds each var type to the enviornment*)
                         let check_expr : typ = type_check_expr e1 env defs in
                         let tuple : (string * typ) = (var, check_expr) in
                         tlet_helper rest expr_list (tuple :: env) defs 

and tcomp_helper (comp: comp) (e1: expr) (e2: expr) (env: typeenv) (defs: def list) : typ =
  let e1_typ : typ = type_check_expr e1 env defs in
  let e2_typ : typ = type_check_expr e2 env defs in 
    match comp with
    | Eq -> (is_of_type e1_typ e2_typ)
    | _ -> (is_of_type (is_of_type TNum e1_typ) e2_typ)

and tbinop_helper (elist: expr list) (env: typeenv) (defs: def list) : typ =
  match elist with
  | [] -> failwith "Type Mismatch"
  | [expr] -> is_of_type TNum (type_check_expr expr env defs)
  | expr :: rest -> let _ : typ = is_of_type TNum (type_check_expr expr env defs) in
                    tbinop_helper rest env defs

(* Typechecks a lists of expressions 
   Returns type of last expression *)
and texpr_list_helper (expr_list: expr list) (env: typeenv) (defs: def list) : typ =
  match expr_list with
  | [] -> failwith "Empty expression list"
  | [expr] -> type_check_expr expr env defs
  | expr :: rest -> let _ : typ = type_check_expr expr env defs in
                    texpr_list_helper rest env defs

(* Check if conditon is type matched and excludes some while *)
and check_condition (cond: expr) (env: typeenv) (defs: def list) : typ =
  let check_expr : typ = type_check_expr cond env defs in
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

(* function to type check definitions *)
(* returns true if function typechecks or fails with a Type Mismatch error *)
let type_check_def (d: def) (defs: def list) : bool = 
    let DFun(fname, arglist, typ, body) = d in
    let ret : typ = type_check_expr body arglist defs in
    if ret = typ then true else failwith "Type Mismatch"
;;

(* function to type check whole program *)
let type_check_prog (body: expr) (defs: def list): typ = 
    (* first type check every definition in the list of definitions *)
    (* this should produce a list of true expressions *)
    (* we don't actually need the result *)
    let _ = List.map (fun d -> type_check_def d defs) defs in
    (* since the definitions typechecked, now typecheck the body expression *)
    (* the types of function arguments are contained only within those definitions *)
    (* from the perspective of the body expression, no types have been defined *)
    (* this will return a typ, which is the type of the program *)
    type_check_expr body [] defs
;;

(*** Actual compiler starts here  ***)  

(* code generation for the AST *)
(* now taking in an environment to track variable's stack position *)
(* also taking in a stack index to make sure no information gets clobbered *)
let rec expr_to_instrs (e: expr) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list = 
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
   | EUnop(op, e1) -> unop_helper op e1 env si defs false
   | EBinop(op, elist) -> binop_helper op elist env si defs false
   | ELet(binding, expr_list) -> let_helper binding expr_list env si defs is_tc
   | EIf(c,t,f) -> if_helper c t f env si defs is_tc
   | EComp(comp,e1,e2) -> comp_helper comp e1 e2 env si defs false
   | ESet(str, expr) -> set_helper str expr  env si defs false
   | EWhile(expr, expr_list) -> while_helper expr expr_list env si true defs false
   | EDo(expr, expr_list) -> while_helper expr expr_list env si false defs false
   | EFor(var, delta, cond, expr_list) -> for_helper var delta cond expr_list env si defs false
   | EApp(fname, elist) -> app_helper fname elist env si defs is_tc
   
   and move_args (si: int) (length: int) (count: int) : instr list =
      if length = count then
        [] else
      [IMov(stackloc (si - 1), Reg(Rax)); IMov(Reg(Rax), stackloc count)]
      @ move_args (si - 1) length (count + 1)

  and arglist_helper (elist: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
    match elist with
    | [] -> if is_tc then move_args (si - (List.length elist)) (List.length elist + 3) 2 else []
    | expr :: rest -> let arg_instrs : instr list = expr_to_instrs expr env si defs is_tc in 
                      let move_arg : instr list = [IMov(Reg(Rax),stackloc si)] in
                      arg_instrs @ move_arg @ arglist_helper rest env (si + 1) defs is_tc

  and app_helper (fname: string) (elist: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
    match find_def defs fname with 
    | None -> failwith "Undefined function"
    | Some(DFun(f, arglist, typ, body)) -> 
    (* Not handling tailer recursive cases *)
    if is_tc then
      let arglist_instrs : instr list = arglist_helper elist env si defs is_tc in 
      let jump : instr list = [IJmp(fname)] in
      arglist_instrs @ jump
    else
      let after : string = new_label "after" in 
      let after_lab : instr list = [ILab(after)] in 
      let label_move : instr list = 
                [IMov(Lab(after), Reg(Rax));IMov(Reg(Rax), stackloc si)] in
      let old_rsp : instr list = [IMov(Reg(Rsp), stackloc (si + 1))] in
      let arglist_instrs : instr list = arglist_helper elist env (si + 2) defs is_tc in 
      let update_rsp : instr list = [ISub(Const(si * 8), Reg(Rsp))] in 
      let jump : instr list = [IJmp(f)] in
      let restore_rsp : instr list = [IMov(RegOffset(-16, Rsp), Reg(Rsp))] in
      label_move @ old_rsp @ arglist_instrs @ update_rsp @ jump @ after_lab @ restore_rsp   

  and for_helper (var: (string * expr)) (delta: expr) (cond: expr) (expr_list: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool)
  : instr list =
  let new_elist : expr list = expr_list @ [delta] in
  let loop : expr = EWhile(cond, new_elist) in
  let built_let : expr = ELet([var], loop :: expr_list) in
  expr_to_instrs built_let env si defs is_tc

  and set_helper (str: string) (expr: expr) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
    let expr_instr : instr list = expr_to_instrs expr env si defs is_tc in
    let store : instr list =
    match find env str with
    | None -> failwith ("Unbound vairable identifier " ^ str)
    | Some(i) -> [IMov(Reg(Rax), stackloc i)]
    in
    expr_instr @ store
  
  and while_helper (cond: expr) (expr_list: expr list) (env: tenv) (si: int) (is_while: bool) (defs: def list) (is_tc: bool) : instr list =
    let cond_instrs : instr list = expr_to_instrs cond env si defs is_tc in
    let cond_string : string = new_label "cond" in
    let break_string : string = new_label "break" in
    let cond_label : instr list = [ILab(cond_string)] in
    let break_label : instr list = [ILab(break_string)] in
    let expr_list_instrs : instr list = seq_helper expr_list env (si + 1) defs is_tc in
    let condjump : instr list = 
      if (is_while) then
      [IJmp(cond_string)] else
      [IJne(cond_string)] in
    let breakjump : instr list = [IJe(break_string)] in
    let comp : instr list = [ICmp(Const(0),Reg(Rax))] in
    if (is_while) then
    cond_label @ cond_instrs @ comp @ breakjump @ expr_list_instrs @ condjump @ break_label else
      cond_label @ expr_list_instrs @ cond_instrs @ comp @ condjump

and comp_helper (comp: comp) (e1: expr) (e2: expr) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list = 
  let e1_instrs : instr list = expr_to_instrs e1 env si defs is_tc in 
  let e2_instrs : instr list = expr_to_instrs e2 env (si + 1) defs is_tc in 
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

and if_helper (c: expr) (t: expr) (f: expr) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list = 
  let cond_instrs : instr list = expr_to_instrs c env si defs false in 
  let t_instrs : instr list = expr_to_instrs t env si defs is_tc in 
  let f_instrs : instr list = expr_to_instrs f env si defs is_tc in 
  let e_string : string  = new_label "else" in
  let b_string : string = new_label "both" in  
  let e_label : instr list = [ILab(e_string)] in
  let b_label : instr list = [ILab(b_string)] in
  let comp : instr list = [ICmp(Const(0),Reg(Rax))] in
  let both_jump : instr list = [IJmp(b_string)] in
  let else_jump : instr list = [IJe(e_string)] in
  cond_instrs @ comp @ else_jump @ t_instrs @ both_jump @ 
      e_label @ f_instrs @ b_label 

and let_helper (binding : (string * expr) list) (expr_list : expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
  (* stores value in rax onto the stack *)
  let store : instr list =
    [IMov(Reg(Rax), stackloc si)] in
  match binding with
  | [] -> seq_helper expr_list env si defs is_tc
  | (str, e1) :: rest -> (* variable with stack location *)
                        let var : (string * int) = (str, si) in
                        (* recurses on expression of the vairbale binding *)
                        (* recurses on rest of binding list as well *)
                        expr_to_instrs e1 (var :: env) si defs false @ store 
                        @ let_helper rest expr_list (var :: env) (si + 1) defs is_tc

(* go through and generate instructions for each expression in a list *) 
and seq_helper (body_list: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
   match body_list with
   | [] -> []
   | [expr] -> expr_to_instrs expr env si defs is_tc
   | expr :: rest -> expr_to_instrs expr env si defs false @ seq_helper rest env (si + 1) defs is_tc

and unop_helper (op: unop) (e': expr) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list = 
(* recurses on e'*)
let e'_instrs : instr list  = expr_to_instrs e' env si defs is_tc in 
(* specialize assembly instruction to operation *)
let op_instr : instr list = 
  match op with 
  | Inc -> [IAdd(Const(1),Reg(Rax))] 
  | Dec -> [ISub(Const(1),Reg(Rax))]
(* combines instructions created *)  
in e'_instrs @ op_instr

and binop_helper (op: binop) (elist: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
  (* ensures list length >= 2 *)
  if List.length elist < 2 then
    failwith "Insufficient number of arguments (Binop)" else
    match elist with
    | [] -> []
    | e1 :: rest ->
  (* seperates first argument from rest of expr list*)
  let e1_instrs : instr list = expr_to_instrs e1 env (si + 1) defs is_tc in
  let store : instr list = [IMov(Reg(Rax), stackloc si)] in
  let (op1, op2) : (instr list * instr list) =
    match op with 
    | Plus ->  ([IAdd (stackloc si, Reg(Rax))], [IAdd (stackloc si, Reg(Rax))]) 
    | Minus -> ([ISub (stackloc si, Reg(Rax))], [IAdd (stackloc si, Reg(Rax))])
    | Times -> ([IMul (stackloc si, Reg(Rax))], [IMul (stackloc si, Reg(Rax))])
  in
  binop_list_helper op2 rest env si defs is_tc store false @ e1_instrs @ op1

and binop_list_helper (op: instr list) (elist: expr list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) (store: instr list) (did_rec: bool): instr list = 
  match elist with
  | [] -> []
  | e :: rest -> if did_rec then
                  expr_to_instrs e env si defs is_tc @ op @ store @ binop_list_helper op rest env (si + 1) defs is_tc store did_rec
                 else
                  expr_to_instrs e env si defs is_tc @ store @ binop_list_helper op rest env (si + 1) defs is_tc store true         
;; 

let rec compile_arglist (body: expr) (arglist: (string * typ) list) (env: tenv) (si: int) (defs: def list) (is_tc: bool) : instr list =
    match arglist with
    | [] -> expr_to_instrs body env si defs is_tc
    | (var, typ) :: rest -> compile_arglist body rest ((var, si) :: env) (si + 1) defs is_tc
;;          

(*  compiles an individual function definition into a list of instructions *)
let compile_def (d: def) (defs: def list) : instr list = 
   let DFun(fname, arglist, typ, body) = d in
   (* make label for function name *)
   let lab : instr list = [ILab(fname)] in
   let body_instrs : instr list =  compile_arglist body arglist [] 2 defs false in
   (* make return instruction for function *)
   let ret : instr list = [IRet] in
     lab @ body_instrs @ ret
;;

(* compile a list of function definitions into one list of instructions *)
let rec compile_defs (defs: def list) (defs_copy : def list) : instr list =
    match defs with
    | [] -> []
    | hd :: tl -> compile_def hd defs_copy @ compile_defs tl defs_copy
;;

(* optimization code starts here *)
(* higher-order function that helps us apply *any* optimization 
   recursively into a subexpressions of an expression *)
let rec map_optimize (f: expr -> expr) (e: expr) : expr = 
    match e with 
    (* recursive cases *)
    | EUnop(op, e2) -> EUnop(op, f e2)
    | EBinop(op, elist) -> EBinop(op, List.map f elist)
    | EComp(op, e1, e2) -> EComp(op, f e1, f e2)
    | EIf(e1, e2, e3) -> EIf(f e1, f e2, f e3)
    | EApp(fname, elist) -> EApp(fname, List.map f elist)
    | ESet(x, e') -> ESet(x, f e')
    | EWhile(c,blist) -> EWhile(f c, List.map f blist)
    | EDo(c,blist) -> EDo(f c, List.map f blist)
    | ELet(bindings, blist) -> ELet(List.map (fun (var, expr) -> (var, f expr)) bindings, List.map f blist)
    (* base cases *)
    | _ -> e 
;;

let rec is_enums (elist: expr list) : bool =
  match elist with
  | [] -> true
  | e :: rest -> match e with
                 |ENum(n) -> is_enums rest
                 | _ -> false
;;

let rec do_op (elist: expr list) (op: binop) (acc: int) : int =
  match elist with
  | [] -> acc 
  | ENum(n1) :: rest -> do_op rest op (match op with  
                                      | Plus -> acc + n1
                                      | Minus -> acc - n1
                                      | Times -> acc * n1)
  | _ -> failwith "wont get here"
;;

(* constant folding *)
let rec constant_fold (e: expr) : expr =
   match e with
    (* Base cases that do arithmetic *)
    | EBinop(op, ENum(n) :: rest) when is_enums rest -> ENum(do_op rest op n)
    | EUnop(Inc,ENum(n)) -> ENum(n+1)
    | EUnop(Dec,ENum(n)) -> ENum(n-1)
    | EComp(Less, ENum(n1), ENum(n2)) -> EBool(n1 < n2)
    | EComp(Great, ENum(n1), ENum(n2)) -> EBool(n1 > n2)
    | EComp(Eq, ENum(n1), ENum(n2)) -> EBool(n1 = n2)
    | EComp(Eq, EBool(b1), EBool(b2)) -> EBool(b1 = b2) 
    (* dead code elimination *)
    | EIf(EBool(true), e1, e2) -> constant_fold e1
    | EIf(EBool(false), e1, e2) -> constant_fold e2  
    | EWhile(EBool(false),blist) -> EBool(false)
    | _ -> map_optimize constant_fold e
;;

let rec contains_some_var (var: string) (e: expr): bool =
  match e with
  | EId(x) -> true
  | ENum(x) -> false
  | EUnop(op, e2) -> contains_some_var var e2 
  | EBinop(op, elist) -> elist_contains_some_var var elist
  | EComp(op, e1, e2) -> elist_contains_some_var var [e1 ; e2]
  | EIf(e1, e2, e3) -> elist_contains_some_var var [e2; e3]
  | ESet(x, e') -> contains_some_var var e'
  (* | ELet(bindings, blist) -> *) 
  | _ -> false

  and elist_contains_some_var (var: string) (elist: expr list) : bool =
    match elist with 
    | [] -> false
    | expr :: rest -> contains_some_var var expr || elist_contains_some_var var rest
;;

let is_base_case (expr: expr) : bool =
  match expr with
  | ENum(_) | EBool(_) -> true
  | _ -> false
;;

(* actually replaces variables with constants *)
let rec replace (x: string) (newe: expr) (orig: expr) : expr =
  match orig with
  | EId(y) when y = x -> newe
  | ESet(y, e') -> if x = y then ESet(y, e') else ESet(y, replace x newe e')
  | ELet(bindings, blist) ->  (if (contains_some_var x newe) then
                              ELet(replace_bindings bindings x newe, blist)
                              else
                              ELet(replace_bindings bindings x newe, List.map (fun e -> replace x newe e) blist))
  | _ -> map_optimize (replace x newe) orig

  and replace_bindings (bindings: (string * expr) list) (x: string) (newe: expr): (string * expr ) list =
    match bindings with
    | [] -> []
    | (var, expr) :: rest -> (var, replace x newe expr) :: replace_bindings rest x newe
;;

(* constant propagation *)
let rec constant_prop (e: expr) : expr =
   match e with
   | ELet(bindings, blist) when List.length blist = 1 -> replace_all bindings (List.hd blist)
   | _ -> map_optimize constant_prop e

  and replace_all (bindings: (string * expr) list) (orig: expr) : expr =
  match bindings with
  | [] -> orig
  | (var, ENum(n)) :: rest -> replace_all rest (replace var (ENum(n)) orig) 
  | (var, EBool(v)) :: rest -> (* Pass along the updated expression after replaces *) 
                           replace_all rest (replace var (EBool(v)) orig)
  | _ -> map_optimize constant_prop (ELet(bindings, [orig]))
;;

let rec copy_prop (e: expr) : expr =
  match e with
  | ELet(bind1, [ELet(bind2, blist)]) when List.length blist = 1 -> copy_prop_helper bind1 bind2 (List.hd blist)
  | _ -> map_optimize copy_prop e

  and copy_prop_helper (bind1: (string * expr) list) (bind2: (string * expr) list) (blist: expr): expr = 
    match bind1 with
    | [] -> if List.length bind2 = 0 then
              ELet(bind1, [blist]) else
              ELet(bind1, [ELet(bind2, [blist])])
    | (var, expr) :: rest -> let (new_bind, new_blist) =  update_bind_blist var bind2 [] blist in
                             copy_prop_helper rest new_bind new_blist

  and update_bind_blist (var: string) (bind2: (string * expr) list) (new_bind: (string * expr) list) (blist: expr): ((string * expr) list * expr) =
    match bind2 with 
    | [] -> (new_bind, blist)
    | (y, expr) :: rest -> if (contains_var var expr) then update_bind_blist var rest new_bind (replace y expr blist) 
                           else update_bind_blist var rest ((y, expr) :: new_bind) blist

  and contains_var (var: string) (e: expr): bool =
    match e with
    | EId(x) -> true
    | ENum(x) -> false
    | EUnop(op, e2) -> contains_var var e2 
    | EBinop(op, elist) -> elist_contains_var var elist
    | EComp(op, e1, e2) -> elist_contains_var var [e1 ; e2]
    | EIf(e1, e2, e3) -> elist_contains_var var [e2; e3]
    | ESet(x, e') -> contains_var var e'
    (* | ELet(bindings, blist) -> *) 
    | _ -> false

  and elist_contains_var (var: string) (elist: expr list) : bool =
      match elist with 
      | [] -> false
      | expr :: rest -> contains_var var expr || elist_contains_var var rest
  ;;

(* makes a binop with body > 2 if there exist one in the next level *)
let rec remove_nested_eq_ops (elist: expr list) (op: binop) : expr list =
  match elist with
  | [] -> []
  | EBinop(new_op, elist) :: rest  when op = new_op -> elist @ remove_nested_eq_ops rest op
  | hd :: rest -> [hd] @ remove_nested_eq_ops rest op
;;

(* checks if an expr list contains an ENum(0) *)
let rec contains_enum (elist: expr list) (enum: expr): bool =
  match elist with
  | [] -> false
  | hd :: rest -> if enum = hd then true else contains_enum rest enum
;;

(* remove all ENum(0) from an expr list *)
let rec remove_enum (elist: expr list) (expr: expr): expr list =
  match elist with
  | [] -> []
  | hd :: rest -> if hd = expr then
                    remove_enum rest expr
                    else hd :: remove_enum rest expr
;;

(* makes a new binop with all enums simplified *)
let simplify_binop (enums: expr list) (non_enums: expr list) (first: expr list) (op: binop) (cons: bool): expr =
  (* Minus
      - first is an enum (when cons = true) && enums >= 1 -> (constant fold appended first @ enums) @ non_enums
      - first is a non-enum (when cons = false) && enums >= 2 -> (constant fold appended enums) @ first @ non_enums *)
  (* (1 3) () (z) false *)
  if (List.length enums >= 1) && (List.length non_enums >= 1) && op = Minus && cons then
    EBinop(op, (constant_fold (EBinop(op, first @ enums))) :: non_enums)
  else if (List.length enums >= 2) && op = Minus then 
    (* [z] @ [1 3] -> [4]*)
    EBinop(op, first @ [(constant_fold (EBinop(Plus, enums)))] @ non_enums)
  else if (List.length enums >= 2) && (List.length non_enums >= 1) then
    EBinop(op, first @ [(constant_fold (EBinop(op, enums)))] @ non_enums) 
  else if (List.length enums + List.length non_enums + List.length first >= 2) then 
    EBinop(op, first @ enums @ non_enums) 
  else failwith "missing case?"
;;

(* makes a tuple of enums and other representations of numbers *)
let rec sorted_tuple (elist: expr list) (enums: expr list) (non_enums: expr list) (op: binop): (expr list * expr list) =
  match elist with
  | [] -> (enums, non_enums)
  | ENum(n) :: rest -> sorted_tuple rest (ENum(n) :: enums) non_enums op 
  | e :: rest -> sorted_tuple rest enums (e :: non_enums) op
;;

let algebra_helper (elist1: expr list) (first: expr list)(op: binop) (cons: bool): expr =
  (* Possibility of being reduced further *)
  (* (1 3) & (z) *)
  if List.length elist1 + List.length first >= 2 then
    (* (1 3) -> [1, 3] []*)
    let (enums, non_enums) : (expr list * expr list) = sorted_tuple elist1 [] [] (if op = Minus then Plus else op) in
    simplify_binop enums non_enums first op cons
  (* Only one expr left return it *)
  else if List.length elist1 + List.length first = 1 then
    if op = Minus then List.hd first else List.hd elist1
  (* Only contained 0's *)
  else 
    if op = Times then ENum(1) else ENum(0)
;;

(* algebraic simplification *)
let rec algebra (e: expr) : expr =
   match e with
   | EBinop(Plus, elist)-> let elist1 : expr list = remove_enum (remove_nested_eq_ops elist Plus) (ENum(0)) in
                           algebra_helper elist1 [] Plus true
   | EBinop(Times, elist) when contains_enum elist (ENum(0)) -> ENum(0)
   | EBinop(Times, elist) -> let elist1 : expr list = remove_enum (remove_nested_eq_ops elist Times) (ENum(1)) in
                              algebra_helper elist1 [] Times true
   | EBinop(Minus, e :: rest) ->  (* (- (- z 1) 3) -> (- z 1 3)*)
                                  let elist =  remove_nested_eq_ops (e :: rest) Minus in
                                  (* (- z 1 3) -> z *)
                                  let e1 = List.hd elist in
                                  (* (- z 1 3) -> (1 3) *)
                                  let rest1 = List.tl elist in
                                  (* z -> false *)
                                  let cons : bool =  (match e1 with
                                                    |ENum(_) -> true
                                                    | _ -> false) in
                                  (* (1 3) -> (1 3)*)
                                  let elist1 : expr list = remove_enum rest1 (ENum(0)) in
                                  algebra_helper elist1 [e1] Minus cons
   | _ -> map_optimize algebra e
;;


let rec strength_reduce_helper (copy: expr) (count: int): expr list =
  match count with
  | 0 -> []
  | _ -> copy :: strength_reduce_helper copy (count - 1)
;;

(* strength reduction *)
let rec strength_reduce (e: expr) : expr =
   match e with
   | EBinop(Times, [expr;ENum(1)]) | EBinop(Times, [ENum(1); expr]) -> expr
   | EBinop(Times, [expr;ENum(-1)]) | EBinop(Times, [ENum(-1); expr]) -> EBinop(Minus, [ENum(0);expr]) 
   | EBinop(Times, [ENum(n); expr]) | EBinop(Times, [expr; ENum(n)])->  
                                                  if n < -1 && n >= -8 then
                                                     EBinop(Minus, [ENum(0)] @ strength_reduce_helper expr (abs(n)))
                                                  else if n > 1 && n <= 9 then 
                                                    EBinop(Plus, strength_reduce_helper expr n)
                                                  else e 
   | _ -> map_optimize strength_reduce e
;;

(* fixpoint *)
(* tries to optimize an expression until it can no longer be optimized *)
let rec optimize (e: expr) : expr =
   (* for each round of optimization try to do algebraic simplification, constant propagation
    * and strength reduction *)
   (* not calling constant_fold separately because algebra will call it *)
   (*let (improved_expr : expr) = strength_reduce (constant_prop (copy_prop (algebra e))) in*)
   let (improved_expr : expr) = algebra e in
   (* check if we made progress *)
   (* if no, stop optimizing *)
   if improved_expr = e then improved_expr
   (* if yes, try to optimize again *)
   else optimize improved_expr

(* compiles a program to an x86 string *)
let compile (p: prog) : string = 
  let (defs, body) = p in 
  (* try to optimize the body of the program*)
  let improved_body = optimize body in
    (* generate instructions for the definitions *)
  let defs_instrs = compile_defs defs defs in
  (* make string for defs instrs *)
  let defs_str = instrs_to_string defs_instrs in
  (* generate instrs for the expression *)
  let expr_instrs = expr_to_instrs improved_body [] 1 defs false in
  (* make string for expr instrs *)
  let expr_str = instrs_to_string (expr_instrs @ [IRet]) in
  (* add the boilerplate to instructions to make it work *)
  sprintf "
  .text
  .globl our_code_starts_here
  %s
  our_code_starts_here:
  %s
  \n" defs_str expr_str
  (* note that definitions go ABOVE our_code_starts_here *)
;;