(* this is where you will convert s-expressions to expressions *)
open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Printf

(* defines rules for what id's are valid, give this to them *)
let reserved_words = ["let"; "inc"; "dec"; "if"; "while"; "true"; "false"; "set"; "do"; "for"];;

(* converts string to Some int or None if not convertible, give this to them *)
let int_of_string_opt (s: string) : int option = 
  try Some(int_of_string s) 
  with Failure _ -> None 
;;

(* checks if a string is in a list *)
let rec find (l: string list) (s: string) : bool = 
    match l with 
    | [] -> false
    | hd :: tl -> if (hd = s) then true else find tl s
;;

(* helper function for parsing that they need to write *)
let rec sexp_to_expr (se: Sexp.t) : expr = 
    match se with
    | Atom("true") -> EBool(true)
    | Atom("false") -> EBool(false)
    | Atom(s) -> (match int_of_string_opt s with 
                  | None -> EId(s) 
                  | Some(i) -> ENum(i))
    | List[Atom("inc"); se] -> EUnop(Inc, sexp_to_expr se)
    | List[Atom("dec"); se] -> EUnop(Dec, sexp_to_expr se)
    | List(Atom("+") :: se1 :: se2 :: sexpr_list) -> EBinop(Plus, sexp_to_expr se1, sexp_to_expr se2, binop_list_helper sexpr_list)
    | List(Atom("-") :: se1 :: se2 :: sexpr_list) -> EBinop(Minus, sexp_to_expr se1, sexp_to_expr se2, binop_list_helper sexpr_list)
    | List(Atom("*") :: se1 :: se2 :: sexpr_list) -> EBinop(Times, sexp_to_expr se1, sexp_to_expr se2, binop_list_helper sexpr_list)
    | List(Atom("let"):: List(binding) :: sexpr_list)-> 
                      ELet(binding_helper binding [], seq_helper sexpr_list)
    | List[Atom("if"); se1; se2; se3] -> 
                      EIf(sexp_to_expr se1, sexp_to_expr se2, sexp_to_expr se3)
    | List[Atom("<"); se1; se2] -> EComp(Less, sexp_to_expr se1, sexp_to_expr se2)
    | List[Atom(">"); se1; se2] -> EComp(Great, sexp_to_expr se1, sexp_to_expr se2)
    | List[Atom("="); se1; se2] -> EComp(Eq, sexp_to_expr se1, sexp_to_expr se2)
    | List[Atom("set"); Atom(var); se] -> ESet(var, sexp_to_expr se)
    | List(Atom("while") :: se :: sexpr_list) -> EWhile(sexp_to_expr se, seq_helper sexpr_list)
    | List(Atom("do") :: se :: sexpr_list) -> EDo(sexp_to_expr se, seq_helper sexpr_list)
    | List(Atom("for") :: var :: delta :: cond :: sexpr_list) -> 
                        let upd_var : (string * expr) = 
                        match binding_helper [var] [] with
                        | [(str, expr)] ->  (str, expr)
                        | _ -> failwith "Invalid expression"
                        in
                        EFor(upd_var, sexp_to_expr delta, sexp_to_expr cond, seq_helper sexpr_list)
    (*| List(Atom("switch") :: Atom(var) :: List(selist) :: selist_list) -> ESwitch(var, seq_helper selist, seq_list_helper selist_list) *)
    | _ -> failwith "Invalid expression"
(*
and seq_list_helper (selist_list: (Sexp.t * Sexp.t list) list) : (expr * expr list) list =
  match selist_list with
  | [] -> []
  | List([se; selist]) -> [(sexp_to_expr se, seq_helper selist)]
  | List([se; selist]) :: rest -> (sexp_to_expr se, seq_helper selist) :: seq_list_helper rest *)

and binop_list_helper (expr_list: Sexp.t list) : expr list =
  match expr_list with
  | [] -> []
  | [expr] -> [sexp_to_expr expr]
  | expr :: rest -> [sexp_to_expr expr] @ binop_list_helper rest

(* takes a list of sexps that make up the body of the let and make them into an list of expr *)
and seq_helper (expr_list: Sexp.t list) : expr list = 
  match expr_list with
  | [] -> failwith "Empty expression list"
  | [expr] -> [sexp_to_expr expr]
  | expr :: rest -> [sexp_to_expr expr] @ seq_helper rest

and binding_helper (binding: Sexp.t list) (vars: string list) : (string * expr) list = 
  match binding with
  | [] -> []
  | List([Atom(str); expr]) :: tl ->  (* if var name is a reserved word *)
                                      if (find reserved_words str) then
                                        failwith "Reserved variable name"
                                      (* if var is already defined *)
                                      else if (find vars str) then
                                        failwith "Duplicate binding"
                                      (* recurses and adds variable to environment *)
                                      else (str, sexp_to_expr expr) :: binding_helper tl (str :: vars)
  | _ -> failwith "Invalid binding list"
;;

(* takes a string to an expr, give them this *)
let rec parse (s: string) : expr = 
  sexp_to_expr (Sexp.of_string s)
;;