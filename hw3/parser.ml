(* this is where you will convert s-expressions to expressions and definitions *)
open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Printf

(* reserved function names *)
let reserved_words = ["inc"; "dec"; "if"; "while"; "true"; "false";"def"];;

(* converts string to Some int or None if not convertible *)
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
    | List(Atom("+") ::  sexpr_list) -> EBinop(Plus, binop_list_helper sexpr_list)
    | List(Atom("-") ::  sexpr_list) -> EBinop(Minus, binop_list_helper sexpr_list)
    | List(Atom("*") ::  sexpr_list) -> EBinop(Times, binop_list_helper sexpr_list)
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
    | List(Atom(fname) :: elist) -> EApp(fname, seq_helper elist)
    | _ -> failwith "Invalid expression"

and binop_list_helper (expr_list: Sexp.t list) : expr list =
  match expr_list with
  | [e1; e2] -> [sexp_to_expr e1; sexp_to_expr e2]
  | expr :: rest -> [sexp_to_expr expr] @ binop_list_helper rest
  | _-> failwith "Invalid Binop expression"

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

let parse_typ (typ: Sexp.t) : typ =
  match typ with
  | Atom("Num") -> TNum
  | Atom("Bool") -> TBool
  | _ -> failwith "Invalid Type"
;;

let rec parse_args (selist: Sexp.t list ) (env: string list) : (string * typ) list =
  match selist with
  | [] -> []
  | Atom(var) :: Atom(":") :: typ :: rest ->  (* if var name is a reserved word *)
                                                  if (find reserved_words var) then
                                                  failwith "Reserved variable name"
                                                  (* if var is already defined *)
                                                  else if (find env var) then
                                                  failwith "Multiple function arguments"
                                                  (* recurses and adds variable to environment *)
                                                  else (var, parse_typ typ) :: parse_args rest (var :: env)
  | _ -> failwith "Invalid Args list"
;;

(*  parses an sexp into a function definition *)
let parse_def (se: Sexp.t): def = 
  match se with 
  | List[Atom("def"); List(Atom(fname) :: arglist); Atom(":"); typ; body] 
              -> DFun(fname, parse_args arglist [], parse_typ typ, sexp_to_expr body)
  | _ -> failwith "Invalid definition"
;;

(* parses a list of sexps into a program *)
(* a program is a list of function definitions and one expression *)
let parse_program (selist: Sexp.t list) : prog = 
  let rec program_helper (selist: Sexp.t list) (defs: string list) : prog =
      match selist with 
      | [] -> failwith "Empty program"
      | [se] -> ([], sexp_to_expr se) 
      | se :: rest -> let DFun(fname, args, typ, body) = parse_def se in
                      if (find reserved_words fname) then
                        failwith "Reserved function name"
                      else if (find defs fname) then
                        failwith "Multiple function definitions"
                      else
                      let new_def = parse_def se in
                      let defs, body = program_helper rest (fname :: defs) in 
                      (new_def :: defs, body)
  in program_helper selist []
;; 
