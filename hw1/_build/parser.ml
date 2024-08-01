(* this is where you will convert s-expressions to expressions *)
open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Printf

(* defines rules for what id's are valid *)
let reserved_words : string list = ["let"; "inc"; "dec"];;

(* converts string to Some int or None if not convertible *)
let int_of_string_opt (s: string) : int option = 
  try Some(int_of_string s) 
  with Failure _ -> None 
;;

(* TODO: function that turns an S-expression into an expression *)
let rec sexp_to_expr (se: Sexp.t) : expr = 
    match se with
    | Atom(s) -> (match int_of_string_opt s with 
                 | None -> EId(s)
                 | Some(i) -> ENum(i))
    | List[Atom("inc"); expr] -> EUnop(Inc, sexp_to_expr expr)
    | List[Atom("dec"); expr] -> EUnop(Dec, sexp_to_expr expr)
    | List[Atom("+"); expr1 ; expr2] -> EBinop(Plus, sexp_to_expr expr1, sexp_to_expr expr2)
    | List[Atom("-"); expr1 ; expr2] -> EBinop(Minus, sexp_to_expr expr1, sexp_to_expr expr2)
    | List[Atom("*"); expr1 ; expr2] -> EBinop(Times, sexp_to_expr expr1, sexp_to_expr expr2)
    | List[Atom("let"); List(binding) ; expr] -> 
                     ELet(binding_helper binding [], sexp_to_expr expr)
    | _ -> failwith "Invalid"

(* this is a helper function that I found helpful, feel free to use it or not *)
(* takes an Sexp that represents a list of bindings and makes it into a list of tuples *)     
  and binding_helper (binding: Sexp.t list) (vars: string list) : (string * expr) list = 
        match binding with
        | [] -> []
        | List([Atom(str); expr]) :: tl -> if (List.mem str reserved_words) then
                                              failwith "Reserved variable name"
                                            else if (List.mem str vars) then
                                              failwith "Duplicate binding"
                                            else (str, sexp_to_expr expr) :: binding_helper tl (str :: vars)
        | _ -> failwith "Invalid binding list"
;;

(* takes a string to an expr *)
let rec parse (s: string) : expr = 
  sexp_to_expr (Sexp.of_string s)
;;