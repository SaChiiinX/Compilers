(* Defines the Abstract Syntax Tree *)
(* also includes helper functions to convert expressions to strings for printing purposes *)
open Printf

(* unary operators *)
type unop = 
  | Inc
  | Dec 

(* binary operators *)
type binop = 
  | Plus
  | Minus
  | Times

(* numbers, variables, and operators *)
(* note that the let can bind multiple variables at a time *)
type expr = 
  | ENum of int 
  | EId of string
  | EUnop of unop * expr
  | EBinop of binop * expr * expr
  | ELet of (string * expr) list * expr

(* ---------------------------------------------------------------------------------------*)

(* HELPER FUNCTIONS TO CONVERT EXPRESSIONS TO STRINGS *)
let rec expr_to_str (e: expr) : string =
    match e with
    | ENum(i) -> sprintf "ENum(%d)" i
    | EId(str) -> sprintf "EId(%s)" str
    | ELet(binding_list, body) -> 
          sprintf "ELet(%s, %s)" (tuple_list_to_str binding_list) (expr_to_str body)
    | EUnop(op, op_expr) -> sprintf "EUnop(%s, %s)" (unop_to_str op) (expr_to_str op_expr)
    | EBinop(op, e1, e2) -> 
          sprintf "EBinop(%s, %s, %s)" (binop_to_str op) (expr_to_str e1) (expr_to_str e2)

and tuple_list_to_str_helper (elist: (string * expr) list) : string =
    match elist with
    | [] -> ""
    | [(last_x, last_e)] -> sprintf "(%s,%s)]" last_x (expr_to_str last_e)
    | (x, e) :: rest -> sprintf "(%s,%s); %s " x (expr_to_str e) (tuple_list_to_str_helper rest)

and tuple_list_to_str (elist: (string * expr) list) : string =
    sprintf "[%s" (tuple_list_to_str_helper elist)

and unop_to_str (op: unop) : string =
    match op with
    | Inc -> "Inc"
    | Dec -> "Dec"

and binop_to_str (op: binop) : string =
    match op with
    | Plus -> "Plus"
    | Times -> "Times"
    | Minus -> "Minus"

