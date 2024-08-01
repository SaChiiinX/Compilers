(* Defines the Abstract Syntax Tree *)
open Printf

(*unary operators *)
type unop = 
  | Inc
  | Dec 

(* binary operators *)
type binop = 
  | Plus
  | Minus
  | Times

(* comparison operators *)
type comp = 
  | Eq
  | Less
  | Great

(* need to put typ definition here so that can define function definition *)
type typ = 
  | TNum
  | TBool

(* numbers, variables, and operators *)
type expr = 
  | EBool of bool 
  | ENum of int 
  | EId of string
  | EUnop of unop * expr
  | EBinop of binop * expr list
  | ELet of (string * expr) list * expr list
  | EComp of comp * expr * expr
  | EIf of expr * expr * expr  
  | ESet of string * expr
  | EWhile of expr * expr list
  | EDo of expr * expr list
  | EFor of (string * expr) * expr * expr * expr list
  | EApp of string * expr list

(* a function definition is a name, a list of arguments and their types, 
 * a return type, and a body expression *)
type def = 
  | DFun of string * (string * typ) list * typ * expr

type prog = def list * expr 

(* HELPER FUNCTIONS TO CONVERT EXPRESSIONS TO STRINGS *)
let rec expr_to_str (e: expr) : string =
    match e with
    | EBool(b) -> sprintf "EBool(%b)" b
    | ENum(i) -> sprintf "ENum(%d)" i
    | EId(str) -> sprintf "EId(%s)" str
    | ELet(binding_list, elist) -> 
          sprintf "ELet(%s, %s)" (tuple_list_to_str binding_list) (expr_list_to_str elist)
    | EUnop(op, e') -> sprintf "EUnop(%s, %s)" (unop_to_str op) (expr_to_str e')
    | EBinop(op, elist) ->
          sprintf "EBinop(%s, %s)" (binop_to_str op) (expr_list_to_str elist)
    | EComp(c,e1,e2) ->
          sprintf "EComp(%s, %s, %s)" (comp_to_str c) (expr_to_str e1) (expr_to_str e2)
    | EIf(e1,e2,e3) ->
          sprintf "EIf(%s, %s, %s)" (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)
    | ESet(x,e') -> 
          sprintf "ESet(%s, %s)" x (expr_to_str e')
    | EWhile(e', elist) -> 
          sprintf "EWhile(%s, %s)" (expr_to_str e') (expr_list_to_str elist)
    | EDo(e', elist) ->
          sprintf "EDo(%s, %s)" (expr_to_str e') (expr_list_to_str elist)
    | EFor((var, e1), delta, cond, elist) ->
          sprintf "EFor((%s, %s), %s, %s, %s)" var (expr_to_str e1) (expr_to_str delta) 
            (expr_to_str cond) (expr_list_to_str elist)
    | EApp(f,elist) -> sprintf "EApp(%s, %s)" f (expr_list_to_str elist)

and tuple_list_to_str_helper (elist: (string * expr) list) : string =
    match elist with
    | [] -> ""
    | [(last_x, last_e)] -> sprintf "(%s,%s)]" last_x (expr_to_str last_e)
    | (x, e) :: rest -> sprintf "(%s,%s); %s " x (expr_to_str e) (tuple_list_to_str_helper rest)
        
and tuple_list_to_str (elist: (string * expr) list) : string =
      sprintf "[%s" (tuple_list_to_str_helper elist)
      
and expr_list_to_str_helper (elist: expr list) : string =
    match elist with
    | [] -> ""
    | [e] -> sprintf "%s]" (expr_to_str e)
    | e :: rest -> sprintf "%s; %s " (expr_to_str e) (expr_list_to_str_helper rest)

and expr_list_to_str (elist: expr list) : string =
    sprintf "[%s" (expr_list_to_str_helper elist)

and unop_to_str (op: unop) : string =
    match op with
    | Inc -> "Inc"
    | Dec -> "Dec"

and binop_to_str (op: binop) : string =
    match op with
    | Plus -> "Plus"
    | Times -> "Times"
    | Minus -> "Minus"

and comp_to_str (c: comp) : string =
    match c with
    | Eq -> "Equal"
    | Less -> "Less"
    | Great -> "Greater"


