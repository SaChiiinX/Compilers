(* all of your function implementations should go here *)

(* here is a function that calculates the max of two integers *)
let max (m: int) (n: int) : int = 
        if (m > n) then m else n
;;

(* here is a function that changes the first element of a list to a 5 *)
let list_5 (l: int list) : int list = 
        match l with 
        | [] -> []
        | hd :: tl -> 5 :: tl
;;

(* Increments all values in an int list by one *)

let rec inc_all (l: int list) : int list =
        match l with
        | [] -> []
        | hd :: tl -> 1 + hd :: inc_all tl
;;

(* Removes duplicate integers from a list *)
let rec consecutive_dupes (l: int list) : int list =
        match l with
        | [] -> []
        | hd1 :: [] -> hd1 :: []
        | hd1 :: hd2 :: tl -> if hd1 = hd2 then consecutive_dupes (hd2 :: tl)
                                           else hd1 :: consecutive_dupes (hd2 :: tl)
;;

(* Sums all integers from a list *)
let rec sum_list (l: int list) : int =
        match l with
        | [] -> 0
        | hd :: tl -> hd + sum_list tl
;;

(* Makes a list of all sums of the inner lists of int list list *)
let rec sum_all (l: int list list) : int list =
        match l with
        | [] -> []
        | hd :: tl -> sum_list hd :: sum_all tl
;;


(* Returns a list of strings that contains stringa with length greater than given number *)
let rec long_strings (l: string list) (n: int) : string list =
        match l with
        | [] -> []
        | hd :: tl -> if (String.length hd > n) then hd :: long_strings tl n
                                         else long_strings tl n
;;

(*  Returns pairs found in the list if the pair contains the given number *)
let rec find_pairs (l: (int * int) list) (n: int) : (int * int) list =
        match l with
        | [] -> []
        | (int1, int2) :: tl -> if ( int1 = n || int2 = n) then (int1, int2) :: find_pairs tl n 
                                                           else find_pairs tl n
;;

(* Makes a list of (quotient, remainder) from the given list and number where each 
   element of the list dividend and the number is the divider *)
let rec remainders (l: int list) (n: int) : (int * int) list =
        match l with
        | [] -> []
        | hd :: tl -> (hd / n , hd mod n) :: remainders tl n
;;

(* Calculates the mean of the integers from a list then returns it as an int option *)
let mean (l: int list) : int option =
        match l with
        | [] -> None
        | _ -> Some((sum_list l) / (List.length l))
;;

(* Reverses a list *)
let rec rev (l: int list) : int list =
        match l with
        | [] -> []
        | hd :: tl -> rev tl @ [hd]
;;

(* Returns the maximum value of an int list *)
let rec get_max (l: int list) (max: int) : int =
        match l with
        | [] -> max
        | hd :: tl -> if (hd > max) then get_max tl hd 
                                    else get_max tl max
;;

(* Gets the maximum value of a list and returns it as an int option *)
let max_list (l: int list) : int option =
        match l with
        | [] -> None
        | hd :: tl -> Some(get_max tl hd)
;;

(*  Returns the first n many items of the list *)
let rec first_n_elts (l: int list) (n: int) : int list =
        match l with
        | [] -> []
        | hd :: tl -> if n > 0 then hd :: first_n_elts tl (n - 1)
                               else []
;;


 type btnode =
| Leaf
| Node of int * btnode * btnode

(* Returns the larger of two numbers *)
let get_larger (num1: int) (num2: int) : int =
        if num1 >= num2 then num1 else num2

(* Determiens the deepest dept of btnode tree *)
let rec height (n: btnode) : int =
        match n with
        | Leaf -> 0
        | Node(_,btnode1,btnode2) -> 1 + get_larger(height btnode1) (height btnode2)
;; 

(* Returns the value of all nodes with no children/two leaves *)
let rec no_children (n: btnode) : int list =
        match n with
        | Leaf -> []
        | Node(num,Leaf,Leaf) -> [num]
        | Node(_,btnode1,btnode2) -> no_children btnode1 @ no_children btnode2
;;

(* Compares the btnodes that should be the same *)
let rec mirror_acc (n1: btnode) (n2: btnode) : bool =
        match (n1, n2) with
        | (Leaf,Leaf) -> true
        | (Node(_,left_btnode1,right_btnode1),Node(_,left_btnode2,right_btnode2)) -> 
                mirror_acc left_btnode1 right_btnode2 && mirror_acc left_btnode2 right_btnode1
        | _ -> false
;;

(* Checks if a btnode tree is mirrored *)
let rec mirror (n: btnode) : bool =
        match n with
        | Leaf -> true
        | Node(_,btnode1,btnode2) -> mirror_acc btnode1 btnode2
;;  