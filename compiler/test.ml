open Runner
open Expr
open Printf
open OUnit2


(* test_run tests expected value against value produced by compiler generated assembly *)
(* this function combines the name of the test with test_run *)
let t name program expected = name>::test_run program name expected;;

(* test_err tests expected errors against errors actually produced by the compiler *)
(* works for both compiler and parser errors *)
(* this function combines the name of the test with test_err *)
let t_err name program expected =
  name>::test_err program name expected;;

(* if you don't want to make all of your tests strings, you can make files instead *)
(* converts a file name to a string - MAKE SURE THE FILE ENDS WITH .INT *)
(* put the test file in the input folder *)
let f_to_s fname = string_of_file ("input/" ^ fname);;

(* makes a string out of a file and runs a test with a name attached to it *)
let t_file test_name file_name expected = (t test_name (f_to_s file_name) expected);;

(* now, if you have a test file named sloth.int which should generate 5 as a result *)
(* you can make a test like this: t_file "sloth_test" "sloth.int" "5" *)

(* example tests that should pass and generate integer results *)
let forty_one = "(dec 42)";;
let forty = "(dec (dec 42))";;
let add1 = "(inc (inc (inc 3)))";;
let ifTest = "(if true 5 6)";;
let boolTest = "true";;
let assoc_add = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (+ (+ z 1) 4))";;
let sub = "(def (f x : Num y : Num) : Num  (+ x y)) (let ((z (f 2 3))) (- (- z 1) 3))"
let assoc_mul = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* (* z 2) 3))";;
let zero = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (+ 0 z))";;
let zero2 = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (- z 0))";;
let zero3 = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* z 0))";;
let id = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* 1 z))";;
let nid = "(def (f x : Num y : Num) : Num (+ x y))(let ((z (f 2 3))) (* -1 z))";;
let commute = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (+ (+ 1 z) 4))";;
let strength = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* z 4))";;
let mult = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* (* z 3) 4))";;
let power = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (* 8 z))";;
let thrash = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 1 3))) (+ z (+ z 3)))";;

let strength_test = "(def (f x : Num y : Num) : Num (+ x y)) (let ((z (f 2 3))) (+ z z z))";;

(* tests that should cause error *)
(* unbound variable - compiler error *)
let failID = "x";;

(* tests the compiler should pass *)
let annaTestList = 
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "ifTest" ifTest "5";
    t "boolTest" boolTest "1";
    t "assoc_add" assoc_add "10";
    t "sub" sub "1";
    t "assoc_mul" assoc_mul "30";
    t "zero" zero "5";
    t "zero2" zero2 "5";
    t "zero3" zero3 "0";
    t "id" id "5";
    t "nid" nid "-5";
    t "commute" commute "10";
    t "strength" strength "20";
    t "mult" mult "60";
    t "power" power "40";
    t "thrash" thrash "11";
    t "strength_test" strength_test "15";
  ]

(* tests that should raise errors *)
let testFailList =
  [
    t_err "failID" failID "Compile error: Unbound variable identifier x";
  ]

(* our tests *)
(* tests for Unop *)
let incTest_nested = "(inc (inc 8))";;
let incTest_bool = "(inc true)";;
let decTest_nested = "(dec (dec 12))";;

(* tests for Binop *)
let addTest_simple = "(- 5 7 (+ 2 3))";;
let addTest_addneg = "(+ (- 5 3 1) 7 1)";;
let mulTest_multiple = "(* 10 3 2 1)";; (*LOOPS*)
let simple_arith = "(+ 7 (* 3 4))";;
let complex_arith = "(+ (* 2 (- 5 3)) 10)";;
let varTest_sub = "(let ((x 5) (y 2)) (- (* x 2) y))";;
let tripleNest_test = "(+ 5 (- 11 3 (* 1 2 4)))";;

(* tests for Let *)
let letTest_simple = "(let ((x 5) (y 10)) (+ x y))";; (*LOOPS*)
let letTest_nested = "(let ((x 5) (y (let ((z 3)) (+ x z)))) (* x y))";; (*LOOPS*)

(* tests for If *)
let ifTest_1 = "(if 42 0 1)";;
let ifTest_2 = "(if true 42 0)";;
let ifTest_3 = "(let ((a 5)) (inc a) (if (= a 5) 1 0))";;

(* tests for Comp *)
let compTest_simple = "(< 3 6)";;
let compTest_fail = "(> false 2)";;

(* tests for Set *)
let setTest_1 = "(let ((x 5)) (set x 10))";;
let setTest_2 = "(let ((x 5) (y 10)) (set x 1) (set y (+ x y)) y)";; (*LOOPS*)

(* tests for While *)
let whileTest_nested = "(let ((x 0) (y 0)) (do (< x 3) (do (< y 2) (set y (inc y))) (set x (inc x))) (+ x y))";; (*LOOPS*)
let whileTest_fail = "(let ((x 0) (y 0)) (do (< x y)))";;

(* tests for Do *)
let doTest_intense = "(let ((x 2)) (let ((y 5)) (do (< x y)(set x (inc x))(set y (dec y))) (+ x y)))";; (*LOOPS*)

(* tests for For *)
let forTest = "(for (x 0) (set x (inc x)) (< x 4) x)";;

(* tests for unique cases *)
let undec_var_arith = "(+ x 10)";;
let emptyProgram_fail = "";;
let resName_fail = "(def (dec x : Num) : Num (+ x 1)) (dec 4)";;
let invDef_fail = "(def (dec_val x : Num)) :";;
let invExpr_fail = "(def (f x : Num ) : Num ()) ()";;

(* tests for functions with multiple arguments *)
let recursiveFourArgFuncTest = "(def (recursive_product_four a : Num b : Num c : Num d : Bool) : Num
                                (if (= a 0) 1 (* a (recursive_product_four (- a 1) b c d))))
                                (recursive_product_four 3 2 3 true)";;
let nestedFourArgFuncTest = "(def (nested_four a : Num b : Num c : Num d : Num) : Num (- (* a b) (+ c d))) (nested_four 2 3 4 5)";;

(* tests for constant propagation optimization *)
let consPropTest = "(let ((x 10)) x)";;
let consPropTest_complex = "(let ((x 10) (y x)) (+ x y))";; (*LOOPS*)

(* tests for copy propagation optimization *)
let copyPropTest = "(let ((x 4)) (let ((y x)) y))";;
let copyPropTest_check = "(let ((x 4)) x)";;
let copyPropTestTwo = "(let ((x 4)) (let ((z 3) (w x)) (+ z w)))";; (*LOOPS*)
let copyPropTestTwo_check = "(let ((x 4)) (let ((z 3)) (+ z x)))";; (*LOOPS*)
let copyPropTestThree = "(let ((x 4)) (let ((y x)) (let ((w x)) (+ x w))))";; (*LOOPS*)
let copyPropTestThree_check = "(let ((x 4)) (+ x x))";;

(* PUT YOUR TESTS HERE *)
let myTestList = 
  [
    (* tests for Unop *)
    t "incTest_nested" incTest_nested "10";
    t "decTest_nested" decTest_nested "10";
    (* tests for Binop*)
    t "addTest_simple"  addTest_simple "-7";
    t "addTest_addneg" addTest_addneg "9";
    t "simple_arith" simple_arith "19";
    t "complex_arith" complex_arith "14";
    t "varTest_sub" varTest_sub "8";
    t "tripleNest_test" tripleNest_test "5";
    (* tests for If *)
    t "ifTest_2" ifTest_2 "42";
    t "ifTest_3" ifTest_3 "1";
    (* tests for Comp *)
    t "compTest_simple" compTest_simple "1";
    (* tests for Set *)
    t "setTest_1" setTest_1 "10";
    (* tests for For *)
    t "forTest" forTest "4";
    (* tests for functions with multiple arguments *)
    t "recursiveFourArgFuncTest" recursiveFourArgFuncTest "6";
    t "nestedFourArgFuncTest" nestedFourArgFuncTest "-3";
    t "consPropTest" consPropTest "10";
    t "copyPropTest" copyPropTest "4";
    t "copyPropTest_check" copyPropTest_check "4";
    t "copyPropTestThree_check" copyPropTestThree_check "8";
  ]

(* appends all the lists of tests together *)
let suite =
  "suite">:::
  annaTestList @ testFailList @ myTestList

(* run the actual suite *)
let () =
  run_test_tt_main suite
;;
