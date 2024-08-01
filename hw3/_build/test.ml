open Runner
open Expr
open Printf
open OUnit2


(* test_run tests expected value against value produced by compiler generated assembly *)
(* this function combines the name of the test with test_run *)
let t name program expected = name>::test_run program name expected;;

(* parse_string parses a program string into an expression *)
(* this function checks whether the parsed value is equal to the expected value *)
(* also combines the name of the test with the actual test *)
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (parse_string_full program));;

(* test_err tests expected errors against errors actually produced by the compiler *)
(* works for both compiler and parser errors *)
(* this function combines the name of the test with test_err *)
let t_err name program expected =
  name>::test_err program name expected;;

(* test_parse_err tests expected parser errors against errors produced by the parser *)
(* this function combines the name of the test with test_parse_err *)
let t_parse_err name program expected =
  name>::test_parse_err program name expected;;

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
let boolTest = "true"
let defTest = "(def (abs_val x : Num) : Num (if (< x 0) (* -1 x) x)) (abs_val -3)"
let plusTest = "(def (plus x : Num y : Num) : Num (+ x y)) (plus 2 5)";;

(* tests that should cause error *)
(* unbound variable - compiler error *)
let failID = "x";;

(* tests for type mismatch errors *)
let inc_args = "(inc true)";;
let plus_args = "(+ 1 false)";;
let arith_nested = "(+ 1 (- 2 (* true false)))";;
let if_cond = "(if 1 2 3)";;
let if_branch = "(if true false (inc 3))";;
let comp = "(< 3 true)";;
let bad_logic = "(< true false)";;
let fun_body  = "(def (abs_val x : Num) : Num (if (< x 0) true false)) (abs_val -3)"
let fun_app = "(def (abs_val x : Num) : Num (if (< x 0) (* -1 x) x)) (abs_val true)"
let wrong_args1 = "(def (f x : Num ) : Num (+ x 1)) (f 3 2)";;
let wrong_args2 = "(def (f x : Num y : Num) : Num (+ x y)) (f 1)";;

(* function parsing issues *)
let mult_defs = "(def (f x : Num) : Num 4) (def (f y : Num) : Num 5) (+ (f 5) (f 4))";;
let mult_args = "(def (f x : Num x : Num) : Num (+ x y)) (f 3 4)";;
let reserved_def = "(def (inc x : Num) : Num (+ x 1)) (inc 4)";;

(* this is the program that forty_one should parse to *)
(* function definition list then expression body *)
let forty_one_p = ([],EUnop(Dec, ENum(42)));;

(* this is the program that plusTest should parse to *)
let plusTest_p = ([DFun("plus",[("x",TNum);("y",TNum)],TNum,EBinop(Plus,[EId("x"); EId("y")]))],EApp("plus",[ENum(2);ENum(5)]));;

(* this is the program that defTest should parse to *)
let defTest_p = ([DFun("abs_val", [("x",TNum)],TNum,EIf(EComp(Less,EId("x"),ENum(0)),EBinop(Times,[ENum(-1); EId("x")]),EId("x")))],EApp("abs_val",[ENum(-3)]));;

(* our tests *)
(* tests for Unop *)
let incTest_nested = "(inc (inc 8))";;
let incTest_bool = "(inc true)";;
let decTest_nested = "(dec (dec 12))";;

(* tests for Binop *)
let addTest_addneg = "(+ (- 5 3 1) 7 1)";;
let mulTest_multiple = "(* 10 3 2 1)";;
let simple_arith = "(+ 7 (* 3 4))";;
let complex_arith = "(+ (* 2 (- 5 3)) 10)";;
let varTest_sub = "(let ((x 5) (y 2)) (- (* x 2) y))";;

(* tests for Let *)
let letTest_simple = "(let ((x 5) (y 10)) (+ x y))";;
let letTest_nested = "(let ((x 5) (y (let ((z 3)) (+ x z)))) (* x y))";;

(* tests for If *)
let ifTest_1 = "(if 42 0 1)";;
let ifTest_2 = "(if true 42 0)";;
let ifTest_3 = "(let ((a 5)) (inc a) (if (= a 5) 1 0))";;

(* tests for Comp *)
let compTest_simple = "(< 3 6)";;
let compTest_fail = "(> false 2)";;

(* tests for Set *)
let setTest_1 = "(let ((x 5)) (set x 10))";;
let setTest_2 = "(let ((x 5) (y 10)) (set x 1) (set y (+ x y)) y)";;

(* tests for While *)
let whileTest_nested = "(let ((x 0) (y 0)) (do (< x 3) (do (< y 2) (set y (inc y))) (set x (inc x))) (+ x y))";;
let whileTest_fail = "(let ((x 0) (y 0)) (do (< x y)))";;

(* tests for Do *)
let doTest_intense = "(let ((x 2)) (let ((y 5)) (do (< x y)(set x (inc x))(set y (dec y))) (+ x y)))";;

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

(* a list of test cases for the parser if you want to test it separately from the compiler *)
let parseTestList =
  [
      t_parse "forty_one_parse" forty_one forty_one_p;
      t_parse "defTest_parse" defTest defTest_p;
      t_parse "plusTest_parse" plusTest plusTest_p;
  ]

(* tests the compiler should pass *)
let annaTestList = 
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "ifTest" ifTest "5";
    t "boolTest" boolTest "1";
    t "defTest" defTest "3";
    t_file "paritytest" "parity.int" "1";
    t_file "fibtest" "fib.int" "3";
    t_file "primtestrec" "prime-rec.int" "0"; 
  ]

(* tests that should raise errors *)
let testFailList =
  [
    t_err "failID" failID "Compile error: Unbound variable identifier x";
    t_err "inc_args" inc_args "Compile error: Type Mismatch" ;
    t_err "plus_args" plus_args "Compile error: Type Mismatch" ;
    t_err "if_cond" if_cond "Compile error: Type Mismatch" ;
    t_err "if_branch" if_branch "Compile error: Type Mismatch" ;
    t_err "arith_nested" arith_nested "Compile error: Type Mismatch";
    t_err "comp" comp "Compile error: Type Mismatch";
    t_err "bad_logic" bad_logic "Compile error: Type Mismatch";
    t_err "fun_body" fun_body "Compile error: Type Mismatch";
    t_err "fun_app" fun_app "Compile error: Type Mismatch"; 
    t_parse_err "mult_defs" mult_defs "Parse error: Multiple function definitions";
    t_parse_err "mult_args" mult_args "Parse error: Multiple function arguments";
    t_parse_err "reserved_def" reserved_def "Parse error: Reserved function name";
    t_err "wrong_args1" wrong_args1 "Compile error: Type Mismatch";
    t_err "wrong_args2" wrong_args2 "Compile error: Type Mismatch";
  ]

(* PUT YOUR TESTS HERE *)
let myTestList = 
  [
    (* tests for Unop *)
    t "incTest_nested" incTest_nested "10";
    t_err "incTest_bool" incTest_bool "Compile error: Type Mismatch";
    t "decTest_nested" decTest_nested "10";
    (* tests for Binop*)
    t "addTest_addneg" addTest_addneg "9";
    t "mulTest_multiple" mulTest_multiple "60";
    t "simple_arith" simple_arith "19";
    t "complex_arith" complex_arith "14";
    t "varTest_sub" varTest_sub "8";
    (* tests for Let *)
    t "letTest_simple" letTest_simple "15";
    t "letTest_nested" letTest_nested "40";
    (* tests for If *)
    t_err "ifTest_1" ifTest_1 "Compile error: Type Mismatch";
    t "ifTest_2" ifTest_2 "42";
    t "ifTest_3" ifTest_3 "1";
    (* tests for Comp *)
    t "compTest_simple" compTest_simple "1";
    t_err "compTest_fail" compTest_fail "Compile error: Type Mismatch";
    (* tests for Set *)
    t "setTest_1" setTest_1 "10";
    t "setTest_2" setTest_2 "11";
    (* tests for While *)
    t "whileTest_nested" whileTest_nested "7";
    t_parse_err "whileTest_fail" whileTest_fail "Parse error: Empty expression list";
    (* tests for Do *)
    t "doTest_intense" doTest_intense "7";
    (* tests for For *)
    t "forTest" forTest "4";
    (* tests for unique cases *)
    t_err "undec_var_arith" undec_var_arith "Compile error: Unbound variable identifier x";
    t_parse_err "emptyProgram_fail" emptyProgram_fail "Parse error: Empty program";
    t_parse_err "resName_fail" resName_fail "Parse error: Reserved function name";
    t_parse_err "invDef_fail" invDef_fail "Parse error: Invalid definition";
    t_parse_err "invExpr_fail" invExpr_fail "Parse error: Invalid expression";
    (* tests for functions with multiple arguments *)
    t "recursiveFourArgFuncTest" recursiveFourArgFuncTest "6";
    t "nestedFourArgFuncTest" nestedFourArgFuncTest "-3";
  ]

(* appends all the lists of tests together *)
let suite =
  "suite">:::
  parseTestList @ annaTestList @ testFailList @ myTestList
(* run the actual suite *)
let () =
  run_test_tt_main suite
;;
