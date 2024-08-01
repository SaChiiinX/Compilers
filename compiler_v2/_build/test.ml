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
  name>::(fun _ -> assert_equal expected (parse_string program));;

(* test_err tests expected compiler errors against errors actually produced by the compiler *)
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
let def_x = "(let ((x 5)) x)";;
let def_x2 = "(let ((x 5)) (dec x))";;
let ifTest = "(if true 5 6)";;
let ifTestLet = "(let ((x 5)) (if (= x 7) 7 8))";;
let setTest = "(let ((x 1)) (set x 2) x)";;
let setTest2 = "(let ((x false)) (set x true) x)";;
let whileTest = "(let ((x 0)) (while (< x 4) (set x (inc x))) x)"
let whileIntense = "(let ((x 2)) (let ((y 5)) (while (< x y)(set x (inc x))(set y (dec y))) (+ x y)))";;
let boolTest = "true";;
let while_weird = "(let ((x 5)) (while (< x 5) (set x (inc x)) (= true true)) x)";;


(* tests that should cause error *)
(* unbound variable - compiler error *)
let failID = "x";;
(* another unbound variable error *)
let failSet = "(set x 2)";;

(* empty bodies *)
let empty_while = "(while true)";;
let empty_let = "(let ((x 4)) )";;

(* bad variable naming *)
let bad_let = "(let ((set 2)) 3)";;

(* tests for type mismatch errors *)
let inc_args = "(inc true)";;
let plus_args = "(+ 1 false)";;
let arith_nested = "(+ 1 (- 2 (* true false)))";;
let if_cond = "(if 1 2 3)";;
let if_branch = "(if true false (inc 3))";;
let comp = "(< 3 true)";;
let set = "(let ((b false)) (set b 5))";;
let while_int = "(let ((x 5)) (while (+ x 1) (set x (inc x))))";; 
let while_bad = "(let ((x 5)) (while (< x 5) (set x (inc x)) (= 4 true)) x)";;
let bad_logic = "(< true false)";;
let while_false = "(inc (while false 4 5))";;

(* Our Tests*)
let doTest = "(let ((x 0)) (do (< x 4) (set x (inc x))) x)";;
let doIntense = "(let ((x 2)) (let ((y 5)) (do (< x y)(set x (inc x))(set y (dec y))) (+ x y)))";;
let forTest = "(for (x 0) (set x (inc x)) (< x 4) x)";;
let mult_binop = "(+ 1 2 3 4 5)"
let myTest_1 = "(inc true)";;
let myTest_2 = "(if 42 0 1)";;
let myTest_3 = "(set x 10)";;
let myTest_4 = "(set y 5)";;
let myTest_6 = "(inc 10)";;
let myTest_7 = "(+ 5 7)";;
let myTest_8 = "(let ((x 3) (y 4)) (+ x y))";;
let myTest_9 = "(if true 42 0)";;
let myTest_10 = "(let ((a 5)) (if (= a 5) 1 0))";;
let doWhile = "(let ((x 0)) (do (< x 4) (set x (inc x))) x)";;
let doWhileNest = "(let ((x 0) (y 0)) (do (< x 3) (do (< y 2) (set y (inc y))) (set x (inc x))) (+ x y))";;
let doWhileFail = "(let ((x 0) (y 0)) (do (< x y)))";;
let add_neg = "(+ (- 5 3) 7)";;
let mul = "(* 10 3)";;
let complex_arith = "(+ (* 2 (- 5 3)) 10)";;
let simple_let = "(let ((x 5) (y 10)) (+ x y))";;
let nest_let = "(let ((x 5) (y (let ((z 3)) (+ x z)))) (* x y))";;
let simple_arith = "(+ 7 (* 3 4))";;
let nest_arith = "(- (* 5 6) 2)";;
let var_mul = "(let ((a 3) (b 4)) (* a b))";;
let var_sub = "(let ((x 5) (y 2)) (- (* x 2) y))";;
let var_add = "(let ((m 7) (n 3)) (+ m n))";;
let var_test = "(let ((x 1) (y x)) (+ y (+ x 5)))";;

let invalid_operator = "(# 5 3)";;
let res_word_as_var = "(let ((let 5)) 5)";;
let undec_var_arith = "(+ x 10)";;
let undef_fun = "(undefined_variable 5 10)";;

(* this is the expression that forty_one should parse to *)
let forty_one_p = EUnop(Dec, ENum(42));;

(* this it the expression that while_weird should parse to *)
let while_weird_p = ELet(["x",ENum(5)],[EWhile(EComp(Less,EId("x"),ENum(5)),[ESet("x",EUnop(Inc,EId("x")));EComp(Eq,EBool(true),EBool(true))]);EId("x")]);;

(* a list of test cases for the parser if you want to test it separately from the compiler *)
let parseTestList = 
  [
    t_parse "forty_one_parse" forty_one forty_one_p;
    t_parse "while_weird_parse" while_weird while_weird_p;
  ]

(* tests the compiler should pass *)
let annaTestList = 
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "def_x" def_x "5";
    t "def_x2" def_x2 "4";
    t "ifTest" ifTest "5";
    t "ifTestLet" ifTestLet "8";
    t "setTest" setTest "2";
    t "setTest2" setTest2 "1";
    t "whiletest" whileTest "4";
    t "boolTest" boolTest "1";
    t "whileIntense" whileIntense "7";
    t "while_weird" while_weird "5";
  ]

(* tests that should raise errors *)
let testFailList =
  [
    t_err "failID" failID "Compile error: Unbound variable identifier x";
    t_err "failSet" failSet "Compile error: Unbound variable identifier x";
    t_err "inc_args" inc_args "Compile error: Type Mismatch" ;
    t_err "plus_args" plus_args "Compile error: Type Mismatch" ;
    t_err "arith_nested" arith_nested "Compile error: Type Mismatch";
    t_err "if_cond" if_cond "Compile error: Type Mismatch" ;
    t_err "if_branch" if_branch "Compile error: Type Mismatch" ;
    t_err "comp" comp "Compile error: Type Mismatch"; 
    t_err "set" set "Compile error: Type Mismatch";
    t_err "while_int" while_int "Compile error: Type Mismatch";
    t_err "while_bad" while_bad "Compile error: Type Mismatch";
    t_err "while_false" while_false "Compile error: Type Mismatch";
    t_parse_err "empty_while" empty_while "Parse error: Empty expression list";
    t_parse_err "empty_let" empty_let "Parse error: Empty expression list";
    t_parse_err "bad_let" bad_let "Parse error: Reserved variable name"; 
    t_err "bad_logic" bad_logic "Compile error: Type Mismatch";
  ]

(* PUT YOUR TESTS HERE *)
let myTestList = 
  [
    t "dotest" doTest "4";
    t "doIntense" doIntense "7";
    t "fortest" forTest "4";
    t "doWhile" doWhile "4";
    t "doWhileNest" doWhileNest "7";
    t "mult_binop" mult_binop "15";
    t_parse_err "doWhileFail" doWhileFail "Parse error: Empty expression list";
    t_err "myTest_1" myTest_1 "Compile error: Type Mismatch";
    t_err "myTest_2" myTest_2 "Compile error: Type Mismatch";
    t_err "myTest_3" myTest_3 "Compile error: Unbound variable identifier x";
    t_err "myTest_4" myTest_4 "Compile error: Unbound variable identifier y";
    t "myTest_6" myTest_6 "11";
    t "myTest_7" myTest_7 "12";
    t "myTest_8" myTest_8 "7";
    t "myTest_9" myTest_9 "42";
    t "myTest_10" myTest_10 "1";
    t "add_neg" add_neg "9";
    t "mul" mul "30";
    t "complex_arith" complex_arith "14";
    t "simple_let" simple_let "15";
    t "nest_let" nest_let "40";
    t "simple_arith" simple_arith "19";
    t "nest_arith" nest_arith "28";
    t "var_mul" var_mul "12";
    t "var_sub" var_sub "8";
    t "var_add" var_add "10";
    t "var_test" var_test "7";
    t_parse_err "invalid_operator" invalid_operator "Parse error: Invalid";
    t_parse_err "res_word_as_var" res_word_as_var "Parse error: Reserved variable name";
    t_err "undec_var_arith" undec_var_arith "Compile error: Unbound variable identifier x";
    t_parse_err "undef_fun" undef_fun "Parse error: Invalid";
  ]

(* appends all the lists of tests together *)
let suite =
  "suite">:::
  parseTestList @ annaTestList @ testFailList @ myTestList

(* run the actual suite *)
let () =
  run_test_tt_main suite
;;