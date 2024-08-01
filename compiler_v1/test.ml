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
let def_x3 = "(let ((x 5)) (let ((x 67)) (dec x)))";;
let def_x4 = "(let ((x (let ((x 5)) (dec x)))) (dec x))";;
let addnums = "(+ 5 10)";;
let nested_add = "(+ 5 (+ 10 20))";;
let nested_add2 = "(+ (- 10 5) 20)";;
let nested_arith = "(- (* (- 54 3) 2) 102)";;
let nested_arith2 = "(+ (- 10 (- 10 (let ((x 6)) (dec x)))) (- 10 (- 10 5)))";;
let let_nested = "(let ((x (+ 5 (+ 10 20)))) (* x x))";;
let multiple_bind = "(let ((x (inc 5)) (y (dec 4))) (+ x y))";;
let nested_bind = "(let ((x (+ 3 4)) (y (inc x))) (- x y))";;
let nested_bind2 = "(let ((x (+ 3 4)) (y (inc x)) (z (+ x y))) (+ z z))";;
let nested_bind3 = "(let ((x (let ((x 3)) (inc x))) (y (let ((y 6)) (dec y)))) (* x y))";;
let nested_bind4 = "(let ((x -1) (y -8)) (let ((x 5)) (+ y x)))";;
let nested_bind5 = "(let ((x (+ 2 3))(y x)(z y)) (+ x (- y z)))";;

(* added tests *)
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
(* added error tests *)
(* invalid operator - compile error *)
let invalid_operator = "(# 5 3)";;
(* using reserved word as a variable - parse error *)
let res_word_as_var = "(let ((let 5)) 5)";;
(* undeclared variable in arimetic expression - compile error *)
let undec_var_arith = "(+ x 10)";;
(* undefined function - compile error *)
let undef_fun = "(undefined_variable 5 10)";;

(* tests that should cause errors *)
(* duplicate binding - parser error *)
let failLet = "(let ((x  1) (y 1) (x 10)) x)";;
(* empty let body - parse error *)
let failLet2 = "(let ((x 5)) )";;
(* empty binding list - parse error *)
let empty_let = "(let (()) 5)";;
(* badly formatted binding list - parse error *)
let bad_let = "(let ((x 4) y) 7)";;
(* unbound variable - compiler error *)
let failID = "x";;
(* another unbound variable - compiler error *)
let failID2 = "(let ((y 5)) x)";;
(* using reserved word - parse error *)
let failwords = "(let ((inc 4)) 5)";;
(* another reserved words error *)
let failwords2 = "(let ((x 5) (dec 4)) 5)";;

(* this is the expression that forty_one should parse to *)
let forty_one_p = EUnop(Dec, ENum(42));;

(* this is the expression that nested_bind should parse to *)
let nested_bind_p = ELet([("x",EBinop(Plus,ENum(3),ENum(4)));("y",EUnop(Inc,EId("x")))], EBinop(Minus,EId("x"),EId("y")));;

(* a list of test cases for the parser if you want to test it separately from the compiler *)
let parseTestList = 
  [
    t_parse "forty_one_parse" forty_one forty_one_p;
    t_parse "nested_bind_parse" nested_bind nested_bind_p;
  ]

(* tests the compiler should pass *)
let annaTestList = 
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "def_x" def_x "5";
    t "def_x2" def_x2 "4";
    t "def_x3" def_x3 "66";
    t "def_x4" def_x4 "3";
    t "addnums" addnums "15";
    t "nested_add" nested_add "35";
    t "nested_add2" nested_add2 "25";
    t "nested_arith" nested_arith "0";
    t "nested_arith2" nested_arith2 "10"; 
    t "let_nested" let_nested "1225";
    t "multiple_bind" multiple_bind "9";
    t "nested_bind" nested_bind "-1";
    t "nested_bind2" nested_bind2 "30";
    t "nested_bind3" nested_bind3 "20";
    t "nested_bind4" nested_bind4 "-3";
    t "nested_bind5" nested_bind5 "5"
  ]

(* tests that should raise errors *)
let testFailList =
  [
    t_parse_err "failLet" failLet "Parse error: Duplicate binding";
    t_parse_err "failLet2" failLet2 "Parse error: Invalid";
    t_parse_err "empty_let" empty_let "Parse error: Invalid binding list";
    t_parse_err "bad_let" bad_let "Parse error: Invalid binding list";
    t_parse_err "failwords" failwords "Parse error: Reserved variable name";
    t_parse_err "failwords2" failwords2 "Parse error: Reserved variable name";
    t_err "failID" failID "Compile error: Unbound variable identifier x";
    t_err "failID2" failID2 "Compile error: Unbound variable identifier x";
  ]

(* our test list *)
let myTestList = 
  [
    (* test list *)
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
    (* test fail list *)
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