open Unix
open Filename
open Str
open Compile
open Printf
open OUnit2
open ExtLib

(* type for modeling the compiler either producing an error or a value *)
(* Left is Error, Right is value *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

(* prints out either the error or the value produced *)
let either_printer (e: ('a, 'b) either) =
  match e with
    | Left(v) -> sprintf "Error: %s\n" v
    | Right(v) -> v

(* takes a string and produces an expression *)
let parse_string (s: string) : Expr.expr =
  Parser.parse s

(* takes a file and produces an expression *)
let parse_file input_file =
  let sexp = Sexplib.Sexp.input_sexp input_file in
  Parser.sexp_to_expr sexp

(* takes in a file and produces an assembly string *)
let compile_file_to_string input_file =
  let input_program = parse_file input_file in
  (compile input_program);;

(* takes in a string in the source language and produces an assembly string *)
let compile_string_to_string s =
  let input_program = parse_string s in
  (compile input_program);;

(* make temporary files for executing and linking our code *)
let make_tmpfiles name =
  let (null_stdin, _) = pipe() in
  let stdout_name = (temp_file ("stdout_" ^ name) ".out") in
  let stdin_name = (temp_file ("stderr_" ^ name) ".err") in
  (openfile stdout_name [O_RDWR] 0o600, stdout_name,
   openfile stdin_name [O_RDWR] 0o600, stdin_name,
   null_stdin)

(* read a file into a string *)
let string_of_file file_name =
  let inchan = open_in file_name in
  let buf = Bytes.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  Bytes.to_string buf

(* compiles a parsed program p to assembly *)
(* out is the file where the output is printed *)
(* actually executes assembly and links it by running commands from makefile *)
(* finally, actually runs the generated executable *)
(* produces an error OR an integer output *)
let run (p: Expr.expr) out =
  (* produces the assembly string, unless hits a compiler error from our compiler *)
  let maybe_asm_string =
    try Right(compile p)
    with Failure s ->
      Left("Compile error: " ^ s)
  in
  match maybe_asm_string with
  | Left(s) -> Left(s) (* this is the compile error case *)
  | Right(asm_string) -> (* actually executes the assembly file and links it *)
    let outfile = open_out (out ^ ".s") in
    fprintf outfile "%s" asm_string;
    close_out outfile;
    let (bstdout, bstdout_name, bstderr, bstderr_name, bstdin) = make_tmpfiles "build" in
    let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "build" in
    let built_pid = Unix.create_process "make" (Array.of_list [""; out ^ ".run"]) bstdin bstdout bstderr in
    let (_, status) = waitpid [] built_pid in

    let try_running = match status with
    | WEXITED 0 ->
      Right(string_of_file rstdout_name)
    | WEXITED _ ->
      Left(sprintf "Finished with error while building %s:\n%s" out
             (string_of_file bstderr_name))
    | WSIGNALED n ->
      Left(sprintf "Signalled with %d while building %s." n out)
    | WSTOPPED n ->
      Left(sprintf "Stopped with signal %d while building %s." n out) in

    let result = match try_running with
    | Left(_) -> try_running
    | Right(msg) -> (* actually runs the generated executable *)
      printf "%s" msg;
      let ran_pid = Unix.create_process ("./" ^ out ^ ".run") (Array.of_list []) rstdin rstdout rstderr in
      let (_, status) = waitpid [] ran_pid in
      match status with
        | WEXITED 0 -> Right(string_of_file rstdout_name)
        | WEXITED n -> Left(sprintf "Error %d: %s" n (string_of_file rstdout_name))
        | WSIGNALED n ->
          Left(sprintf "Signalled with %d while running %s." n out)
        | WSTOPPED n ->
          Left(sprintf "Stopped with signal %d while running %s." n out) in

    List.iter close [bstdout; bstderr; bstdin; rstdout; rstderr; rstdin];
    List.iter unlink [bstdout_name; bstderr_name; rstdout_name; rstderr_name];
    result

(* ------------------------ Testing functions -------------------------------- *)

(* function that tests a program that should produce output *)
(* takes in a program, an output file and the expected value the program produces *)
(* parses the program into a string and runs it *)
(* compares the integer it gets with expected value *)
(* prints out any errors that arise *)
let test_run program_str outfile expected _ =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string program_str in
  let result = run program full_outfile in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

(* function that tests a program that should produce a compiler error *)
(* takes in a program, an output file, and the expected error message *)
(* compares the error actually raised with the expected error *)
let test_err program_str outfile errmsg _ =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string program_str in
  let result = run program full_outfile in
  assert_equal
    (Left(errmsg))
    result
    ~printer:either_printer
    ~cmp: (fun check result ->
      match check, result with
        | Left(expect_msg), Left(actual_message) ->
          String.exists actual_message expect_msg
        | _ -> false
    )

(* tries to parse a program which is represented as a string *)
let try_parse (program_str : string) =
  try Right(parse_string program_str) with
  | Failure errmsg -> Left("Parse error: " ^ errmsg)

(* either print out the error the parser produced or the AST the parser produced *)
let either_parse_printer e =
  match e with
  | Left(s) -> "Error: " ^ s ^ "\n"
  | Right(_) -> "AST\n"

(* function that tests a program that should produce a parse error *)
(* takes in a program, an output file and the expected error message *)
(* compares the error actually raised with the expected error *)
let test_parse_err program_str outfile errmsg _ =
  let result = try_parse program_str in
  assert_equal
    (Left errmsg)
    result
    ~printer:either_parse_printer
    ~cmp: (fun check result ->
      match check, result with
        | Left(expect_msg), Left(actual_message) ->
          String.exists actual_message expect_msg
        | _ -> false
    )
