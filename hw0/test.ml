(* all of your tests will go into this file *)
open OUnit2
open Functions 

(* tests for the max function *)
(* use built in assert_equal function that compares two values *)
let max_test _ = 
        assert_equal 5 (max 5 4);
        assert_equal 3 (max 2 3)
;; 

(* tests for the list_5 function, can also use assert_equal to compare lists *)
let list_5_test _ = 
        assert_equal [5] (list_5 [1]);
        assert_equal [5;2] (list_5 [1;2])
;; 

(* tests for the inc_all function *)
let inc_all_test _ = 
        assert_equal [] (inc_all []);
        assert_equal [1] (inc_all [0]);
        assert_equal [1;2;3;4;5] (inc_all [0;1;2;3;4])
;;

(* tests for the consecutive_dupes function *)
let consecutive_dupes_test _ =
        assert_equal [] (consecutive_dupes []);
        assert_equal [1;2;3;4;5] (consecutive_dupes [1;2;2;3;4;5;5]);
        assert_equal [1;2;3;4;5] (consecutive_dupes [1;2;3;4;5])
;;

(* tests for the sum_all function *)
let sum_all_test _ =
        assert_equal [] (sum_all []);
        assert_equal [0;0] (sum_all [[];[]]);
        assert_equal [3;7] (sum_all [[1;2];[3;4]])
;;

(* tests for the long_strings function *)
let long_strings_test _ =
        assert_equal [] (long_strings [] 1);
        assert_equal [] (long_strings ["how";"are";"you"] 4);
        assert_equal ["hello";"world"] (long_strings ["hello";"world";"hi"] 4)
;;

(* tests for the find_pairs function *)
let find_pairs_test _ =
        assert_equal [] (find_pairs [] 1);
        assert_equal [(4,5);(3,4)] (find_pairs [(2,3);(4,5);(3,4)] 4);
        assert_equal [(1,2);(1,3);(1,4)] (find_pairs [(1,2);(1,3);(1,4)] 1)
;;

(* tests for the remainders function *)
let remainders_test _ = 
        assert_equal [] (remainders [] 1);
        assert_equal [(1,1);(2,0);(3,2)] (remainders [4;6;11] 3);
        assert_equal [(1,0);(2,0);(3,0);(3,1)] (remainders [2;4;6;7] 2)
;;

(* tests for the mean function *)
let mean_test _ =
        assert_equal None (mean []);
        assert_equal (Some 4) (mean [2;4;6]);
        assert_equal (Some 6) (mean [1;5;6;12])
;;

(* tests for the rev function *)
let rev_test _ =
        assert_equal [] (rev []);
        assert_equal [3;2;1] (rev [1;2;3]);
        assert_equal [1;2;3] (rev [3;2;1])
;;

(* tests for the max_list function *)
let max_list_test _ =
        assert_equal None (max_list []);
        assert_equal (Some 1) (max_list [1]);
        assert_equal (Some 4) (max_list [4;1;3])
;;

(* tests for the first_n_elts function *)
let first_n_elts_test _ =
        assert_equal [] (first_n_elts [] 1);
        assert_equal [1;3] (first_n_elts [1;3;5] 2);
        assert_equal [2;4;6;8] (first_n_elts [2;4;6;8] 4)
;;

(* tests for the height function *)
let height_test _ =
        assert_equal 0 (height Leaf);
        assert_equal 1 (height (Node (3, Leaf, Leaf)));
        assert_equal 4 (height (Node (1, Node (2, Node (3, Node 
                               (4, Leaf, Leaf), Leaf), Leaf), Leaf)));
        assert_equal 4 (height (Node (1, Node (2, Node 
                               (3, Node (4, Leaf, Leaf), Leaf), Node 
                               (5, Leaf, Leaf)), Node (6, Node (7, Leaf, Leaf), Leaf))));
;;

(* tests for the no_children function *)
let no_children_test _ =
        assert_equal [] (no_children Leaf);
        (* makes tree for testing *)
        let tree1 = Node (5, Node (7, Leaf, Leaf), Leaf) in
        assert_equal [7] (no_children tree1);
        (* makes another tree for testing *)
        let tree2 = Node (10, Node (8, Leaf, Leaf), Node (12, Leaf, Leaf)) in
        assert_equal [8;12] (no_children tree2);
;;

(* tests for the mirror function *)
let mirror_test _ =
        (* makes tree for testing *)
        let tree1 = Node (5, Node (3, Leaf, Leaf), Node (7, Leaf, Leaf)) in
        assert_equal true (mirror tree1);
        (* makes another tree for testing *)
        let tree2 = Node (5, Node (7, Leaf, Leaf), Leaf) in
        assert_equal false (mirror tree2);
        (* makes another tree for testing *)
        let tree3 = Leaf in
        assert_equal true (mirror tree3);
        (* makes another tree for testing *)
        let tree4 = Node (1, Node (2, Node (4, Leaf, Leaf), Node 
                         (5, Leaf, Node (8, Leaf, Leaf))), Node (3, Node 
                         (6, Leaf, Leaf), Node (7, Node (9, Leaf, Leaf), Leaf))) in
        assert_equal false (mirror tree4);
        (* makes another tree for testing *)
        let tree5 = Node (1, Node (2, Node (4, Node (8, Leaf, Leaf), Leaf), Node 
                         (5, Leaf, Leaf)), Node (3, Node (6, Leaf, Leaf), Node 
                         (7, Leaf, Node (9, Leaf, Leaf)))) in
        assert_equal true (mirror tree5);
;;

let suite = 
        "suite">:::
                [
                        "max_test" >:: max_test;
                        "list_5_test" >:: list_5_test;
                        "inc_all_test" >:: inc_all_test;
                        "consecutive_dupes_test" >:: consecutive_dupes_test;
                        "sum_all_test" >:: sum_all_test;
                        "long_strings_test" >:: long_strings_test;
                        "find_pairs_test" >:: find_pairs_test;
                        "remainders_test" >:: remainders_test;
                        "mean_test" >:: mean_test;
                        "rev_test" >:: rev_test;
                        "max_list_test" >:: max_list_test;
                        "height_test" >:: height_test;
                        "no_children_test" >:: no_children_test;
                        "mirror_test" >:: mirror_test
                ];;

let () = 
        run_test_tt_main suite