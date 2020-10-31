(*Problem 1*)
let my_subset_test0 = subset [] [1;3]
let my_subset_tes0 = not (subset [4;5;7] [1;2;4])

(*Problem 2*)
let my_equal_sets_test0 = equal_sets [1;2] [2;1;1;2]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [1])

(*Problem 3*)
let my_set_union_test0 = equal_sets (set_union [1;1;1;2;3;4] [1]) [1;3;2;4]
let my_set_union_test1 = equal_sets (set_union [1;2] []) [2;1]

(*Problem 4*)
let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3;3;3] [3]) [3]
let my_set_intersection_test1 = equal_sets (set_intersection [2;3;5;6] []) []

(*Problem 5*)
let my_set_diff_test0 = equal_sets (set_diff [1;2] [1;4;2;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [] [1;4;5;4]) []
let my_set_diff_test2 = equal_sets (set_diff [1;5;4;5;6;6] [5;6]) [1;4]

(*Problem 6*)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x-> x*3 + x*2 - x*4) 2 = 2
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x-> x / 2) 1000 = 0
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 10.0 +. x *. 4.0) 4.0 = infinity

(*Problem 7*)
type my_awksub_nonterminals =
  | CS | EE | CSE | CE

let my_awksub_rules = 
    [CS, [N EE; N CE];
     CS, [N CSE; T"CS131"];
     CS, [N EE; N CE; T"CS133"];
     EE, [N CSE];
     EE, [N CSE; T"EE102"];
     CSE, [T"CS101"];
     CSE, [N EE];
     CE, [N CS; T"EE115C"]]

let my_awksub_grammar = CE, my_awksub_rules

let my_filter_reachable_test0 = filter_reachable my_awksub_grammar = my_awksub_grammar 
let my_filter_reachable_test1 = filter_reachable (CSE, my_awksub_rules) = 
     (CSE,
       [EE, [N CSE];
        EE, [N CSE; T"EE102"];
        CSE, [T"CS101"];
        CSE, [N EE]])

let my_filter_reachable_test2 = filter_reachable (EE, my_awksub_rules) =
    (EE,
      [EE, [N CSE];
       EE, [N CSE; T"EE102"];
       CSE, [T"CS101"];
       CSE, [N EE]])

let my_filter_reachable_test3 = filter_reachable (CS, List.tl my_awksub_rules) =
   (CS,
     [CS, [N CSE; T"CS131"];
      CS, [N EE; N CE; T"CS133"];
      EE, [N CSE];
      EE, [N CSE; T"EE102"];
      CSE, [T"CS101"];
      CSE, [N EE];
      CE, [N CS; T"EE115C"]])
    

