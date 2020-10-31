let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type major_nonterminals = | CS | EE | CSE
let major_grammar = (CS, function | CS -> [ [T"cs131"; T"cs130"]; [T"cs131";T"cs130"; N EE]; [T"cs131"; N CS; T"cs130"]; [N EE; T"cs131"; T"cs130"]]
                                  | EE -> [ [T"ee183"; T"ee182"]; [T"ee183";T"ee182"; N CS]; [T"cs183"; N EE; T"ee182"]; [N CS; T"ee183"; T"ee182"]]
                                  | CSE -> [ [T"cse1"; T"cse2"; N CS]; [T"cse1";N CSE;T"cse2"]]
                      )

let frag = ["cs131"; "cs131"; "cs130"; "ee183"; "ee182";
            "cs131"; "cs130"; "cs130" ] 

(*Test case: make_matcher*)
let make_matcher_test = ( make_matcher major_grammar accept_all frag = Some [])

(*Test Case: make_parser*)

let make_parser_test = match make_parser major_grammar frag with
| None -> false
| Some tree -> if parse_tree_leaves tree = frag then true else false
