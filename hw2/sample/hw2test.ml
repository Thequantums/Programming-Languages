let accept_all string = Some string

type brackets_nonterminals = 
  | Circle | Square

let brackets_grammar = 
  (Circle,
    function
      | Circle -> 
        [[T"("; T")"];
         [N Square; T"("; T")"];
         [T"("; N Circle; T")"];
         [T"("; T")"; N Square]]
      | Square -> 
        [[T"["; T"]"];
        [T"["; N Square; T"]"];
         [T"["; T"]"; N Circle];
         [N Circle; T"["; T"]"]])

let frag = ["("; "("; ")"; "["; "]"; "("; ")"; ")"]

(* Problem 5: make_matcher_test *)
let make_matcher_test = 
  ((make_matcher brackets_grammar accept_all frag) = Some [])

(* Problem 6: make_parser_test *)
let make_parser_test = 
  let make_parser_rs = make_parser brackets_grammar frag 
  in match make_parser_rs with
    | None -> false
    | Some tree ->
      if parse_tree_leaves tree = frag then true
      else false