(*problem 1: convert homework 1 style grammar to homework 2-style grammar*)

type ('nonterminal, 'terminal) symbol =
   | T of 'terminal
   | N of 'nonterminal
   
(*return alternative list *)
let rec get_alt_list rule nonterminal = 
    match rule with
    | [] -> []
    | hd::tl ->
 	    if (fst hd) = nonterminal then begin
		    (snd hd)::(get_alt_list tl nonterminal)
		end
        else begin
		    get_alt_list tl nonterminal
		end
;;

let convert_grammar grammar =
  let s_sym = fst grammar in
  let rule = snd grammar in 
  (s_sym, get_alt_list rule);;
   
(*Problem 2 : function that traverse throught parse tree*)
type ('nonterminal, 'terminal)parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*traverse throught the list*)
(*
let rec parse_tree_leaves tree =
   match tree with 
   | [] -> []
   | hd :: tl ->
      match hd with
	  | Node (nonterminal, sub_t) -> (parse_tree sub_t ) :: (parse_tree tl )
	  | Leaf leaf -> leaf :: (parse_tree tl)
	
;;
  *)
let rec parse_t tree = match tree with
    | [] -> []
    | hd::tl ->
     match hd with 
(* Traverse subtree of that nonterminal first because this is left to right rule. Then travser tail*)
	     | Node (nonterminal, sub_tree) -> (parse_t sub_tree ) @ (parse_t tl )
	     | Leaf leaf -> leaf :: (parse_t tl)
;;

let parse_tree_leaves tree = parse_t (tree :: []);;
 

(* Problem 3: make matcher *)

let rec real_matcher production_function s_sym rule accept frag = match rule with  
  | [] -> None  (*no more rule in alternative list to match, so return non*)
  | hd::tl -> 
       (*There is a first rule to find match for*)
      let match_hd = match_current_rule production_function hd accept frag 
      in match match_hd with
       (*if the first rule does not have prefix match - try other rule in alternative lists*)
        | None -> real_matcher production_function s_sym tl accept frag
        (*If match the first rule, then return whatever the acceptor returns*)
        | _ -> match_hd 
(*This is the function to iterate throught the elements in the lust*)
and match_current_rule production_function rule accept frag = match rule with
 (*if rule is empty just return what acceptor return with frag as argument*)
  | [] -> accept frag
(*If rule has a Nonterminal element, then finding the prefix match for *)
  | (N n_t)::tl -> 
     (*shift the suffix the acceptor accept by scanning to the tail element in the current list*)
      let s_accept = match_current_rule production_function tl accept
      (*recursivly call real_matcher with alternative list from non_terminal*)
      in real_matcher production_function n_t (production_function n_t) s_accept frag
(*If element  in the rule is a terminal then match with frag*)
  | (T t)::tl -> ( match frag with
      (*There is no more frag to try to match for*)
      | [] -> None
      (*There is a frag to match*)
      | f_hd::f_tl -> 
          (*if terminal match with the current frag then try to match the rest of the frag, if terminal doesnt
    match with the frag then return None *)
        if f_hd = t then match_current_rule production_function tl accept f_tl else None);;  

let make_matcher grammar accept frag = 
let product_fun = snd grammar in
let start_rule = product_fun (fst grammar) in
real_matcher product_fun (fst grammar) start_rule accept frag;;

(*Problem 4: make_parser *)

let accept_only_empty suffix = match suffix with
| _::_ -> None
| x -> Some []
;;

let rec matcher_parser product_fun s_symbol rule acceptor frag = match rule with
(*No rule in current alternative rule to match, return none*)
| [] -> None
| hd::tl ->  
 (*matching first rule in alternative list*)
 (*ssym_rule is a tuple of nonterminal and alternative rule of that nonterminal *)
  let match_hd = current_matcher2 product_fun hd acceptor frag in
   match match_hd with
  (*First rule in alternative list doesnt match, so try the second one*)
  | None -> matcher_parser product_fun s_symbol tl acceptor frag
  (*First rule match with the prefix, return what the acceptor returns*)
  | Some result_rule -> Some (hd::result_rule)
 
(*Traverse throught the rule list and fine match for*) 
and current_matcher2 product_fun rule acceptor frag =
   match rule with
   (*if no more rule, return what the acceptor returns*)
   | [] -> acceptor frag
   (*If elemen in the rule is a non_terminal,*)
   | (N n_t) :: tl -> 
   (*Shift the suffix the acceptor will a*)
     let s_accept = current_matcher2 product_fun tl acceptor
     in matcher_parser product_fun n_t (product_fun n_t) s_accept frag
   (*If hd is non_terminal, then match it with the frag*)
   | (T t) :: tl -> match frag with
       (*If there is no more frag, then return none*)
      | [] -> None
      | f_hd :: f_tl ->
        if f_hd = t then current_matcher2 product_fun tl acceptor f_tl else None 
;;

let rec compute_parse_tree nt_root rules = 
match nt_root with 
| [] -> (rules, []) 
| hd::tl -> (match construct_path hd rules with (*construct path from this node to others *)
    | (r,cur_ntl) -> (match compute_parse_tree tl r with 
        | (x,tre) -> (x, cur_ntl::tre))) 
and construct_path nt_root rules =
match rules with 
(*If there is no more element in rule*)
| [] -> (match nt_root with
(*construct the leaf terminal*)
          | (T t) -> ([], Leaf t)
(*if there is a nonterminal, then construct the nontermial node*)
          | (N nt) -> ([], Node(nt,[])))
(*If there is rule in the element*)
| hd :: tl -> (match nt_root with
(*If there is a element in the rule and the rule is a leaf, then tuple
 consist of all rules and the leaf t*)
                 | (T t) -> (hd::tl, Leaf t)
(*If there the element in the rule is a nonterminal, then find the subtree
 and construct the nodes*)
                 | (N nt) -> ( match compute_parse_tree hd tl with
                               | (x,tre) -> (x, Node(nt, tre)))  )		  

;;


let make_parser grammar frag =
   let product_fun = snd grammar in
   let start_rule = product_fun (fst grammar) in
     match matcher_parser product_fun (fst grammar) start_rule accept_only_empty frag with
     | None -> None
     | Some b_rule -> (match b_rule with
       | [] -> None
       | b_r -> (  
       (* we can make parse tree *)
       let nt_root = [N (fst grammar)] in
        match compute_parse_tree nt_root b_r with 
	   | (_,tree) -> (match tree with
                          | hd :: tl -> Some hd
                          | [] -> None ) ))
;;  










