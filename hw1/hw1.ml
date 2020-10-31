(*return true if e is in list b else otherwise*)
let rec is_in set e =
    match set with
    | [] -> false
    | hd :: tl ->
      if hd = e then true else is_in tl e
;;

(*Problem 1: return true iff a is a subset of b else otherwise*)

let rec subset a b =
    match a with
    | [] -> true
    | hd :: tl ->
      if (is_in b hd) = true then subset tl b else false
;;

(*Problem 2: return true if set a and b are equal otherwise false*)

let equal_sets a b = 
    if subset a b = true && subset b a = true then true else false
;;

(*Problem 3: return a list union of a & b*)

let rec set_union a b =
    match a with
    | [] -> b
    | hd :: tl -> if (is_in b hd) then (set_union tl b) else (set_union tl (hd::b))
;;

(*Problem 4: return a intersection list of a b *)
let rec set_intersection a b = 
    match a with
    | [] -> []
    | hd :: tl -> if (is_in b hd) then (hd :: set_intersection tl b) else (set_intersection tl b)
;; 

(* problem 5: return a list that is a set of a - b *)
let rec set_diff a b =
    match a with
    | [] -> []
    | hd :: tl -> if (is_in b hd) then (set_diff tl b) else (hd :: set_diff tl b )
;;

(*Problem 6: write a function computed_fixed_point eq f x *)
let rec computed_fixed_point eq f x =
    if (eq (f x) x ) then x
    else computed_fixed_point eq f (f x)
;;


(*Problem 7: a function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed*)

let rec print_list = function
|[] -> ()
| hd :: tl -> print_int hd; print_string " "; print_list tl
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

(* get right hand side non-terminal only and discard terminal values *)
let rec get_rst right_side =
    match right_side with
    | [] -> []
    | T hd :: tl -> get_rst tl
    | N hd :: tl -> hd :: ( get_rst tl)
;;

(* recursivly build up non-terminal list iterate through n; might miss some reachable rules because it passed *) 
let rec reachable_nt nt rule =
    match rule with
    | [] -> nt
    | hd :: tl ->
      if is_in nt (fst hd) then begin
	 let list_nt = set_union nt (get_rst (snd hd)) in
             reachable_nt list_nt tl
      end
      else begin
         reachable_nt nt tl
      end
;;

   
(* recursivly build up non-terminal list that is reachable; iterate until nt is not changed and to get complete non-terminal list *)
let rec reachable_list_nt nt rule = 
    let list_nt = reachable_nt nt rule in
	if equal_sets list_nt nt then begin
	   nt
        end
        else begin
           (*reiterate through the rule again to get all reachable non terminals*)
            reachable_list_nt list_nt rule 
        end 
            
    
(*if rule has non_terminal value matched to one of the values ins list_nt, then retain it else discard it.*)
let rec filter_unreach list_nt rule = 
    match rule with
    | [] -> []
    | hd::tl ->
      let rule_nt = fst hd in
         if (is_in list_nt rule_nt) then begin
	     hd :: filter_unreach list_nt tl
	 end
	 else begin
             filter_unreach list_nt tl
	 end
;;  


(*left side of grammer is non_terminal start; right side of grammer is a list of rules
Use filter function to filter non reachable rules from passing rule*)
let filter_reachable grammer =
    let nt = (fst grammer :: []) in
    let rule = snd grammer in 
    let list_nt = reachable_list_nt nt rule in
       (fst grammer, filter_unreach list_nt rule)
;;

         

 
  
   
 
