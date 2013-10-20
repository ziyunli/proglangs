(* Dan Grossman, Coursera PL, HW2 Provided Code *)
Control.Print.printDepth := 20;

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, lst) =
    let fun delete(acc, lst) =
	    case lst of
		[] => NONE
	      | curr::remine => if same_string(str, curr)
				then SOME(acc @ remine)
				else delete(curr::acc, remine)
    in
	delete([], lst)
    end;
	
fun get_substitutions1(str_lists, s) =
    case str_lists of
	[] => []
      | hd_lst::remine =>
	let
	    val res =
		case all_except_option(s, hd_lst) of
		    NONE => []
		  | SOME lst => lst 
	in
	    res @ get_substitutions1(remine, s)
	end

fun get_substitutions2(str_lists, s) =
    case str_lists of
	[] => []
      | hd_lst::remine =>
	let
	    fun substitutions_acc(acc, lists) =
		case lists of
		    [] => acc
		  | hd_lst::remine =>
		    let
			val res = 
			    case all_except_option(s, hd_lst) of
				NONE => []
			      | SOME lst => lst
		    in
			substitutions_acc(res @ acc, remine)
		    end
	in
	    substitutions_acc([], str_lists)
	end;
		  
			    
		      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
