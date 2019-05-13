(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun curry f x y = f (x, y)

fun uncarry f (x, y) = f x y

fun other_curry f x y = f y x
			  
fun only_capitals strs =
    List.filter (fn s => Char.isUpper ((curry String.sub) s 0)) strs

fun longest_string1 strs =
    List.foldl (fn (x, l) => if String.size x > String.size l then x else l) "" strs

fun longest_string2 strs =
    List.foldl (fn (x, l) => if String.size x >= String.size l then x else l) "" strs

fun longest_string_helper f strs =
    List.foldl (fn (x, l) => if f(String.size x, String.size l) then x else l) "" strs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v 

fun all_answers f xs =
    let
	fun all_answers_acc f xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => all_answers_acc f xs' (acc @ v)
    in
	all_answers_acc f xs []
    end

fun count_wildcards p =
    g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
    g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (s, p) =
    g (fn _ => 0) (fn x => if String.compare(s, x) = EQUAL then 1 else 0) p

fun check_pat p =
    let
	fun all_variables p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (x, i) => i @ (all_variables x)) [] ps
	      | _ => []
			 
	fun is_repeated xs =
	    case xs of 
		[] => false
	      | c::[] => false 
	      | c::xs' => if List.exists (fn x => String.compare (x, c) = EQUAL) xs'
			  then true
			  else is_repeated xs'
		
    in
	not (is_repeated (all_variables p))
    end
    		  
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => if v = Unit then SOME [] else NONE
      | ConstP c => if v = Const c then SOME [] else NONE
      | TupleP ps => (case v of
			  Tuple vs => if List.length ps = List.length vs
				      then all_answers match (ListPair.zip(vs, ps))
				      else NONE
			| _ => NONE 
		     )
      | ConstructorP(s1, p1) => (case v of
				     Constructor(s2, v2) => if s1 = s2 then match(v2, p1) else NONE
				   | _ => NONE)
				    
fun first_match v ps =
    SOME(first_answer (curry match v) ps) handle NoAnswer => NONE

fun typecheck_patterns ps = 
