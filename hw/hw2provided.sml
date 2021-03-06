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
	      | curr::remain => if same_string(str, curr)
				then SOME(acc @ remain)
				else delete(curr::acc, remain)
    in
	delete([], lst)
    end
	
fun get_substitutions1(str_lists, s) =
    case str_lists of
	[] => []
      | hd_lst::remain =>
	let
	    val res =
		case all_except_option(s, hd_lst) of
		    NONE => []
		  | SOME lst => lst 
	in
	    res @ get_substitutions1(remain, s)
	end

fun get_substitutions2(str_lists, s) =
    case str_lists of
	[] => []
      | hd_lst::remain =>
	let
	    fun substitutions_acc(acc, lists) =
		case lists of
		    [] => acc
		  | hd_lst::remain =>
		    let
			val res = 
			    case all_except_option(s, hd_lst) of
				NONE => []
			      | SOME lst => lst
		    in
			substitutions_acc(acc @ res, remain)
		    end
	in
	    substitutions_acc([], str_lists)
	end

type full_name = {
    first : string,
    middle: string,
    last: string }

fun similar_names(str_lists, fullname) =
    let
	val { first = x, middle = m, last = y } = fullname
	fun generate_fullname(subsitutions) =
	    case subsitutions of
		[] => []
	      | hd::tl => { first = hd, middle = m, last = y } :: generate_fullname(tl)
    in
	fullname :: generate_fullname(get_substitutions2(str_lists, x))
    end
					    
		      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card) =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_value(card) =
    case card of
	(_, Ace) => 11
      | (_, Num x) => x
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | hd::tl => if hd = c then tl else hd::remove_card(tl, c, e)

fun all_same_color(cs) =
    case cs of
	[] => true
     | hd::[] => true
     | hd::sc::tl => if card_color hd = card_color sc
		     then all_same_color(sc::tl)
		     else false

fun sum_cards(cs) =
    let
	fun sum_cards_acc(acc, list) =
	    case list of
		[] => acc
	      | hd::tl => sum_cards_acc(acc + card_value hd, tl)
    in
	sum_cards_acc(0, cs)
    end

fun score(held_cards, goal) =
    let
	val sum = sum_cards(held_cards)
	val is_same_color = all_same_color(held_cards)
	val preliminary_score = if sum > goal
				then 3 * (sum - goal)
				else goal - sum
    in
	if is_same_color
	then preliminary_score div 2
	else preliminary_score
    end
	
fun officiate(card_list, move_list, goal) =
    let
	fun take_turn(curr_cards, curr_hands, moves) =
	    if sum_cards curr_hands > goal
	    then score(curr_hands, goal)
	    else
		case moves of
		    [] => score(curr_hands, goal)
		  | Discard(card)::remain_moves => take_turn(curr_cards, remove_card(curr_hands, card, IllegalMove), remain_moves)
		  | Draw::remain_moves => case curr_cards of
					      [] => score(curr_hands, goal)
					    | hd_card::remain_cards => take_turn(remain_cards, hd_card::curr_hands, remain_moves)		    
    in
	take_turn(card_list, [], move_list)
    end
    
fun score_challenge(held_cards, goal) =
    let
	val is_same_color = all_same_color(held_cards)

	fun calculate_score_from_sum(sum) =
	    let
		val prel_score = if sum > goal
				 then 3 * (sum - goal)
				 else goal - sum
	    in
		if is_same_color
		then prel_score div 2
		else prel_score
	    end
	    
	fun find_optimal_score_acc(acc, cards) =
	    case cards of
		[] => calculate_score_from_sum(acc)
	      | (_, Ace)::remain_cards => Int.min(find_optimal_score_acc(acc + 1, remain_cards), find_optimal_score_acc(acc + 11, remain_cards))
	      | hd_card::remain_cards => find_optimal_score_acc(acc + card_value hd_card, remain_cards)
    in
	find_optimal_score_acc(0, held_cards)
    end

fun officiate_challenge(card_list, move_list, goal) =
    let
	fun sum_cards_challenge(cs) =
	    let
		fun sum_cards_acc(acc, list) =
		    case list of
			[] => acc
		      | (_, Ace)::tl => sum_cards_acc(acc + 1, tl)
		      | card::tl => sum_cards_acc(acc + card_value card, tl) 
	    in
		sum_cards_acc(0, cs)
	    end
	
	fun take_turn(curr_cards, curr_hands, moves) =
	    if sum_cards_challenge curr_hands > goal
	    then score_challenge(curr_hands, goal)
	    else
		case moves of
		    [] => score_challenge(curr_hands, goal)
		  | Discard(card)::remain_moves => take_turn(curr_cards, remove_card(curr_hands, card, IllegalMove), remain_moves)
		  | Draw::remain_moves => case curr_cards of
					      [] => score_challenge(curr_hands, goal)
					    | hd_card::remain_cards => take_turn(remain_cards, hd_card::curr_hands, remain_moves)		    
    in
	take_turn(card_list, [], move_list)
    end   

fun careful_player(cs, goal) =
    let
	fun take_turn(curr_cards, curr_hands) =
	    let
		val sum = sum_cards(curr_hands)
				   
		fun find_card(cs, num) =
		    case cs of
			[] => NONE
		      | (s, Ace)::tl => if num = 11 then SOME(s, Ace) else find_card(tl, num) 
		      | (s, Num x)::tl => if num = x then SOME(s, Num x) else find_card(tl, num)
		      | card::tl => if num = 10 then SOME card else find_card(tl, num)
	    in
		case curr_cards of
		    [] => []
		  | hd_card::remain_cards =>
		    let
			val card = find_card(curr_hands, sum + card_value hd_card - goal)
		    in
			case card of
			    SOME card => [Discard card, Draw]
			  | NONE => if sum + 10 < goal
				    then Draw::take_turn(remain_cards, hd_card::curr_hands)
				    else []
		    end
	    end
    in
	take_turn(cs, [])
    end						
								     
