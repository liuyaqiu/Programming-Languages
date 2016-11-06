(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem 1-a solution *)
fun all_except_option (str, lst) =
  let fun except (str, []) = []
	| except (str, head::rest) = if same_string(str, head) then rest
				     else head::except(str, rest)
  in
      let val res = except(str, lst) in
	  if res = lst
	  then NONE
	  else SOME res
      end
  end

(* Problem 1-b solution *)
fun get_substitutions1 (substitutions: string list list, s: string) =
  case substitutions of
      [] => []
    | head::tail => let val cur = all_except_option(s, head) in
			case cur of
			    NONE => get_substitutions1(tail, s)
			  | SOME value => value @ get_substitutions1(tail, s)
		    end
			
(* Problem 1-c solution *)
fun get_substitutions2 (subs: string list list, s: string) =
  let fun tail_rec (cur, res) =
      case cur of
	  [] => res
	| head::tail => let val tmp = all_except_option(s, head) in
			    case tmp of
				NONE => tail_rec(tail, res)
			      | SOME value => tail_rec(tail, res @ value)
			end
  in
	  tail_rec(subs, [])
  end
      
(* Problem 1-d solution *)
fun similar_names (subs, name) =
  let fun replaceFirst (rep, {first=fir, middle=mid, last=las}) =
	{first=rep, middle=mid, last=las}
  in
    let fun getRes ([], name) = []
	| getRes (head::tail, name) = replaceFirst(head, name)::getRes(tail, name)
    in
	case name of
	    {first=fir, middle=mid, last=las} => getRes(fir::get_substitutions2(subs, fir), name)
    end
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

(* Problem 2-a solution *)
fun card_color card =
  case card of
      (Clubs, rank) => Black
    | (Spades, rank) => Black
    | _ => Red

(* Problem 2-b solution *)
fun card_value card =
  case card of
      (suit, Num n) => n
    | (suit, Ace) => 11
    | (suit, _) => 10

(* Problem 2-c solution *)
fun remove_card (cs: card list, c: card, e) =
  let fun remove (cs, c) =
	case cs of
	    [] => []
	  | head::tail => if head = c then tail
			  else head::remove(tail, c)
  in
      let val res = remove(cs, c) in
	  if res = cs
	  then raise e
	  else res
      end
  end
      
(* Problem 2-d solution *)
fun all_same_color (cs: card list) =
  case cs of
      h1::(h2::rest) => card_color(h1) = card_color(h2) andalso all_same_color(h2::rest)
    | _ => true

(* Problem 2-e solution *)
fun sum_cards (cs: card list) =
  let fun tail_rec (cs, res) =
	case cs of
	    [] => res
	  | head::tail => tail_rec(tail, res + card_value(head))
  in
      tail_rec(cs, 0)
  end
      
(* Problem 2-f solution *)
fun score (cs: card list, goal: int) =
  let val sum = sum_cards(cs) in
      let val pre1 = (sum - goal) * 3; val pre2 = (goal - sum) in
	  if sum >= goal
	  then if all_same_color(cs)
	       then pre1 div 2
	       else pre1
	  else
	      if all_same_color(cs)
	      then pre2 div 2
	      else pre2
      end
  end

(* Problem 2-g solution *)
fun officiate (cs: card list, mv: move list, goal: int) =
  let fun helper (cs: card list, hold: card list, mv: move list, goal: int) =
	case mv of
	    [] => score(hold, goal)
	  | head::tail => case head of
			      Discard card => helper(cs, remove_card(hold, card, IllegalMove), tail, goal)
			    | Draw => case cs of
					  [] => score(hold, goal)
					| h::t => if sum_cards(h::hold) > goal
						  then score(h::hold, goal)
						  else helper(t, h::hold, tail, goal)
  in
      helper(cs, [], mv, goal)
  end
