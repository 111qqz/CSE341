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


fun only_capitals (st_lst) =
    List.filter (fn st => Char.isUpper(String.sub(st,0)) ) st_lst


fun longest_string1(st_lst) = 
    List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y )  ""  st_lst

fun longest_string2(st_lst) = 
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y )  ""  st_lst

fun longest_string_helper f st_lst   = 
    List.foldl (fn (x,y) => if f(String.size(x),String.size(y) ) then x else y )  ""  st_lst

val  longest_string3 = longest_string_helper (fn (x,y) => x>y)
val  longest_string4 = longest_string_helper (fn (x,y) => x>=y)

val longest_capitalized  =  longest_string1 o only_capitals

val rev_string  =  String.implode o List.rev o String.explode 


fun first_answer f lst = 
	case lst of 
		[]  => raise  NoAnswer
	|	x::xs => let 
					val ret = f(x)
				 in  
					if isSome ret then valOf ret else first_answer f xs 
				 end 

fun all_answers f lst =
	let fun helper lst acc = 
		case lst of 
			[] => SOME acc 
		|   SOME(x)::xs => helper xs (acc @ x)
        |   NONE::xs  => NONE 
	in 
        helper (map f lst)  []
	end  

(* val test8a = all_answers  (only_capitals) ["this","list","has"] *)

val count_wildcards =  g (fn () => 1)  (fn _ => 0 )

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (st,p) = 
    g (fn() => 0) (fn(x) => if x=st then 1 else 0) p


fun check_pat p = 
    let 
        fun get_all_sts p = 
            case p of
              Variable x        => [x]
            | TupleP ps         => List.foldl (fn (p,i) => (get_all_sts p) @ i) [] ps
            | ConstructorP(_,p) => get_all_sts p
            | _                 => []
        fun has_repeats st_lst =
            case st_lst of
                [] => false 
            |   x::xs => List.exists(fn y => y=x ) xs orelse  has_repeats xs  
    in 
        (not o has_repeats o get_all_sts) p 
    end

fun match (v,p) = 
    case (p,v) of 
        (Wildcard,_) =>  SOME []
    |   (Variable s,_) => SOME [(s,v)]
    |   (UnitP,Unit) => SOME []
    |   (ConstP x,Const y) => if x=y then SOME [] else NONE 
    |   (TupleP ps,Tuple vs) => if List.length ps = List.length vs 
                                then all_answers match (ListPair.zip(vs,ps))
                                else NONE 
    |   (ConstructorP(s1,p),Constructor(s2,v)) => if s1 = s2 then match(v,p) else NONE 
    |   _ => NONE


                        
fun first_match v ps = 
    ( SOME(first_answer (fn p => match(v,p)) ps) ) handle NoAnswer => NONE







