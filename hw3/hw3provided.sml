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
