(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(st, lst) = 
    let fun helper(acc) = 
        case acc of 
            [] => []
        |   x::xs' => if same_string(x,st) then helper(xs') else x::helper(xs')
        val ret = helper(lst)
    in 
        if ret = lst then NONE else SOME ret
    end


fun get_substitutions1(st_lst_lst,st) = 
    case st_lst_lst of 
        [] => [] 
    |  x::xs' =>let val ret = all_except_option(st,x) 
                in 
                    case ret of 
                       NONE => get_substitutions1(xs',st)
                    |  SOME y => y @ get_substitutions1(xs',st)
                end 


fun get_substitutions2(st_lst_lst,st) = 
    let 
        fun helper(st_lst_lst,st,acc) = 
            case st_lst_lst of
              [] => acc 
            | x::xs' =>  let val ret =  all_except_option(st,x) 
                         in 
                             case ret of 
                                NONE => helper(xs',st,acc)
                            |   SOME y => helper(xs',st,acc @ y)
                         end 
    in  
        helper(st_lst_lst,st,[])
    end 
            


fun similar_names(st_lst_lst, {first=x,middle=y,last=z}) = 
let 
    val ret = get_substitutions2(st_lst_lst,x)

    fun helper(name_lst)  =
        case name_lst of 
            []  => []
        |   x'::xs' => {first=x',middle=y,last=z}::helper(xs')
in 
    {first=x,middle=y,last=z}::helper(ret)
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


fun card_color(in_suit,in_rank) = 
    case in_suit of
        Clubs => Black 
    |   Spades => Black 
    |     _  => Red 


fun card_value(in_suit,in_rank) = 
    case in_rank of 
        Ace => 11
    |   Num x => x 
    |   _     => 10


fun remove_card(cs,c,e) = 
    let fun helper(acc,flag) = 
        case acc of 
            [] => []
        |   x::xs' => if flag andalso x=c then helper(xs',false) else x::helper(xs',flag)
        val ret = helper(cs,true)
    in 
        if ret = cs then raise e  else ret 
    end


fun all_same_color(card_lst) =
    case card_lst of 
        [] => false 
    |   _::[] => true 
    |   (headSuit,headRank)::((neckSuit,neckRank)::rest) 
        => (headSuit = neckSuit andalso all_same_color((neckSuit,neckRank)::rest) )

