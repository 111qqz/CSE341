fun is_older(date_lhs:int*int*int,date_rhs:int*int*int ) =
    if #1 date_lhs < #1 date_rhs then true else 
    if #2 date_lhs < #2 date_rhs then true else 
    if #3 date_lhs < #3 date_rhs then true else false 


fun number_in_month(date_lst:(int*int*int) list, month:int) = 
    if null date_lst 
    then 0
    else let val ret = if  #2 (hd date_lst) = month then 1 else 0 
         in  ret + number_in_month(tl date_lst,month)
         end 


fun number_in_months(date_lst:(int*int*int)list,month_lst:int list) =
    if null month_lst 
    then 0
    else number_in_month(date_lst,hd month_lst) + number_in_months(date_lst, tl month_lst)

fun dates_in_month(date_lst:(int*int*int) list, month:int ) = 
    if null date_lst 
    then []
    else let val fst_date = hd date_lst 
         in
         if #2 fst_date = month 
         then fst_date::dates_in_month(tl date_lst,month)
         else dates_in_month(tl date_lst,month)
         end

fun dates_in_months (date_lst:(int*int*int)list,month_lst:int list) =
    if null month_lst
    then []
    else dates_in_month(date_lst, hd month_lst)  @ dates_in_months(date_lst, tl month_lst)


(* what if n <= 0?  *)
fun get_nth(lst:string list, n:int) = 
    if n = 1
    then hd lst
    else get_nth(tl lst,n-1)

fun date_to_string(date: int*int*int) = 
    let val month_lst = ["January", "February", "March", "April",
            "May", "June", "July", "August", "September", "October", "November", "December"]
        val year = #1 date
        val month = #2 date
        val day =  #3 date 
    in 
        get_nth(month_lst,month)^" "^Int.toString(day)^", "^Int.toString(year)
    end
       


fun number_before_reaching_sum(sum:int,lst:int list) =
    if sum <= 0 
    then ~1
    else 1 + number_before_reaching_sum(sum - (hd lst),tl lst)

fun what_month(day:int) = 
    let val day_lst = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(day,day_lst) + 1
    end 

fun month_range(day1:int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

(* fun oldest(date_lst:(int*int*int) list) = 
    if null date_lst
    then NONE
    else 
        let ret = oldest(tl date_lst)
        in 
            if 


 *)



