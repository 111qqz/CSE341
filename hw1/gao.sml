fun is_older(date_lhs:int*int*int,date_rhs:int*int*int ) =
 if #1 date_lhs < #1 date_rhs then true else 
 if #2 date_lhs < #2 date_rhs then true else 
 if #3 date_lhs < #3 date_rhs then true else false 
