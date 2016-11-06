(* Problem 1 solution *)
fun is_older (dateA: int * int * int, dateB: int * int * int) =
    if #1 dateA > #1 dateB
    then false
    else if #1 dateA < #1 dateB
    then true
    else
	if #2 dateA > #2 dateB
	then false
	else if #2 dateA < #2 dateB
	then true
	else
	    if #3 dateA >= #3 dateB
	    then false
	    else true

(* Problem 2 solution *)
fun number_in_month (dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else
      let val tl_res = number_in_month(tl dates, month) in
	  if #2 (hd dates) = month
	  then tl_res + 1
	  else tl_res
      end

(* Problem 3 solution *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else
	  number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Problem 4 solution *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else
      let val tl_res = dates_in_month(tl dates, month) in
	   if #2 (hd dates) = month
	   then (hd dates)::tl_res
	   else tl_res
      end
	  
(* Problem 5 solution *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Problem 6 solution *)
fun get_nth(strs: string list, nth: int) =
  if nth = 1
  then hd strs
  else get_nth(tl strs, nth - 1)

(* Problem 7 solution *)
fun date_to_string (date: int * int * int) =
  let val Months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(Months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* Problem 8 solution *)
fun number_before_reaching_sum (sum: int, array: int list) =
  if sum <= 0 orelse null array
  then ~1
  else 1 + number_before_reaching_sum(sum - (hd array), tl array)

(* Problem 9 solution *)
fun what_month(day: int) =
  let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
      number_before_reaching_sum(day, days) + 1
  end

(* Problem 10 solution *)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else
	  what_month(day1) :: month_range(day1 + 1, day2)

(* Problem 11 solution *)
fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else
      let val tl_res = oldest(tl dates) in
	  if isSome tl_res
	  then if is_older(hd dates, valOf tl_res)
	       then SOME (hd dates)
	       else tl_res
	  else SOME (hd dates)
      end
	  
(* Problem 12 Challenge *)
fun remove_duplicate (months: int list) =
  if null months
  then []
  else let fun unique(cur: int, rest: int list) =
	     if null rest
	     then true
	     else if cur = hd rest
	     then false
	     else unique(cur, tl rest)
       in
	   if unique(hd months, tl months)
	   then  hd months :: remove_duplicate (tl months)
	   else  remove_duplicate(tl months)
       end
	   
fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
  let val unique_months = remove_duplicate(months) in
      number_in_months(dates, unique_months)
  end

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  let val unique_months = remove_duplicate(months) in
      dates_in_months(dates, unique_months)
  end

(* Problem 13 Challenge *)
fun increment_element(array: int list, pos: int) =
  if null array
  then []
  else if pos < 1
  then array
  else if pos = 1
  then (hd array + 1) :: increment_element(tl array, pos - 1)
  else hd array :: increment_element(tl array, pos - 1)

fun is_legal_day(d: int, m: int, days: int list) =
  if null days orelse m < 1
  then false
  else if m = 1
  then d >= 1 andalso d <= hd days
  else is_legal_day(d, m - 1, tl days)
	       
fun reasonable_date(date: int * int * int) =
  let val year = #1 date
      val month = #2 date
      val day = #3 date
      val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      fun legal_year(y: int) = if y > 0 then true else false
      fun legal_month(m: int) = if m >= 1 andalso m <= 12 then true else false
      fun is_leap(y: int) = if y mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0) then true else false
  in
      let fun legal_day(y:int, m:int, d:int) =
	if is_leap y
	then let val leap_days = increment_element(days, 2) in
		 is_legal_day(d, m, leap_days)
	     end
	else is_legal_day(d, m, days)
      in
	  legal_year(year) andalso legal_month(month) andalso legal_day(year, month, day)
      end
  end
      
