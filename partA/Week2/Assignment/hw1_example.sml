fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
        (* break slightly hard to read tuples into easy to parse paires *)
        val year = (#1 date1, #1 date2);
        val month = (#2 date1, #2 date2);
        val day = (#3 date1, #3 date2);
    in
        (* look if the paires are equal. If they are not return the result of the expression *)
        if #1 year = #2 year then
            if #1 month = #2 month then
                #1 day < #2 day
            else
                #1 month < #2 month
        else
            #1 year < #2 year
    end;

(* return the amount of dates occur in a month *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates then
        0
    else if #2 (hd dates) = month then
        1 + number_in_month(tl dates, month)
    else
	number_in_month(tl dates, month);

(* iterate over the list months and do number_in_month on all values  *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months then
        0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months);

(* Same as number_in_month but returns a list with tuples instead *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates then
        []
    else if #2 (hd dates) = month then
        hd dates :: dates_in_month(tl dates, month)
    else
        dates_in_month(tl dates, month);
(* see above *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months then
        []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);

(* gets the nth element in the list *)
fun get_nth (strings : string list, n : int) =
    if n=1 then
        hd strings
    else
        get_nth(tl strings, n-1);

(* Turns a tuple with the date into a string (ie. December 4, 1998) *)
fun date_to_string (date : int*int*int) =
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November",
                      "December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end;

(* Fun fact: I misunderstand this question so badly that I wrote four different versions of this.
   One of them even had the final version as a helper function, so that is fun. *)
fun number_before_reaching_sum (sum : int, numbers : int list) = 
    if sum - hd numbers <= 0 then 
	0
    else
	1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

(* Returns the month in ints that the date is in. *)				      
fun what_month (day : int) =
    let
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    in
	number_before_reaching_sum(day, days) + 1
    end;
(* Does what_month for all values between day1 and day2 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then
	[]
    else
	what_month(day1) :: month_range(day1+1, day2);

(* Returns the oldest date in a list *)
fun oldest (dates : (int*int*int) list) =
    let
	fun oldest_nonempty(dates : (int*int*int) list) =
	    if null (tl dates) then
		hd dates
	    else
		let
		    val tl_ans = oldest_nonempty(tl dates)
		in
		    if is_older(hd dates, tl_ans) then
			hd dates
		    else
			tl_ans
		end
    in
	if null dates then
	    NONE
	else
	    SOME(oldest_nonempty(dates))
    end;

(* Removes all duplicate values in a date list *)
fun remove_existing(dates : (int*int*int) list, months : int list) =
    let
	(* Returns true if the date exists in the list and false other wise *)
	fun check_if_date_exists(date : int*int*int, existing : (int*int*int) list) =
	    if null existing then
		false
	    else if date = hd existing then
		true
	    else
		check_if_date_exists(date, tl existing);

	fun check_if_month_exists(month : int, existing : int list) =
	    if null existing then
		false
	    else if month = hd existing then
		true
	    else
		check_if_month_exists(month, tl existing)

	fun remove_existing_dates(dates : (int*int*int) list,
				  existing : (int*int*int) list) =
	    if null dates then
		existing
	    else if check_if_date_exists(hd dates, existing) then
		remove_existing_dates(tl dates, existing)
	    else
		remove_existing_dates(tl dates, hd dates::existing)

	fun remove_existing_months(months : int list, existing : int list) =
	    if null months then
		existing
	    else if check_if_month_exists(hd months, existing) then
		remove_existing_months(tl months, existing)
	    else
		remove_existing_months(tl months, hd months::existing)
		    
    in
	(remove_existing_dates(tl dates, [hd dates]),
	 remove_existing_months(tl months, [hd months]))
    end

(* Applies remove_existing to number_in_months and dates_in_months *)
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
	val no_dupes_pair = remove_existing(dates, months)
    in
	number_in_months(#1 no_dupes_pair, #2 no_dupes_pair)
    end;


fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
	val no_dupes_pair = remove_existing(dates, months)
    in
	1	dates_in_months(#1 no_dupes_pair, #2 no_dupes_pair)
    end;


fun reasonable_date (date : int*int*int) =
    let
	(* pairs with the name of the month and the amount of days in a common year *)
	val common_year_pair = [("January", 31), ("February", 28), ("March", 31),
				("April", 30), ("May", 31), ("June", 30), ("July", 31),
				("August", 31), ("September", 30), ("October", 31),
				("November", 30), ("December", 31)]

	(* written with the help of psuedocode on wikipedia *)
	fun is_leap_year (year : int) =
	    if true <> (year mod 4 = 0) then
		false
	    else if true <> (year mod 100 = 0) then
		true
	    else if true <> (year mod 400 = 0) then
		false
	    else
		true
		    
	(* Looks up how many days there were in a given month *)
	fun look_up_days (month : int, date_list : (string*int) list) =
	    if month=1 then
		#2 (hd date_list)
	    else
		look_up_days(month-1, tl date_list)
			    
	(* helper function to reduce if condition size *)		    
	fun between_x_y (min : int, max : int, lookup : int) =
	    lookup >= min andalso lookup <= max

	(* UoW changed the format on us again.*)
	val day = #3 date;
	val month = #2 date;
	val year = #1 date;
    in
	if year < 1 then
	    false
	else if true <> (between_x_y(1, 12, month)) then
	    false
	else if month = 2 andalso is_leap_year(year) then
	    between_x_y(1, look_up_days(month, common_year_pair)+1, day)
	else
	    between_x_y(1, look_up_days(month, common_year_pair), day)
    end;

