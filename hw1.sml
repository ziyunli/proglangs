(* Assignment 1 *)
Control.Print.printDepth := 20;

fun is_older(date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2 then true else
	if #2 date1 < #2 date2 then true else
	    if #3 date1 < #3 date2 then true else false;

fun number_in_month(dates: (int * int * int) list, month: int) = 0;

fun number_in_months(dates: (int * int * int) list, months: int list) = 0;

fun dates_in_month(dates: (int * int * int) list, month: int) = [];

fun dates_in_months(dates: (int * int * int) list, months: int list) = [];

fun get_nth(items: string list, n: int) = "";

fun date_to_string(date: int * int * int) = "";

fun number_before_reaching_sum(sum: int, numbers: int list) = 0;

fun what_month(day: int) = 0;

fun month_range(day1: int, day2: int) = [];

fun oldest(dates: (int * int * int) list) = NONE;
