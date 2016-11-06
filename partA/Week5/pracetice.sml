val x = 50
val y = 3
val z = 10
val f = fn z => z
val a =
    let val x = 3 * x
	val z = y * z
    in x * z
    end
fun f x z = x + y + z
			
fun foo f x y z = 
	if x >= y
	then (f z)
	else foo f y x (tl z)
		 
fun baz f a b c d e = (f (a ^ b))::(c + d)::e
						
fun maybeEven x = 
	if x = 0 
	then true
	else
	if x = 50
	then false
	else (print "Call maybeOdd\n"; maybeOdd (x-1))

and maybeOdd y =
	if y = 0
	then false
	else 
	if y = 99
	then true
	else (print "Call maybeEven\n"; maybeEven (y-1))

signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end
		       
structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end


    
