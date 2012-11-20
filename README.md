Lazy Rackette
========
A lazy language written in OCaml


Instructions:
=====================
To run the REPL, run "ocaml lazy.ml" (in the lazy directory).

To evaluate a file, run "ocaml read_file *filepath*" which will create the top level bindings for your program and let you use them in a REPL.

Enjoy!

Spec (in progress)
==================
<b>these are identifiers</b>

*these are expression*

*expressions* =

(define (<b>id</b> <b>arg1</b> <b>arg2</b> ...) *body*) 

	- binds func-name to the resulting function (taking in arg1 ...)

	- works only in toplevel

	- introduces global (and therefore recursive) bindings

(define <b>id</b> *expr*) 

	- binds name to expr

	- the previous definition of define desugars to this: (define func-name (lambda (arg1 ...) body))


(lambda (<b>x</b>...) *body*)

	- creates an anonymous procedure

(*func* *arg1* *arg2* ...)

	-evaluates func with the given arguments

	-lazily evaluates arguments on a need basis


(let ((<b>id</b> *val*) (<b>id2</b> *val2*) ...) *body*) 

	- binds identifiers to values in body

(if *cond* *then* *else*)

	- evaluates cond to a boolean, then branches depending on the truth of the result

	- nonbooleans branch based on the result of the primitive procedure "bool"

(and *expr1* *expr2*) 

	- returns the result of (bool expr)^(bool expr)

	- short circuits

(or *expr1* *expr2*)

	- returns (bool expr)|(bool expr)

	- short circuits

(cons *x* *y*) 

	- constructs a pair, normally used to construct lists ending with empty

empty

	- the empty list

(print *expr*)
	
	- strictly evaluates *expr and prints it


Primitive Procedures:
=====================
(binop <b>x</b> <b>y</b>) with binop = + , / , * 
	
	- applies the corresponding OCaml function to the two inputs

(comparator <b>x</b> <b>y</b>) \>, \<
	
	- evaluates OCaml <,> procedures on numeric input

= 
	- equality testing. Polymorphic.

(first *lst*) 
	
	- fetches the first element of the list (car)

(rest *lst*) 
	
	- fetches the rest of the list without the first element (cdr)

(list *x* *y* ...) 
	
	- constructs the nested cons, terminating with an empty

Built in Procedures
=====================
(map *func* *lst1*) 

	- returns the list of func applications on every element of lst1

(map2 *func* *lst1* *lst2*)

	- returns the list of func applications on corresponding elements of lst1 and lst2

	- e.g. (map2 + (list 1 2 3) (list 4 5 6)) -> (list 5 6 7)

(take *n* *lst*)

	- takes the first n elements of the lst

(drop *n* *lst*)

	- drops the first n elements of the lst

(sub1 *n*)

	- returns n-1

(add1 *n*) 

	- returns n+1



Other Notes
========================================================================
To use the eager language Lazy Rackette was based on (Rackette),
run "ocaml repl.ml" in the eager directory.

This language is a bit more sparse than Lazy Rackette, and is substitution based rather than environment based. 





