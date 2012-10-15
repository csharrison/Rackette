Rackette
========
Charlie Harrison and Mengjie (Kelly) Wang
A substitution based interpreter for the Racket language in OCaml


instructions for use:
=====================

An end-user comes in and types  **doit "EXPRESSION"** ,where EXPRESSION is a valid Rackette expression. For example, if he is going to type 
			(let ((x 10)) (if (= x 20) 45 x))    in Racket, 
in Rackette he should type 
			doit "(let ((x 10)) (if (= x 20) 45 x))" ;;
And the result will evaluate to 10.



Expanations
========================================================================

- parse: parse takes in a quotedSyntax expression, and translates it into abstractSyntax.
Base cases translate directly into abstractSynax whereas list cases (e.g. if and let)
have to be parsed recursively. That is, every element of the list must be parsed down
recursively so everything, including function arguments, are in abstractSyntax

-subst: subst takes in an expression, and old symbol to be replaced, and a new value to 
replace it with. Base cases work simply, an integer will not be replaced, whereas a symbol
equal to old will. Substituting into functions is done recursively through arguments,
but cases like let and lambda are more difficult, and involve checking to see whether the
identifier is the same as the old symbol. If the identifier is the same, a substitution will
not take place.

-eval: eval works differently depending on its input expression
	base cases: primitives return themselves (int-> int and bool -> bool, lambda->lambda)
	Proc: first check to see what the type of the input is by evaluating arguments
		then check to see if the function matches any of the primitive-type functions,
		for example, if Proc is given integer arguments, eval will check to see if the
		function is a Sum, Difference, etc.
	If: evaluates the if statement by evaluating arguments and passing them to an ocaml if 
		statement
	Let and LProc: evaluates the substitution of the id into the let/lambda expression
-print: prints primitive types (ints and bools)



