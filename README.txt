README
¥ your variant type for abstract syntax;
AbstractSyntax has several different types: 
* The direct ones of strings, integers and booleans & some operations
| Sym of string
 | Int of int 
| Bool of bool
| Sum 
| Difference
| Quotient 
| Product 
* comparator stuff
| And | Or | Equal
* Also some fancy syntax stuff
| If of abstractSyntax*abstractSyntax*abstractSyntax
| Let of abstractSyntax*abstractSyntax*abstractSyntax
| Lambda of abstractSyntax*abstractSyntax
| Proc of abstractSyntax*abstractSyntax*abstractSyntax
| LProc of abstractSyntax*abstractSyntax;;

instructions for use, describing how an end-user would interact with your program; 
An end-user comes in and types  **doit "EXPRESSION"** ,where EXPRESSION is a valid Rackette expression. For example, if he is going to type 
			(let ((x 10)) (if (= x 20) 45 x))    in Racket, 
in Rackette he should type 
			doit "(let ((x 10)) (if (= x 20) 45 x))" ;;
And the result will evaluate to 10.

for each procedure, a plain-language explanation of what the procedure does and how it 
does it (try to make the length of each explanation proportionate to the length of the 
code being explained (rule of thumb: 1 sentence for every 5 lines of code));
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

for the procedures that make use of other procedures you've written, 
an explanation of the relationships between them;
the only procedure that makes use of another one (besides itself) is eval, which needs subst to work. This is because evaluating lambdas and let expressions involve a substitution to work correctly. All other functions are contained within themselves.

a short paragraph overviewing how your program functions, how all of the pieces fit together;
First, a string is converted into quotedSyntax using the already written "read" procedure,
then, the quotedSyntax is converted into abstractSyntax using parse. This abstractSyntax can be understood by the rest of the program, it is the inner language of rackette. 
The abstractSyntax is then evaluated using eval. Depending on the expression, eval can call subst if it needs a substitution to work. Finally, the resulting evaluated expression is converted into a string to be printed by "print". So all in all, the program control flow
can be seen by how one would evaluate a string of rackette:
print (eval (parse (read "((lambda (x) +) 3 4)"))) -> "7"

a description of any possible bugs or problems with your program 
(rest assured, you will lose fewer points for documented bugs than undocumented ones); 
none that we know of. 

a list of the people with whom you collaborated; and ¥ a description of any extra features you chose to implement.
just Mengjie (Kelly) Wang and Charlie!