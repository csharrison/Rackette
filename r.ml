#use "read.ml";;

type abstractSyntax =
| Sym of string | Int of int | Bool of bool
| Sum | Difference | Quotient | Product 
(*boolean + comparator stuff*)
| And | Or | Equal
(*fancy syntax stuff*)
| If of abstractSyntax*abstractSyntax*abstractSyntax
| Let of abstractSyntax*abstractSyntax*abstractSyntax
| Lambda of abstractSyntax*abstractSyntax
| Proc of abstractSyntax*abstractSyntax*abstractSyntax (*common operatiosn*)
| LProc of abstractSyntax*abstractSyntax;; (*procedure that especially designed for procedure within lambda*)

(*parse
	input: a quoted syntax expression from "read"
	output: the quoted syntax converted to rackettes abstract
	internal representation*)
let rec parse (input : quotedSyntax) : abstractSyntax =
	match input with
		|Symbol("true")->Bool(true)
		|Symbol("false") -> Bool(false)
		|Symbol("+") -> Sum
		|Symbol("-") -> Difference
		|Symbol("/") -> Quotient
		|Symbol("*") -> Product
		|Symbol("and") -> And
		|Symbol("or") -> Or
		|Symbol("=") -> Equal
		|Number(x) -> Int(x)
		|List(Number(x)::rest) -> failwith "expected function, got type Integer" (*racket do not take in just a pure list*)
		|List([Symbol("if");p;t_clause;e_clause]) -> 
			If(parse p, parse t_clause, parse e_clause)		
		(*fancy stuff*)
		|List([Symbol("let"); List[List[id;ex]] ; expr]) ->   
		  Let(parse id, parse ex, parse expr)              (*let expression*)
		|List([Symbol "lambda" ; List([id]); expr]) -> Lambda(parse id, parse expr)  (*lambda expression*)
		|List[x;y]-> LProc(parse x, parse y) (*this line is written to deal with lambda*)
		|List([p;x;y])->Proc(parse p,parse x, parse y)
		|Symbol(s) -> Sym(s)
		|_ -> failwith "error at parse level!";;

(*test cases*)
parse (read "(lambda (x) (+ x 10))") = Lambda(Sym "x", Proc(Sum,Sym "x", Int 10));;
parse (read "(let ((x 10)) (+ x 10))") = Let(Sym "x",Int 10, Proc(Sum,Sym "x", Int 10));;
parse (read "+") = Sum;; parse (read "-") = Difference;; parse (read "/") = Quotient;;
parse (read "*") = Product;; parse (read "and") = And;;parse (read "or") = Or ;;
parse (read "=") = Equal ;;
parse (read "3") = Int 3;;
try ignore(parse (read "(3 3 3)"));false with Failure(x) -> x = "expected function, got type Integer";;
try ignore(parse (read "()"));false with Failure(x) -> x = "error at parse level!";;
parse (read "(lambda (x) (+ x 10))") = Lambda(Sym "x",Proc(Sum,Sym "x",Int 10));;
parse (read "(((lambda (x) +) 0) 3 4)") = Proc(LProc(Lambda(Sym "x", Sum),Int 0), Int 3, Int 4);;
parse (read "(if (= 10 20) 100 200)") = If(Proc(Equal,Int 10, Int 20),Int 100, Int 200);;


(*subst: abstractSyntax abstractSyntax abstractSyntax -> abstractSyntax
input: three expressions of type abstractSyntax, the expression to substitute into
	the value to substitute, and the new value that the old value changes into
output: expr with every instance of old replaced with new
special cases: let expressions and lambdas give priority to innermost definitions
*)
let rec subst (expr:abstractSyntax) (old:abstractSyntax) (n:abstractSyntax) : abstractSyntax =
	match expr with
		|Int(x) -> Int(x)
		|Sym(x) -> if Sym(x)=old then n else Sym(x)
		|Proc(any,x,y) -> Proc(subst any old n, subst x old n, subst y old n)
		|LProc(x,y) -> LProc(subst x old n, subst y old n)
		|If(p,x,y) -> If(subst p old n, subst x old n, subst y old n)
		|Lambda(id, lexpr) -> if id=old then expr else Lambda(id, subst lexpr old n)
		|Let(id,ex,y) -> if id=old then     (* let expressions and lambdas give priority to innermost definitions*)
			Let(id, subst ex old n, y) else
			Let(id, subst ex old n, subst y old n)
		|func -> func;;

(*eval: abstractSyntax-> 'a
        Input: an abstractSyntax called input
        Output: evaluate the abstractSyntax according to designed rules, do some calculation and produce the result in a datum of various possible forms *)
let rec	eval (input: abstractSyntax) : 'a =
	match input with
		|Int(x) -> Int(x)
		|Bool (x)  -> Bool (x)		
		|Proc(p,x,y) -> let xeval = (eval x) and yeval = (eval y) in
			(match (xeval,yeval) with
			|(Int(i),Int(j)) -> 
				(match eval p with
				|Sum -> Int(i+j)
				|Difference -> Int(i-j)
				|Product -> Int(i*j)
				|Quotient -> Int(i/j)
				|Equal -> Bool(i=j)
				|_-> failwith "function got int arguments, expected other types")
			|(Bool(i),Bool(j)) ->
				(match p with
				|And -> Bool(i && j)
				|Or -> Bool(i || j)
				|Equal -> Bool (i = j)
				|_ -> failwith "function got bool args, expected other types")
			|_ -> failwith "the primitive procedure arguments are not recognized as primitives")
		|If(predicate,true_clause,else_clause) -> 
			if (eval predicate)=Bool(true) then eval true_clause else eval else_clause
		|Let(id, ex, expr) -> eval (subst expr id ex)
		|Lambda(id,expr) -> Lambda(id,expr)
		|LProc(Lambda(id,expr), arg) -> eval (subst expr id (eval arg))
		|LProc(x,y) -> let hopefully_lambda = (eval x) and arg = (eval y) in   (*evaluate lambda procedure*)
			(match hopefully_lambda with
			|Lambda(id,lexpr) -> eval (subst lexpr id arg)
			|_ -> failwith "only lambda procedures can take one arg!!!")
		|Sym a -> failwith("error: "^a^" is not defined")
		|func -> func;;
(*test cases*)
eval (parse (read "(if (= 4 4) 100 200)")) = Int 100;;
eval (parse (read "(if (= 2 4) 100 200)")) = Int 200;;
eval (parse (read "(if (and (= 100 2) (= 3 3)) (+ 100 200) (- 300 3))")) = Int 297;;

(*print: abstractSyntax -> string
Input: an abstractSyntax 
output: add quotes to the input*)
let rec print (input:abstractSyntax):string =
	match input with
	|Int(x) -> string_of_int x
	|Bool(x) -> string_of_bool x
	|Lambda(Sym(x),expr) -> "(lambda ("^x^")...)"
	|_-> failwith "input is unprintable";;
(*test cases*)
print (Bool true) = "true";;
print (Int 3) = "3";;
print (Lambda (Sym "x", Proc (Sum, (Int 3), Sym "x")))= "(lambda (x)...)";;


(* doit: string -> string
input: a string that is a quoted expression in the Racket expression
output: the result that is expected to be given by Racket with quotes  
doit: basically a program writte in ocaml to imitate Racket language*)
let doit (input:string) : string =
	print (eval (parse (read input)));;
(*test cases*)
doit "(let ((x 10)) (if (= x 20) 45 x))" = "10";;
doit "(let ((x 10)) (let ((y x))  (+ x (+ y x))))" =  "30";;

eval (parse (read "((lambda (x) (+ x 10)) 100)")) = Int 110;;
eval (parse (read "(((lambda (x) (lambda (y) (+ x y))) 5) 4)")) =  Int 9;;
doit "(let ((x (lambda (y) (+ y 100)))) (x 23))" = "123";;
doit "(let ((x (lambda (y) (lambda (z) (= y z))))) ((x 5) 5))" = "true";;
doit "(((lambda (x) +) 0) 3 4)" = "7";;
doit "(((lambda (x) +) 0) ((lambda (x) 4) 0) ((lambda (x) (+ x 100)) 100))" = "204";;
eval (parse (read  "(* 2 (let ((x (lambda (x) (+ x 2)))) (x 3)))")) = Int 10;;

doit "((let ((x (lambda (x) (+ x x)))) x) 3)" = "6";;
doit "((if (= 2 2) (lambda (x) (+ 3 x)) *) 5)" = "8";;
doit "((if true + *) 3 4)" = "7";;
doit "(((lambda (x) +) 16) 5 6)" = "11";;
try ignore(doit "(+ * *)");false with Failure(x) -> x = "the primitive procedure arguments are not recognized as primitives";;
try ignore(doit "(+ 1)");false with Failure(x) -> x = "only lambda procedures can take one arg!!!";;
doit "(((lambda (x) (lambda (x) (+ x x))) 10) 3)"="6";;
doit "(let ((x 10)) (let ((x 1045)) (+ x x)))" = "2090";;
doit "(let ((x 1000)) (((lambda (y) (if (= x y) - +)) 4) 200 100))"="300";;
doit "(((lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))) (lambda (f) lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))) 5)";;
