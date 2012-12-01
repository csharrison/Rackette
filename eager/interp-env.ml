#use "../read.ml";;
type abstractSyntax =
| Sym of string | Int of int | Bool of bool
| Binop of string
(*fancy syntax stuff*)
| If of abstractSyntax * abstractSyntax * abstractSyntax
| Lambda of string * abstractSyntax
| Rec of string * string * abstractSyntax * abstractSyntax
| Proc of abstractSyntax * abstractSyntax*abstractSyntax (*common operations - built in proc application *)
| LProc of abstractSyntax * abstractSyntax;; (* anon procedure application *)

type value = 
|IntV of int
|BoolV of bool
|BinopV of string
|ClosureV of string * abstractSyntax * environment
and
environment = (string * value) list;;


(*parse
	input: a quoted syntax expression from "read"
	output: the quoted syntax converted to rackettes abstract
	internal representation*)
let rec parse (input : quotedSyntax) : abstractSyntax =
	match input with
		|Number(x) -> Int(x)
		|Symbol(s) ->(match s with
		              |"true" -> Bool(true)
		              |"false" -> Bool(false)
		              |"="|"+"|"-"|"/"|"*" -> Binop(s)
		              |any -> Sym(any))
		(* do some tricky desugaring to get rid of And and Or *)
		|List([Symbol("and"); x ; y]) -> If(parse x , parse y, Bool(false))
		|List([Symbol("or") ; x ; y]) -> If(parse x , Bool(true), parse y)
		
		|List([Symbol("if");p;t_clause;e_clause]) -> If(parse p, parse t_clause, parse e_clause)		

		|List([Symbol "lambda" ; List([Symbol(id)]); expr]) -> Lambda(id, parse expr)
		|List([Symbol "rec" ; List([Symbol(func) ; Symbol(id)]); expr ; body]) -> Rec(func, id, parse expr, parse body)
		|List([Symbol("let"); List[List[Symbol(id);ex]] ; expr]) -> LProc(Lambda(id, parse expr), parse ex)

		|List([x;y])-> LProc(parse x, parse y)
		|List([p;x;y])->Proc(parse p,parse x, parse y)
		|_ -> failwith "error at parse level!";;

let rec lookup (str : string) (env : environment) : value = 
	match env with
	|[] -> failwith ("unbound identifier: "^str)
	|(id, value)::rest -> if id = str then value else lookup str rest;;

(*eval: abstractSyntax-> 'a
        Input: an abstractSyntax called input
        Output: evaluate the abstractSyntax according to designed rules, do some calculation and produce the result in a datum of various possible forms *)
let rec	eval (input: abstractSyntax) (env : environment) : value =
	match input with
		|Sym(x) -> lookup x env
		|Int(x) -> IntV(x)
		|Bool (x)  -> BoolV(x)		
		|Binop(x) -> BinopV(x)
		|Proc(p,x,y) -> 
			(match (eval p env, eval x env, eval y env) with
				|(BinopV(op), IntV(i), IntV(j)) -> 
					(match op with
			            |"+" -> IntV(i+j)
			            |"-" -> IntV(i-j)
			            |"*" -> IntV(i*j)
			            |"/" -> IntV(i/j)
			            |"=" -> BoolV(i=j)
						|_-> failwith "function got int arguments, expected other types")
				|(BinopV(op), BoolV(x), BoolV(y)) ->
					(match op with
						|"=" -> BoolV(x = y)
						|_ -> failwith "function got bool args, expected other types")
				|_ -> failwith "the primitive procedure arguments are not recognized as primitives")
		|If(pred,tr,el) -> eval (if eval pred env = BoolV(true) then tr else el) env
		|Lambda(id,expr) -> ClosureV(id,expr,env)
		|Rec(func, id, expr, body) -> 
			(*the weirdness, we recursively defined an environment in terms of itself
				this might be a too hacky (in terms of the students actually figuring out how this is working)
				but I think it's cool
			*)
			let rec extended = (func, ClosureV(id,expr, extended))::env in eval body extended

		|LProc(proc,arg) -> 
			(match (eval proc env) with
			|ClosureV(id,expr,clos_env) -> let a = (eval arg env) in eval expr ((id, a)::clos_env)
			|_ -> failwith "expected a lambda procedure!!!");;
(*print: abstractSyntax -> string
Input: an abstractSyntax 
output: add quotes to the input*)
let rec print (input:value):string =
	match input with
	|IntV(x) -> string_of_int x
	|BoolV(x) -> string_of_bool x
	|ClosureV(x,expr,env) -> "(lambda ("^x^")...)"
	|BinopV(s) -> s;;	
	


(* doit: string -> string
input: a string that is a quoted expression in the Racket expression
output: the result that is expected to be given by Racket with quotes  
doit: basically a program writte in ocaml to imitate Racket language*)
let doit (input:string) : string =
	print (eval (parse (read input)) []);;
(*test cases*)
doit "(let ((x 10)) (if (= x 20) 45 x))" = "10";;
doit "(let ((x 10)) (let ((y x))  (+ x (+ y x))))" =  "30";;
doit "(let ((x (lambda (y) (+ y 100)))) (x 23))" = "123";;
doit "(let ((x (lambda (y) (lambda (z) (= y z))))) ((x 5) 5))" = "true";;
doit "(((lambda (x) +) 0) 3 4)" = "7";;
doit "(((lambda (x) +) 0) ((lambda (x) 4) 0) ((lambda (x) (+ x 100)) 100))" = "204";;
doit "((let ((x (lambda (x) (+ x x)))) x) 3)" = "6";;
doit "((if (= 2 2) (lambda (x) (+ 3 x)) *) 5)" = "8";;
doit "((if true + *) 3 4)" = "7";;
doit "(((lambda (x) +) 16) 5 6)" = "11";;
try ignore(doit "(+ * *)");false with Failure(x) -> x = "the primitive procedure arguments are not recognized as primitives";;
try ignore(doit "(+ 1)");false with Failure(x) -> x = "only lambda procedures can take one arg!!!";;
doit "(((lambda (x) (lambda (x) (+ x x))) 10) 3)"="6";;
doit "(let ((x 10)) (let ((x 1045)) (+ x x)))" = "2090";;
doit "(let ((x 1000)) (((lambda (y) (if (= x y) - +)) 4) 200 100))"="300";;



doit "(rec (f n) (if (= n 0) 1 (* n (f (- n 1)))) (f 5))" = "120";;
