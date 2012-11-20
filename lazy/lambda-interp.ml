#use "../read.ml";;


type expr =
| Sym of string 
| Int of int 
| Binop of string (* string = / + * - *)
| Lambda of string*expr
| Proc of expr*expr*expr (*common operations - built in proc application *)
| LProc of expr*expr;; (* anon procedure application *)

type 
env = (string*value) list 
and
value = (* what exps evaluate to *)
|NumV of int
|ClosureV of string*expr*env
|BinopV of string
|SuspendV of expr*env;;

let yes = Lambda("true", Lambda("false", Sym("true")));;
let no  = Lambda("true", Lambda("false", Sym("false")));;

(*parse
	input: a quoted syntax expression from "read"
	output: the quoted syntax converted to rackettes abstract
	internal representation *)
let rec parse (input : quotedSyntax) : expr =
	match input with
		|Number(x) -> Int(x)
		|Symbol(s) ->(match s with
		              |"true" -> yes
		              |"false" -> no
		              |"=" -> Binop("=")
		              |"+" -> Binop("+")
		              |"-" -> Binop("-")
		              |"/" -> Binop("/")
		              |"*" -> Binop("*")
		              |any -> Sym(any))
		(* do some tricky desugaring to get rid of And and Or 
		|List([Symbol("and"); x ; y]) -> If(parse x , parse y, No)
		|List([Symbol("or") ; x ; y]) -> If(parse x , Yes, parse y) *)
		|List([Symbol("if");p;t_clause;e_clause]) -> 
			LProc(LProc(parse p, parse t_clause), parse e_clause)		
		(*fancy stuff*)
		|List([Symbol "lambda" ; List([Symbol(id)]); expr]) -> Lambda(id, parse expr)(* cascade_lambdas ids expr lambda expression*)
		|List([Symbol("let"); List[List[Symbol(id);ex]] ; expr]) -> LProc(Lambda(id, parse expr), parse ex)
		(* to deal with lambda application and Prim application*)
		|List(Number(x)::rest) -> failwith "expected function, got type Integer"
		|List[x;y]-> LProc(parse x, parse y)
		|List([p;x;y])->Proc(parse p,parse x, parse y)
		|_ -> failwith "error at parse level!";;

let rec lookup (id : string) (environment : env) : value = 
	match environment with
	|[] -> failwith ("unbound identifier: "^id)
	|(ident , v)::tl -> if id = ident then v else lookup id tl;;

let extend_environment (environ : env) (id : string) (v : value) =
	(id, v)::environ;;

(*print: abstractSyntax -> string
Input: an abstractSyntax 
output: add quotes to the input*)
let rec print  suspend(input:value) :string =
	match input with
	|NumV(x) -> string_of_int x
	|ClosureV(x,expr,e) -> "(lambda ("^x^")...)"
	|SuspendV(b, e) -> if suspend then"...suspended computation..."
						else print suspend (strict input) 
	|BinopV(s) -> s

and

(* given a value (possibly suspended computation),
 strict forces full evaluation to a "normal" value *)
strict (v : value) : value =
	match v with
	|NumV(x) -> v
	|ClosureV(id, ex, e) -> v
	|BinopV(s) -> v
	|SuspendV(body, e) -> strict (eval body e)
(*eval: abstractSyntax-> 'a
    Input: an abstractSyntax called input
    Output: evaluate the abstractSyntax according to designed rules, do some calculation and produce the result in a datum of various possible forms 
*)
and	eval (input: expr) (e : env) : value =
	match input with
		|Int(x) -> NumV(x)
		|Sym a -> lookup a e
		|Binop(s) -> BinopV(s)
		|Proc(p,x,y) -> 
			(match (strict (eval x e),strict (eval y e)) with
            |(NumV(i),NumV(j)) -> 
	            (match strict (eval p e) with
	            |BinopV("+") -> NumV(i+j)
	            |BinopV("-") -> NumV(i-j)
	            |BinopV("*") -> NumV(i*j)
	            |BinopV("/") -> NumV(i/j)
	            |BinopV("=") -> strict (eval (if i=j then yes else no) e)
	            |a-> failwith ("function "^(print true a)^"got int arguments, expected other types"))
            |_ -> failwith "the primitive procedure arguments are not recognized as primitives")
		|Lambda(id,expr) -> ClosureV(id,expr,e)
		|LProc(x,arg) -> 
			(match strict (eval x e) with
			|ClosureV(id,body,clos_env) -> eval body (extend_environment clos_env id (SuspendV(arg, e)))
			|_ -> failwith "only lambda procedures can take one arg!!!");;
	
	

(* interp: string -> string
input: a string that is a quoted expression in the Racket expression
output: the result that is expected to be given by Racket with quotes  
interp: basically a program writte in ocaml to imitate Racket language*)
let interp (input:string) : string =
	print true (eval (parse (read input)) []);;
let rec racketteRepl parse eval display =
  Printf.printf "Rackette > " ;
    (try
	match read_line () with
	|"exit" -> exit 1
	|line -> Printf.printf "%s\n" (display (eval (parse (read line)) [])) 
    with
      | e -> (match e with 
        | Failure(str) -> Printf.printf "Error: %s\n" str
        | _ -> Printf.printf "Error: %s\n" "Other exception failure" ));
      (racketteRepl parse eval display);;

let repl suspend = racketteRepl parse eval (print suspend) ;;

interp "((let ((x (lambda (x) (+ x x)))) x) 3)" = "6";;
interp "((if (= 2 2) (lambda (x) (+ 3 x)) *) 5)" = "8";;
interp "((if true + *) 3 4)" = "7";;
interp "(((lambda (x) +) 16) 5 6)" = "11";;
interp "(((lambda (x) (lambda (x) (+ x x))) 10) 3)"="6";;
interp "(let ((x 10)) (let ((x 1045)) (+ x x)))" = "2090";;
interp "(let ((x 1000)) (((lambda (y) (if (= x y) - +)) 4) 200 100))"="300";;


interp "((lambda (x) ((lambda (y) y) 10)) (+ 4 true))";;
