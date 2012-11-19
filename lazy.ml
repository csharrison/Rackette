#use "read.ml";;


type expr =
| Sym of string 
| Int of int 
| Bool of bool
(*boolean + comparator stuff*)
| Cons of expr * expr
| Empty
(*fancy syntax stuff*)
| If of expr * expr * expr
| Lambda of (string list) * expr
| Define of string * expr
| Prim2 of string * expr * expr (*common operations - built in proc application *)
| Prim1 of string * expr
| App of expr * (expr list);; (* one arg procedure application *)

type 
env = (string*value) list 
and
value = (* what exps evaluate to *)
|NumV of int
|BoolV of bool
|ClosureV of (string list) * expr * env
|ConsV of value * value
|EmptyV
|ComputedV of value ref
|SuspendV of expr * env;;

let extract_ids sym_lst : (string list) = 
	let extract s = match s with
	|Symbol(str) -> str
	|_ -> failwith "lambda expects a list of ids" in
	List.map extract sym_lst;;

(*parse
	input: a quoted syntax expression from "read"
	output: the quoted syntax converted to rackettes abstract
	internal representation *)
let rec parse (input : quotedSyntax) : expr =
	match input with
		|Number(x) -> Int(x)
		|Symbol(s) ->(match s with
		              |"true" -> Bool(true)
		              |"false" -> Bool(false)
		              |"empty" -> Empty
		              |any -> Sym(any))
		(* do some tricky desugaring to get rid of And and Or *)
		|List([Symbol("define") ; List(Symbol(func)::ids) ; body])->
			Define(func, Lambda(extract_ids ids, parse body))
		|List([Symbol("define") ; Symbol(name) ; expr]) ->
			Define(name, parse expr)
		|List(Symbol("list")::tl) -> List.fold_right (fun x r -> Cons(parse x,r)) tl Empty
		|List([Symbol("cons"); x; y]) -> Cons(parse x, parse y)
		|List([Symbol("and"); x ; y]) -> If(parse x , parse y, Bool(false))
		|List([Symbol("or") ; x ; y]) -> If(parse x , Bool(true), parse y)
		|List([Symbol("if");p;t_clause;e_clause]) -> If(parse p, parse t_clause, parse e_clause)		
		(*fancy stuff*)
		|List([Symbol "lambda" ; List(sym_lst); expr]) -> 
			Lambda(extract_ids sym_lst, parse expr)
		|List([Symbol("let"); List[List[Symbol(id);ex]] ; expr]) -> 
			App(Lambda([id], parse expr),[parse ex])
		(* to deal with lambda application and Prim application*)
		|List(Number(x)::rest) -> failwith "expected function, got type Integer"
		|List(func::args) -> App(parse func, List.map parse args)
		|_ -> failwith "error at parse level!";;


let make_prim2 str = ClosureV(["x";"y"], Prim2(str, Sym("x"), Sym("y")), []);;
let make_prim1 str = ClosureV(["x"], Prim1(str, Sym("x")), []);;

let global_env = Hashtbl.create 30;;
(List.map (fun x-> Hashtbl.replace global_env x (make_prim2  x)) ["+";"-";"*";"/";"="; "<"; ">"]);;
(List.map (fun x-> Hashtbl.replace global_env x (make_prim1  x)) ["first";"rest"; "print";"empty?"; "cons?"; "not" ; "bool"]);;

let rec lookup (id : string) (environment : env) : value option = 
	match environment with
	|[] -> None
	|(ident , v)::tl -> if id = ident then Some(v) else lookup id tl;;

let extend_environment (environ : env) (id : string) (v : value) =
	(id, v)::environ;;


(* given a value (possibly suspended computation),
 strict forces full evaluation to a "normal" value *)
let rec strict ?(really=false) (v : value) : value =
	match v with
	|NumV(x) -> v
	|BoolV(x) -> v
	|ClosureV(ids, ex, e) -> v
	|ConsV(x,y) -> ConsV(strict ~really:really x, if really then strict ~really:true y else y)
	|EmptyV -> v
	|ComputedV(value_ref) -> 
		(match !value_ref with
		|SuspendV(b,e) -> 
			begin
				value_ref := strict ~really:really (eval b e); !value_ref
			end
		|other -> strict ~really:really other)
	|SuspendV(body, e) -> failwith "SuspendV should be wraped in a ComputedV"

and bool_eval (inp : value) : value =
	match (strict inp) with
		|NumV(x) -> BoolV(x <> 0)
		|BoolV(x) -> BoolV(x)
		|ClosureV(ids, ex, e) -> BoolV(true)
		|ConsV(x,y) -> BoolV(true)
		|EmptyV -> BoolV(false)
		|ComputedV(vref) -> bool_eval (!vref)
		|SuspendV(body, e) -> failwith "shouldn't get here (strict)"

(*eval: abstractSyntax-> 'a
    Input: an abstractSyntax called input
    Output: evaluate the abstractSyntax according to designed rules, do some calculation and produce the result in a datum of various possible forms 
*)
and	eval ?(global=false) (input: expr) (e : env) : value =
	match input with
		|Int(x) -> NumV(x)
		|Bool (x) -> BoolV(x)
		|Sym a -> (match (lookup a e) with
					|None -> (match (Hashtbl.find_all global_env a) with
								|hd::tl -> hd
								|[] -> failwith ("unbound identifier "^a))
					|Some(v) -> v)
		|Cons(x,y) -> ConsV(ComputedV(ref (SuspendV (x, e))), ComputedV(ref (SuspendV(y, e))))
		|Empty -> EmptyV
		|Prim1(p, x) ->
			let v = strict (eval x e) in
			(match p with
				|"print" -> strict ~really:true v
				|"not" -> (match (bool_eval v) with
							|BoolV(x) -> BoolV(not x)
							|_ -> failwith "bool_eval should return a boolean")
				|"bool" -> bool_eval v
				|"empty?" | "cons?" | "first" | "rest" -> 
					(match v with
					|EmptyV -> 
						(match p with
							|"empty?" -> BoolV(true)
							|"cons?" -> BoolV(false)
							|_ -> failwith ("bad operation, "^p^" on empty list"))
					|ConsV(first,rest) ->
						(match p with
							|"empty?" -> BoolV(false)
							|"cons?" -> BoolV(true)
							|"first" -> first
							|"rest" -> rest
							|_ -> failwith ("bad operation, "^p^" on a cons"))
					|_ -> failwith ("list operation "^p^" applied to nonlist: "^(print v)))
				|_ -> failwith ("unknown operation: "^p))
		|Prim2(p,x,y) -> 
			(match (strict (eval x e),strict (eval y e)) with
            |(NumV(i),NumV(j)) -> 
	            (match p with
	            |"+" -> NumV(i+j)
	            |"-" -> NumV(i-j)
	            |"*" -> NumV(i*j)
	            |"/" -> NumV(i/j)
	            |"<" -> BoolV(i<j)
	            |">" -> BoolV(i>j)
	            |"=" -> BoolV(i=j)
	            |_-> failwith "function got int arguments, expected other types")
            |(BoolV(i),BoolV(j)) ->
	            (match p with
	            |"="-> BoolV(i = j)
	            |_ -> failwith "function got bool args, expected other types")
            |_ -> failwith "the primitive procedure arguments are not recognized as primitives")
		|If(predicate,true_clause,else_clause) -> 
			if (eval predicate e)=BoolV(true) then eval true_clause e else eval else_clause e
		|Lambda(id_lst,expr) -> ClosureV(id_lst,expr,e)
		|Define(name, expr) -> 
			if global then
				let value = (eval expr e) in
				(begin Hashtbl.replace global_env name value; value end)
			else failwith "define construct only allowed in toplevel"
		|App(func, args) -> 
			(match strict (eval func e) with
			|ClosureV(ids,body,clos_env) -> 
				let new_environment = (List.map2 (fun x y -> (x, ComputedV(ref (SuspendV(y,e))))) ids args)@clos_env in 
				eval body new_environment
			|_ -> failwith "found a nonfunction in application position")

(*print: abstractSyntax -> string
Input: an abstractSyntax 
output: add quotes to the input*)
and print (input:value) :string =
	match input with
	|NumV(x) -> string_of_int x
	|BoolV(x) -> string_of_bool x
	|ClosureV(ids,expr,e) -> "(lambda ( "^(List.fold_right (fun x r -> x^" "^r) ids "")^")...)"
	|SuspendV(b, e) -> "<~suspended~>"
	|ConsV(x,y) -> 
		let rec p v = 
		(match v with
		|ConsV(f,r) -> " "^(print f)^(p r)
		|EmptyV -> ")"
		|ComputedV(v) -> (p (!v))
		|SuspendV(sus,e) -> " "^(print v)^")"
		|other -> ". "^(print other)^")") in "(list"^(p input)
	|ComputedV(v) -> print (!v)
	|EmptyV -> "empty";;	


(* interp: string -> string
input: a string that is a quoted expression in the Racket expression
output: the result that is expected to be given by Racket with quotes  
interp: basically a program writte in ocaml to imitate Racket language*)
let interp (input:string) : value =
	(eval ~global:true (parse (read input)) []);;
let rec racketteRepl parse eval display =
  Printf.printf "Lazy Rackette > " ;
    (try
	match read_line () with
	|"exit" -> exit 1
	|line -> Printf.printf "%s\n" (display (eval (parse (read line)) [])) 
    with
      | e -> (match e with 
        | Failure(str) -> Printf.printf "Error: %s\n" str
        | _ -> Printf.printf "Error: %s\n" "Other exception failure" ));
      (racketteRepl parse eval display);;

let repl = (fun ()-> racketteRepl parse (fun x -> eval ~global:true x) print);;


let map  = interp "(define (map fun lst) (if (empty? lst) empty (cons (fun (first lst)) (map fun (rest lst)))))" ;;
let map2 = interp "(define (map2 fun lst1 lst2) (if (empty? lst1) empty (cons (fun (first lst1) (first lst2)) (map2 fun (rest lst1) (rest lst2)))))";;
let take = interp "(define (take n lst) (if (= 0 n) empty (cons (first lst) (take (- n 1) (rest lst)))))";;
let drop = interp "(define (drop n lst) (if (= 0 n) lst (drop (- n 1) (rest lst))))";;
let fibs = interp "(define fibs (cons 0 (cons 1 (map2 + fibs (rest fibs)))))";;
let nth  = interp "(define (nth lst n) (if (= n 0) (first lst) (nth (rest lst) (- n 1))))";;
let range = interp "(define (range s n) (if (= s n) empty (cons s (range (+ s 1) n))))";;
let add1 = interp "(define (add1 n) (+ n 1))";;
let sub1 = interp "(define (sub1 n) (- n 1))";;