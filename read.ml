#load "str.cma";;
type quotedSyntax = Symbol of string | Number of int | List of quotedSyntax list

let take string index = String.sub string 0 index;;
let drop string index = String.sub string index ((String.length string) - index);;

let rec splitParens string : string list= 
	if string  = "" then []
	else if (String.length string) = 1 then [string]
	else if String.contains string '('
		then splitParens(take string (String.index string '('))@("("::splitParens(drop string (1 + (String.index string '('))))
	else if String.contains string ')'
		then splitParens(take string (String.index string ')'))@(")"::splitParens(drop string (1 + (String.index string ')'))))
	else [string];;

let toStringList string =  List.flatten (List.map splitParens (Str.split (Str.regexp_string " ") string));;

let string_to_int str =
	try
	let res = int_of_string str in 
	Some(res)
	with
	  | e -> None;;

let rec parse lols los : quotedSyntax= 
match (lols, los) with
|([[x]], []) -> x
|(_,"("::tail) -> parse ([]::lols) tail
|(hd::hd2::tl,")"::tail) -> parse ((List(List.rev hd)::hd2)::tl)  tail
|(hd::tl, head::tail) -> (let isNum = string_to_int head in
match isNum with
| None -> parse ((Symbol(head)::hd)::tl) tail
| Some(x) -> parse ((Number(x)::hd)::tl) tail)
|(_,_) -> failwith "Syntax Error";;

let read string =  parse [[]] (toStringList string);;
