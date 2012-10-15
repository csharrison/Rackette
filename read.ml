#load "str.cma";;
type quotedSyntax = Symbol of string | Number of int | List of quotedSyntax list

let read string =
let take string index = String.sub string 0 index
in let drop string index = String.sub string index ((String.length string) - index)
in let rec splitParens string = 
if string  = ""
then []
else if (String.length string) = 1
then [string]
else if String.contains string '('
then splitParens(take string (String.index string '('))@("("::splitParens(drop string (1 + (String.index string '('))))
else if String.contains string ')'
then splitParens(take string (String.index string ')'))@(")"::splitParens(drop string (1 + (String.index string ')'))))
else [string]
in let toStringList string =  List.flatten (List.map splitParens (Str.split (Str.regexp_string " ") string))
  in let string_to_int str =
try
let res = int_of_string str in 
Some(res)
with
  | e -> None
in let rec parse lols los = 
match (lols, los) with
|([[x]], []) -> x
|(_,"("::tail) -> parse ([]::lols) tail
|(hd::hd2::tl,")"::tail) -> parse ((List(List.rev hd)::hd2)::tl)  tail
|(hd::tl, head::tail) -> (let isNum = string_to_int head in
match isNum with
| None -> parse ((Symbol(head)::hd)::tl) tail
| Some(x) -> parse ((Number(x)::hd)::tl) tail)
|(_,_) -> failwith "Syntax Error"
in parse [[]] (toStringList string)


let rec racketteRepl parse eval display =
  Printf.printf "Rackette > " ;
    (try
      Printf.printf "%s\n" (display (eval (parse (read (read_line ()))))) 
    with
      | e -> (match e with 
        | Failure(str) -> Printf.printf "Error: %s\n" str
        | _ -> Printf.printf "Error: %s\n" "Other exception failure" ));
      (racketteRepl parse eval display)
