#use "read_file.ml";;


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


let rec traverse path = 
	let rec find_files (files : string array) i : string list= 
		if i >= (Array.length files) then [] else
		let file = Array.get files i in
				if List.hd (List.rev (Str.split (Str.regexp_string ".") file)) = "lazy" 
					then (path^"/"^file)::(find_files files (i+1))
				else if Sys.is_directory (path^"/"^file)
					then (traverse (path^"/"^file))@(find_files files (i+1))
				else find_files files (i+1)
	in find_files (Sys.readdir path) 0;;

let built_in = List.map run (traverse "library");;

let a = ref [];;
let a =
	for i = 1 to Array.length Sys.argv - 1 do
		a:=run Sys.argv.(i)
	done;;

if Array.length Sys.argv > 1 then repl () else ();;

repl ();;