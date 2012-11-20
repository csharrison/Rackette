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


run "library/built-ins.lazy";;
let a = ref [];;
let a =
	for i = 1 to Array.length Sys.argv - 1 do
		a:=run Sys.argv.(i)
	done;;

if Array.length Sys.argv > 1 then repl () else ();;

repl ();;