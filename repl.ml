#use "read.ml";;
#use "lambda-interp.ml";;

let rec racketteRepl parse eval display =
  Printf.printf "Rackette > " ;
    (try
	match read_line () with
	|"exit" -> exit 1
	|line -> Printf.printf "%s\n" (display (eval (parse (read line)))) 
    with
      | e -> (match e with 
        | Failure(str) -> Printf.printf "Error: %s\n" str
        | _ -> Printf.printf "Error: %s\n" "Other exception failure" ));
      (racketteRepl parse eval display);;
      
racketteRepl parse eval print;;
