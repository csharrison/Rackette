#load "str.cma";;
#use "../read.ml";;
#use "lazy.ml";;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let read_file s =
	let closed = Str.global_replace (Str.regexp "[\n\t]") "" s in
	"("^closed^")";;

let parse_file (f : string list) =
	List.map (fun x-> (parse (read x))) f;;


let rec eval_file (f : expr list) = 
	match f with
	|[] -> ()
	|hd::tl -> 
		let e = eval ~global:true hd [] in
		begin
			print_string (print e);
			print_string "\n";
			eval_file tl
		end;;

let run file_name =
	match read (read_file (load_file file_name)) with
	|List(l) ->  (List.map (fun x-> (eval ~global:true (parse x) [] )) l)
	|_-> failwith "syntax error";;

let a =
	for i = 1 to Array.length Sys.argv - 1 do
		run Sys.argv.(i)
	done;;

repl ();;
