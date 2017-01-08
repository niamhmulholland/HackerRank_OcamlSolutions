(*    HackerRank: CTCI     *
 *                         *
 *     OCaml Solutions     * 
 *						   *
 *	Arrays: Left Rotation  *  
 *						   *
 *	  Niamh Mulholland     * 
 *       08.01.17          *)


(* Read input from STDIN. Print output to STDOUT *)

let args = input_line stdin;;
let ints = input_line stdin;;
let args_list = Str.split (Str.regexp "[ \t]+") args;;
let ints_list = Str.split (Str.regexp "[ \t]+") ints;;   

let length = int_of_string (List.nth args_list 0);;
let r_num = int_of_string (List.nth args_list 1);;
                
let val_arr = Array.of_list (List.map (fun x -> int_of_string x) ints_list);;

let new_arr = Array.make length 0;;

for i = 0 to (length - 1) do 
    new_arr.(i) <- val_arr.((r_num + i)mod length)
done;;

Array.iter (fun x -> Printf.printf "%d " x) new_arr;;

(* Alternative (slower) solution *)

let rec rotate ints_list r_num acc = 
    match ints_list with
    | h::t -> if r_num > 0 then rotate (t @ [h]) (r_num - 1) (t @ [h])
                else acc;;

List.map (fun x -> Printf.printf "%s " x) (rotate ints_list r_num []) ;;



