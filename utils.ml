(* --------------------- UTILS --------------------- *)
(* adds spaces before float and truncate it *)
let pad_float f length trunc = 
  let fsign, af = if (f < 0.) then (1, -. f) else (0, f) in 
  let s = string_of_float af in
  let [x; y] = String.split_on_char '.' s in
  let xl = String.length x in
  let yl = String.length y in
  let y' = String.sub y 0 (min trunc yl) in
  let nb_additional_zeros = max (trunc - yl) 0 in
  let total_non_white_string_length =  
    fsign + xl + 1 + trunc in
  let nb_additional_white_spaces = 
    max (length - total_non_white_string_length) 0 in
  let white_spaces = String.make nb_additional_white_spaces ' ' in
  let zeros = String.make nb_additional_zeros '0' in
  let sign = if (fsign = 1) then "-" else "" in
  print_string (white_spaces^sign^x^"."^y'^zeros)
;;

(* padded rounded array *)
let pra length trunc = 
  print_string "| ";
  Array.iter 
  (fun x -> pad_float x length trunc; print_string "| ")
;;

(* generating a csv output from a (float * float) array *)
let array2_to_csv 
~filename:string ~array2:val_array = 
  let file_string_out = "csv/"^string^".csv" in
  let oc = open_out file_string_out in
  output_string oc  "# time, value\n";
  Array.iter 
  (fun (timestamp, value) -> 
    output_string oc (string_of_float timestamp);
    output_string oc ", ";
    output_string oc (string_of_float value);
    output_char oc '\n'
    )
    val_array;
  close_out oc
;;
