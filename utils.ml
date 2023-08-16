(* padded float *)
let padded_float x pad = 
  let s = string_of_float x in
  let l = String.length s in
  let p = if (pad > l) then pad - l else 0 in
  let sp = String.make p ' ' in
  if (x=0.) 
    then print_string (sp^"*.")
    else print_string (sp^s)
;;

let truncate_float x pad = 
  let shift = 10. ** (float_of_int pad) in
  let xx = x *. shift in
  let xxn = floor xx in
  xxn /. shift
;;

let pad_trunc_float x pad =
  let tx = truncate_float x 1 in
  padded_float tx pad
;;

(* padded rounded array *)
let pra pad = Array.iter 
  (fun x -> pad_trunc_float x pad;   print_string "| ")
;;



(* let h nb_lines = 
let ic = open_in "Kandle_benchmark_data.csv" in
let oc = open_out "Kandle_benchmark_data_price.csv" in
let _  = input_line ic in
output_string oc "# price\n";
for i = 1 to nb_lines
  do 
    let sline = input_line ic in
    let esline = String.split_on_char ',' sline in
    let current_price = List.nth esline 6 in
    output_string oc current_price;
    output_char oc '\n';
  done;
close_in ic;
close_out oc
;; *)

(* loading price_series as an array *)
let h nb_lines filename = 
  let ic = open_in filename in
  let _  = input_line ic in
  let price_series = Array.make nb_lines 1.0 in
  for i = 0 to (nb_lines - 1)
    do 
      let sline = input_line ic in
      let esline = String.split_on_char ',' sline in
      let current_price = List.nth esline 6 in
      price_series.(i) <- float_of_string current_price
    done;
  close_in ic;
  price_series
  ;;
