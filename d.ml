(* #use "utils.ml";; *)

 (* initial price *)
 let p0 = 2903.33
;;

(* Kandle parameters *)
let width = 2. and step = 1.02 (* 1 < step <= width *) and qB = 10_000. (* think in terms of *. p0 *) 
(* and Ca = 1. and Cb = 1. and transport_step = 1 *)
;;
let qA = qB /. p0
(* step = width corresponds to the base "band" b*a with 3 slots *)
;;

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

(* I the index set for the various state elements depends only on width and step *)
let mid, index_set = 
  let n = int_of_float (log(width) /. log(step)) in 
  (* NB: mid >= 1, index_set >= 3 *)
  n, 2*n + 1
;;

(* NB  pmin >= p0 /. width and pmax <= p0 *. width because of rounding *)

(* STATE of the strat *)
let current_price = ref p0 and price_grid = Array.make index_set 1. 
and bid = Array.make index_set 0. and ask = Array.make index_set 0.
and ib  = ref 0 and ia  = ref (index_set - 1)
;;

(* log linear price subdivision *)
let populate_price_grid () = 
  for i = 0 to (index_set - 1)
  do
  let expo = float_of_int (i - mid) in
  price_grid.(i) <-  p0 *. (step ** expo)
  done
;;

(* let price_init ~pmax:fpmax ~pmin:fpmin ~slots:n_slots =
  let ratio = (fpmax -. fpmin) /. float_of_int (n_slots - 1) in
  Array.init n_slots (fun index -> fpmin +. ratio *. float_of_int index) price
;; *)

let populate_book () = 
  (* INV: !ib + !ia + 1 = index_set = 2 * mid + 1 *)
  ib := mid - 1;
  ia := mid + 1;
  let qBu = (qB /. (float_of_int mid)) 
  and qAu = (qA /. (float_of_int mid)) in
  (* quantities are distributed uniformly *)
  for i = 0 to (mid - 1)
      do
      bid.(i) <- (qBu /. price_grid.(i)) ;
      ask.(i+mid+1) <- qAu
      done
;;

let portfolio_B () = 
  let res = ref 0. in 
  for i = 0 to (!ib) 
    do
    res := !res +. price_grid.(i) *. bid.(i)
    done  
    ;
  !res 
;;

let portfolio_A () = 
  let res = ref 0. in 
  for i = (!ia) to (index_set - 1)
    do
    res := !res +. ask.(i)
    done  
    ;
  !res 
;;

(* let ps () = 
  let pad = 6 in
print_string "current_price: "; print_float (!current_price); print_string "\n"; 
pra pad price_grid; print_string "\n"; 
pra pad bid; print_string "\n"; 
pra pad ask; print_string "\n";
(* pad_trunc_float *)
print_string "top_bid_index: "; print_int (!ib) ; print_string "\n"; 
print_string "top_ask_index: "; print_int (!ia) ; print_string "\n"
;; *)

let upmove q =
while ((!ia < index_set) && (q >= price_grid.(!ia))) (* order matters! *)
  do
  (* bb*aa =a=>  bbb*a *)
  let iia = !ia in 
  bid.(iia - 1) <- ask.(iia) *. price_grid.(iia) /. price_grid.(iia - 1);
  ask.(iia) <- 0.; 
  ib := iia - 1; (* self stabilising to INV, check *)
  incr ia; 
  done
;;

let downmove q =
  while ((!ib >=0) && (q <= price_grid.(!ib)))
    do
    (* bb*aa =b=>  b*aaa *)
    let iib = !ib in 
    ask.(iib + 1) <- bid.(iib);
    bid.(iib) <- 0.;
    ia := iib + 1;
    decr ib; 
    done
;;

let onestep q =
  let p = !current_price in
  if (q > p)
    then upmove q
    else if (q < p) 
           then downmove q
    ;
    current_price := q
;;

let price_series = h 6748 "Kandle_benchmark_data.csv" in
populate_price_grid ();
populate_book ();
Array.iter onestep price_series;
let qAf = portfolio_A () in
let qBf = portfolio_B () in
let mtmf = qAf *. !current_price +. qBf in
let mtmi = qA *. p0 +. qB in
print_string ("price_increment = ");
print_float (step -. 1.);
print_string (" -> return = ");
print_float(mtmf /. mtmi); 
print_string ("\n");
print_string "current_price = ";
print_float !current_price;
print_string ("\n")
;;

(* padded float *)
(* let padded_float x pad = 
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
*)