(* Paris Mar 2025 *)

open Utils
open Brownians

(* --------------------- PRICE GRID --------------------- *)
(* 
   NB: number of points for various values of gridstep
   given by log(rangeMultiplier) /. log(gridstep);; 
*)

let generate_price_grid 
~half_number_of_price_points:mid 
~gridstep:ratio 
~initial_price:p0 = 
  Array.init 
    (2 * mid + 1) 
    (fun i -> let expo = float_of_int (i - mid) in
              p0 *. ratio ** (expo)) 
;;


(* --------------------- POPULATE BOOK --------------------- *)
(* capital in A and B is distributed uniformly on both sides *)
(* ask volumes are constant = qA/nb_asks *)
(* bid volumes are decreasing = qB/nb_asks * 1/pg(i) *)
(* we don't fill the central slot = hole *)  
let populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB 
~price_grid:pg = 
    let index_set  = 2 * mid + 1 in
    let bid = Array.make index_set 0. 
    and ask = Array.make index_set 0. 
    and ib  = ref (mid - 1)
    and ia  = ref (mid + 1) in
    let qBu = (qB /. (float_of_int mid)) 
    and qAu = (qA /. (float_of_int mid)) in
    for i = 0 to (mid - 1)
        do
        bid.(i) <- (qBu /. pg.(i)) ;
        ask.(i+mid+1) <- qAu
        done;
    ia, ib, ask, bid
;;
  
(* no partial fill invariant: !iia - !iib = 2 *)

(* computing the amount of B, quote, in the book *)
let portfolio_B iib bid price_grid = 
  let simres = ref 0. in 
  for i = 0 to (iib) 
    do
    simres := !simres +. price_grid.(i) *. bid.(i)
    done  
    ;
  !simres 
;;

(* computing the amount of A, base, in the book *)
let portfolio_A mid iia ask = 
  let simres = ref 0. in 
  for i = (iia) to (2 * mid)
    do
    simres := !simres +. ask.(i)
    done  
    ;
  !simres 
;;


(* --------------------- KANDLE SIMULATION --------------------- *)
(* 
inputs: 
1) strat parameters = price grid, and capital in cash (quote, written qB) and cashmix
2) start = when to enter game and duration = how long to play
3) price series 

outputs: 
return (the stochastic integral of strat against price)
number of up- and down-crossings
NB: duration could be a stopping time in general, eg looking at a price-crossing event 
NB: no need to slice the price series as the price series array is read only
*)

let sim 
(* the parameters chosen = investment decision *)
~rangeMultiplier:rangeMultiplier                 (* pmax/p0 = p0/pmin *)
~gridstep:gridstep                               (* ratio of price grid *)
~quote:qB                                        (* total budget in quote *)
~cashmix:alpha                                   (* 0 ≤ alpha = qB/(qA+qB) ≤ 1 *)
~start:start                                     (* time of entry in the position *)
~duration:duration                               (* duration =  number of price moves *)
(* the future *)
~price_series:price_series                       (* series of price *)
=
(* check we have enough points in the price series *)
if not (start + duration <= (Array.length price_series))
  then failwith "sim: number of steps required > length of price series";

(* state: static part *)
let mid = int_of_float (log(rangeMultiplier) /. log(gridstep)) in
let index_set = 2 * mid + 1 in
(* the initial price *)
let p0 = price_series.(start) in
let current_price = ref p0 in

let price_grid = generate_price_grid 
~half_number_of_price_points:mid 
~gridstep:gridstep 
~initial_price:p0 in

(* print_string "> price grid:\n "; 
pra 12 price_grid;  print_string "\n"; *)

(* here we divide the initial quote capital *)
(* alpha = 0.5 means starting a 50/50 position *)
let qB' = alpha *. qB in
let qA =  (1. -. alpha) *. qB /. p0 in
(* state: dynamic part *)
let ia, ib, ask, bid = 
populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB' 
~price_grid:price_grid
in


(*  tracking up- and down-crossings *)
let rebu = ref 0 in
let rebd = ref 0 in
let cross_above = ref 0 in 
let cross_below = ref 0 in 

(* what we do when next price is higher than current *)
let upmove q  =
  while ((!ia < index_set) && (q >= price_grid.(!ia))) 
  (* 
      "left strict AND" semantics matters in the test above: 
      because !ia can point 1 + higher than the highest possible ask
      when price has escaped up, in which case price_grid would return 
      an "index out of bounds" error 
  *)
    do
    (* bb0aa =a=>  bbb0a *)
    incr rebu; (* we count the number of upcrossings/rebal up *)
    let iia = !ia in 
    (* down transport rule *)
    bid.(iia - 1) <- ask.(iia) *. price_grid.(iia) /. price_grid.(iia - 1); 
    (* +. bid.(iia - 1); second term off if there is a hole = no partial fill *)
    ask.(iia) <- 0.; 
    ib := iia - 1; (* self-filling if holed *)
    incr ia; 
    if (!ia >= index_set) then (incr cross_above)
    done

(* what we do when next price is lower than current *)
and downmove q =
  while ((!ib >= 0) && (q <= price_grid.(!ib)))
    do
    (* bb0aa =b=>  b0aaa *)
    incr rebd; (* we count the number of downcrossings/rebal down *)
    let iib = !ib in 
    (* up transport rule *)
    ask.(iib + 1) <- bid.(iib);
    (* +. ask.(iib + 1);  *)
    (* second term taken off if there is a hole *)
    bid.(iib) <- 0.;
    ia := iib + 1;
    decr ib; 
    if (!ib < 0) then (incr cross_below)
    done
in
(* one step *)
let onestep q =
  let p = !current_price in
  if (q > p)
    then upmove q
    else if (q < p) 
           then downmove q
    ;
    (* new current_price *)
  current_price := q
in

(* 
we consume duration prices, the first one goes to p0, 
the rest = duration - 1 to price moves 
*)
for i = (start + 1) to (start + duration - 1) 
(* ~start:0 ~duration:243_779 *)
  do  
  onestep price_series.(i);
  done
;
(* counting money *)
let qAf = portfolio_A mid (!ia) ask 
and qBf = portfolio_B (!ib) bid price_grid in
let mtmf = qAf *. !current_price +. qBf in
let mtmi = qB in (* qB since we enter only with cash *)
(mtmf /. mtmi -. 1.), 
!rebu, !cross_above, !rebd,  !cross_below, 
price_series.(start + duration - 1)
;;

(* test de sim *)
let test_sim ~duration:x = 
let ps = gen_price_series 
  ~initial_value:1. ~drift:0. ~volatility:0.2 
  ~timestep:0.001 ~duration:1. in
sim 
  (* ~rangeMultiplier:(1.01 ** 20.000_000_1) ~gridstep:1.01  *)
  ~rangeMultiplier:(1.04 ** 5.000_000_1)  ~gridstep:1.04 
  ~quote:10_000.
  ~cashmix:0.5
  ~duration:x
  ~price_series:ps
  ~start:0
;;

let test_sim_rep ~repeats:n ~duration:x = 
  let output = Array.make n (0., 0, 0, 0, 0, 1.) in
  for i = 1 to n
  do 
   (* ret, uc, ux, dc, dx  *)
   output.(i - 1) <- test_sim ~duration:x;
  done;
  let file_string_out = "test_sim.csv" in
  let oc = open_out file_string_out in
  output_string oc  "return, up crossings, up exits, down crossings, down exits, final price\n";
  Array.iter 
    (
      fun (ret, uc, ux, dc, dx, fp) -> 
      output_string oc (string_of_float ret);
      output_string oc ", ";
      output_string oc (string_of_int uc);
      output_string oc ", ";
      output_string oc (string_of_int ux);
      output_string oc ", ";
      output_string oc (string_of_int dc);
      output_string oc ", ";
      output_string oc (string_of_int dx);
      output_string oc ", ";
      output_string oc (string_of_float fp);
      output_char oc '\n'
    )
  output;
  close_out oc
;;



let sim_stopped 
 ~rangeMultiplier:rangeMultiplier 
 ~gridstep:gridstep 
 ~quote:qB 
 ~cashmix:alpha
 ~start:start 
 ~price_series:price_series 
 
=
 
let mid = int_of_float (log(rangeMultiplier) /. log(gridstep)) in
let index_set = 2 * mid + 1 in
 
let price_series_length = Array.length price_series in
let p0 = price_series.(start) in

let current_index = ref start in (* where we are at in the price_series *)
let current_price = ref p0 in
 
let price_grid = generate_price_grid 
~half_number_of_price_points:mid 
~gridstep:gridstep 
~initial_price:p0 in

(* pra 8 3 price_grid; *)
 
let qB' = alpha *. qB in
let qA =  (1. -. alpha) *. qB /. p0 in

let ia, ib, ask, bid = 
populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB' 
~price_grid:price_grid
in


(*  up-/down-crossings and upper/lower exits *)
let rebu = ref 0 in
let rebd = ref 0 in
let upper_exit = ref false in 
let lower_exit = ref false in 

 
 (* next price is higher than current *)
let upmove q  =
  while ((!ia < index_set) && (q >= price_grid.(!ia))) 
   (* 
       "left strict AND" semantics matters in the test above: 
       because !ia can point 1 + higher than the highest possible ask
       when price has escaped up, in which case price_grid in the second term would return 
       an "index out of bounds" error 
   *)
     do
     (* bb0aa =a=>  bbb0a *)
     incr rebu; 
 
     let iia = !ia in 
     bid.(iia - 1) <- ask.(iia) *. price_grid.(iia) /. price_grid.(iia - 1); 
     (* +. bid.(iia - 1); second term off if there is a hole = no partial fill *)
     ask.(iia) <- 0.; 
     ib := iia - 1; (* self-filling if holed *)
     incr ia; 
     if (!ia >= index_set) then (upper_exit:=true)  
     done
 
 (* next price is lower than current *)
 and downmove q =
   while ((!ib >= 0) && (q <= price_grid.(!ib)))
     do
     (* bb0aa =b=>  b0aaa *)
     incr rebd; 
 
     let iib = !ib in 
     (* up transport rule *)
     ask.(iib + 1) <- bid.(iib);
     (* +. ask.(iib + 1); second term taken off if there is a hole *)
     bid.(iib) <- 0.;
     ia := iib + 1;
     decr ib; 
     if (!ib < 0) then (lower_exit:=true)
     done
 in

 (* one step *)
 let onestep q =
   let p = !current_price in
   if (q > p)
     then upmove q
     else if (q < p) 
            then downmove q
    ;
     (* new current_index, current_price *)
     incr current_index;
     current_price := q
 in

(* test price_series not exhausted and price still in range *)
while (!current_index < price_series_length &&
       not !lower_exit && 
       not !upper_exit) 
    do 
     (* move to next price in price_series *)
      onestep price_series.(!current_index);
    done
;

(* print_string "-->  exit index/price = "; 
pad_float (float_of_int !current_index) 3 0;
print_string ", ";
pad_float !current_price 7 5;
print_string "\n"; *)

(* counting money *)
let qAf = portfolio_A mid (!ia) ask 
and qBf = portfolio_B (!ib) bid price_grid in
let mtmf = qAf *. !current_price +. qBf in
let mtmi = qB in 
mtmf /. mtmi, (* growth rate, because more compositional *)
!rebu, !upper_exit,
!rebd, !lower_exit, 
!current_index,
!current_price
;;

(* test sim_stopped *)
let ps = 
  gen_price_series 
  ~initial_value:1. ~drift:0. ~volatility:0.2 
  ~timestep:0.001 ~duration:1. in
sim_stopped 
  ~rangeMultiplier:(1.01 ** 2.000_1) 
  ~gridstep:1.01 
  ~quote:10_000.
  ~cashmix:0.5
  ~start:0
  ~price_series:ps
;;

(* 
shift up/down price grid on new exit current_price (q); 
*)

let rec sim_reset 
  ~rangeMultiplier:rangeMultiplier
  ~gridstep:gridstep
  ~quote:cash
  ~cashmix:alpha
  ~start:start
  ~price_series:ps
  ~total_gamma:tg
  ~uc:uc
  ~hiexit:hix
  ~dc:dc
  ~loexit:lox
=
(* on lance sim_stopped *)
let 
gamma, 
rebu, upper_exit,
rebd, lower_exit,
current_index_in_ps,
current_price_in_ps
=
sim_stopped 
  ~rangeMultiplier:rangeMultiplier
  ~gridstep:gridstep
  ~quote:cash
  ~cashmix:alpha
  ~start:start
  ~price_series:ps
in
(* on accumule les resultats *)
let tg' = tg *. gamma
and uc' = uc + rebu
and hix'  = if (upper_exit) then hix + 1 else hix
and dc' = dc + rebd
and lox'  = if (lower_exit) then lox + 1 else lox
in
if (current_index_in_ps < Array.length ps)
  then 
  sim_reset 
  ~rangeMultiplier:rangeMultiplier
  ~gridstep:gridstep
  ~quote:cash
  ~cashmix:alpha
  ~start:current_index_in_ps
  ~price_series:ps
  ~total_gamma:tg'
  ~uc:uc'
  ~dc:dc'
  ~loexit:lox'
  ~hiexit:hix'
 else 
tg', uc', hix',  dc', lox', current_price_in_ps
;;

let test_sim_reset_4_offers ~duration:duration ~gridstep:ratio ~volatility:vol ~deltaT:deltaT = 
  let ps =
  gen_price_series 
    ~initial_value:1000. ~drift:0. ~volatility:vol
    ~timestep:deltaT ~duration:duration in
  
  let rangeMultiplier = ratio ** 2.000_1
  and quote = 10_000.0
  and mix = 0.5 in
  (* pra 8 4 ps; *)
  sim_reset
    ~rangeMultiplier:rangeMultiplier 
    ~gridstep:ratio
    ~quote:quote
    ~cashmix:mix
    ~start:0
    ~price_series:ps
    ~total_gamma:1.
    ~uc:0
    ~hiexit:0
    ~dc:0
    ~loexit:0
;;

let test_sim_reset_4_offers_rep ~duration:duration ~repeats:n ~gridstep:gridstep ~volatility:vol ~deltaT:deltaT = 
  let output = Array.make n (0., 0, 0, 0, 0, 1.) in
  for i = 1 to n
  do 
   (* ret, uc, ux, dc, dx, fp  *)
   output.(i - 1) <- test_sim_reset_4_offers ~duration:duration ~gridstep:gridstep ~volatility:vol ~deltaT:deltaT;
  done;

(* in hist.py
filestring = 'jobo/csv/test_sim_reset_4_offers_'
df = pd.read_csv(f'{filestring}_{duration}_{nbRep}_{ratio}_{vol}_{deltaT}.csv', 
*)

  let file_string_out_prefix = "jobo/csv/test_sim_reset_4_offers_" in
  let file_string_out_suffix = ".csv" in
  (* let file_string_mid = 
    let sl = [string_of_float duration;
              string_of_int n;
              string_of_float gridstep;
              string_of_float vol;
              string_of_float deltaT] in
    String.concat "_" sl in *)
  let file_string_mid = 
      Printf.sprintf "%.1f_%d_%.3f_%.2f_%.5f" 
      duration n gridstep vol deltaT in
  let file_string_out = file_string_out_prefix^file_string_mid^file_string_out_suffix in
  let oc = open_out file_string_out in
  output_string oc  "return, up crossings, up exits, down crossings, down exits, final price\n";
  Array.iter 
    (
      fun (ret, uc, ux, dc, dx, fp) -> 
      output_string oc (string_of_float (ret -. 1.0)); 
      (* ^^ on parle en return *)
      output_string oc ", ";
      output_string oc (string_of_int uc);
      output_string oc ", ";
      output_string oc (string_of_int ux);
      output_string oc ", ";
      output_string oc (string_of_int dc);
      output_string oc ", ";
      output_string oc (string_of_int dx);
      output_string oc ", ";
      output_string oc (string_of_float fp);
      output_char oc '\n'
    )
  output;
  close_out oc
;;

(* arguments for compiled execution *)
let () =
  let duration = float_of_string Sys.argv.(1) 
  and nbRep = int_of_string Sys.argv.(2) 
  and gridStep = float_of_string Sys.argv.(3) 
  and vol = float_of_string Sys.argv.(4) 
  and deltaT = float_of_string Sys.argv.(5) 
in 
test_sim_reset_4_offers_rep duration nbRep gridStep vol deltaT
;;
