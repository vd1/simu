(* 

TODO:

* study difference bewteen sigmaF and sigmaH
call sigmaF = volatility of GBM recovered from R(\delta t) [Farhat]
call sigmaH = sigma(p)/pzero [Hamza]

* allow for (vQ initial, v'Q/vQ = fraction de cash dévolue à Base) allocations
* implementer un reset sur une condition de prix? 
    chaque fois que p up/down-cross le dernier ask/bid on resplit 50/50 moulo un swap
    et on repart
    1. etape = tester si le prix sort de l'intervalle
* add transportstep
* add short
* decoupler les deux lambda max et min 
* mutualiser les realisation de GBM quand on enumere -eg les gridstep
* ~noil:true -> simply evaluate the position at p(0) instead of p(-1)?
                instead of changing the last price; this neglects 
                the potential (fictitious) buying/selling on the way back to p(0)

SUGG:

- can we do everything with log returns log(p_{n+1}/p_{n}) instead of prices
- liquidity is measured via number of crossings? volume traded?
- est-ce que les returns sont indépendants de ... 
- caveat: dt << gridstep sinon on sous-estime les crossings? 
- check jump distribution after dt rather than dt itself??

- fat crossings:  
  In the case of a "tubular fat price" we check that the price up-crosses "frankly"
      q/(1 - fee)) >= price_grid.(!ia)
  with fees 1bps on both local mkt and arb's source
  can also add gasCosts = 0.02 but then the constraint for profitability depends on Volume traded:
  V * (1 - mangroveFee) * (1 - uniFee) * p(Uni) > V * p(askKandle) + gasCosts

*)


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
  (fun x -> pad_float x length trunc;   print_string "| ")
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

let array3_to_csv 
~filename:filename ~array3:res = 
  let file_string_out = "csv/"^filename^".csv" in
  let oc = open_out file_string_out in
  output_string oc  "# gridstep ; ret; crossings\n";
  Array.iter 
    (
      fun (price_inc, return, nb_of_crossings) -> 
      output_string oc (string_of_float price_inc);
      output_string oc ", ";
      output_string oc (string_of_float return);
      output_string oc ", ";
      output_string oc (string_of_int nb_of_crossings);
      output_char oc '\n'
    )
  res;
  close_out oc
;;

let moving_average n arr2 =
  let len = Array.length arr2 in
  let simresult = Array.make len (0.,0.) in
  for i = 0 to len - 1 do
    let sum = ref 0. in
    let count = ref 0 in
    for j = max 0 (i-n+1) to i do
      sum := !sum +. snd arr2.(j);
      incr count;
    done;
    simresult.(i) <- (fst arr2.(i), !sum /. float_of_int !count);
  done;
  simresult
;;

(* --------------------- BROWNIANS --------------------- *)

let s  = Random.State.make_self_init ();;
let s' = Random.State.make_self_init ();;

let pi = 4.0 *. atan 1.0;;

(* box_muller: [0,1]^2 -> R: G(box_muller)(U_2) = N(0,1) *)
(* grad box_muller (u1,u2) =  ...  *)
let box_muller u1 u2 =
  let r = sqrt (-2.0 *. log u1) in
  let theta = 2.0 *. pi *. u2 in
  r *. cos theta
;;

let normal_random () =
  box_muller (Random.State.float s 1.) (Random.State.float s' 1.)
;;

(* O(nb_samples) calculation of mean and return *)
let mean_estimate nb_repeats =
  assert (nb_repeats > 0);
  let sum = ref 0. in
  let sumofsquares = ref 0. in
  for i = 1 to nb_repeats
    do
    let rand = normal_random () in
    sum := rand +. !sum;
    sumofsquares := rand ** 2. +. !sumofsquares
    done
    ;
  let n = float_of_int nb_repeats in
  let mean = !sum /. n in
  let var = !sumofsquares /. n -. mean ** 2. in
  mean, sqrt var
;;

(* |N(0,1)| > 2 sigma with probability 9% *)
let xyz n = 
let simres = ref 0 in
for i = 1 to n 
  do
    let v = abs_float (normal_random ()) in
      if ( v > 2.) 
      then incr simres
  done;
(float_of_int !simres) /. (float_of_int n)
;;

let browmo 
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration =
  let steps = int_of_float (duration /. dt) + 1 in (* infinite if dt = 0! *)
  let series = Array.make steps (0.,0.) in
  series.(0) <- (0., ival);
  for i = 1 to (steps - 1)
    do 
      let dt_increment = drift *. dt in
      let sqrt_dt_increment = vol *. normal_random () *. sqrt(dt)  in
      let (timestamp, value) = series.(i - 1) in
      let increment = (dt_increment +. sqrt_dt_increment) in
      series.(i) <- (timestamp +. dt, value +. increment)
    done;
  series
;;

let gbrowmo
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration =
  let steps = int_of_float (duration /. dt) + 1 in 
  let series = Array.make steps (0.,0.) in
  series.(0) <- (0., ival);
  for i = 1 to (steps - 1)
    do 
      let dt_increment = drift *. dt in
      let sqrt_dt_increment = vol *. normal_random () *. sqrt(dt)  in
      let (timestamp, value) = series.(i - 1) in
      let increment = value *. (dt_increment +. sqrt_dt_increment) in
      series.(i) <- (timestamp +. dt, value +. increment)
    done;
  series
;;

(* 
gbrowmo ~initial_value:1. ~drift:0. ~volatility:0.1 ~timestep:0.01 ~duration:0.03;; 
*)

(* 
alternative construction of GBM
  Y_0 = \exp(\sig B_0)
  Y_t = \exp((\mu - \sig^2/2)t + \sig B_t)
- how to compare and make sure that both methods agree?
- why store intermediate points ?
- what is the proper size of dt in relation to volatility?
- if \mu = 0, \sig = 0.01, Y_t =  \exp(-0.5 10^{-4} t) *  \exp(0.01 * B_t)
*)

let gbrowmo2 
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration =
  let ival2 = log(ival) /. vol in
  let drift2 = drift -. (vol ** 2.)/. 2.  in
  let arr2 = browmo 
  ~initial_value:ival2 ~drift:drift2 ~volatility:vol
  ~timestep:dt ~duration:duration in
  Array.map 
    (fun (t,v) -> (t,exp(v)))
    arr2
;;

(* generates nb_of_rays new trajectories each in separate file *)
let gbm_sim 
?(nb_of_rays = 1)
?(initial_value = 1.) ?(drift = 0.01) ?(volatility = 0.01) 
?(timestep = 1./.1440.) ?(duration = 1.)
() = 
for i = 1 to nb_of_rays
do
 let gb = gbrowmo 
 ~initial_value:initial_value ~drift:drift ~volatility:volatility 
 ~timestep:timestep ~duration:duration in
 let filename = "gm/gbm2"^(string_of_float volatility)^"_"^(string_of_int i) in
 array2_to_csv ~filename:filename ~array2:gb
done
;;

let gen_price_series 
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration = 
(* steps = duration/dt + 1 *)
Array.map (fun (_,x) -> x) 
          (gbrowmo 
             ~initial_value:ival ~drift:drift ~volatility:vol 
             ~timestep:dt ~duration:duration)
;;

(* ~timestep:0.0007 = 1/1440 = once per minute if duration = 1d *)

(*   
gen_price_series 
~initial_value:1. ~drift:0. ~volatility:0.10 
~timestep:(1. /. 1440.) ~duration:0.1
;; 
*)


(* 
   inputs: discrete GBM parameters
   outputs: array of pairs (time, log return) 
   number of simulation steps = duration/dt + 1 
*)

let gen_log_returns 
~initial_value:ival 
~drift:drift 
~volatility:vol 
~timestep:dt 
~duration:duration = 
  let array_of_pairs = gbrowmo 
  ~initial_value:ival
  ~drift:drift
  ~volatility:vol 
  ~timestep:dt
  ~duration:duration in
  Array.init  
  (Array.length array_of_pairs)
  (
  fun i -> 
    if (i = 0) 
      then (0.,0.) 
      else 
        (
        let _, pred_price = array_of_pairs.(i-1) in
        let t, next_price = array_of_pairs.(i) in
        t, log(next_price /. pred_price)
        )
  )
;;

(*
let lrs =  
  gen_log_returns 
  ~initial_value:1. ~drift:0. ~volatility:0.1 
  ~timestep:0.001 ~duration:0.1 in
pra 10 6 (Array.map (fun (x,y) -> y) lrs)
;;
*)

(* inference of mean and std *)
(* 
we apply the R(delta t) = log(X(t+delta t)/X(t)) formula to derive mu, sig
m = (\mu -\sig^2/2) \da t
s = \sig\sqrt{\da t}
sig = s/\sqrt{\da t}
mu  = 1/\da t(m + s^2/2) 
*)

let infer_mean_std dt arr2 = 
let sum = ref 0. in
let sumofsquares = ref 0. in
let n = Array.length arr2 in 
for i = 0 to (n - 1)
  do
  let x,y = arr2.(i) in
  sum := y +. !sum;
  sumofsquares := y ** 2. +. !sumofsquares
  done
  ;
let nf = float_of_int n in
let mean = !sum /. nf in
let var = !sumofsquares /. nf -. mean ** 2. in
  (mean +. var /. 2. ) /. dt,
  sqrt (var /. dt)
;;

(* 
conclusion: sigma is well-inferred, but mean still unstable after 10_000 samples
let lrs =  
  gen_log_returns 
  ~initial_value:1. ~drift:0. ~volatility:0.1 
  ~timestep:0.001 ~duration:10. in
infer_mean_std 0.001 lrs;;
- : float * float = (0.0315092310528696379, 0.0992095041088259)
 *)

(* 
   input: price series
   outputs: csv file with the log returns 
*)

let log_return 
~price_series:ps 
~filename:filename = 
  (*  eg filename = "csv/logret.csv"*)
  let oc = open_out filename in
  let l = Array.length ps in
  (* let log_ret = Array.make (l - 1) 1. in *)
  for i = 1 to (l - 1)
    do  
    (* log_ret.(i-1) <- log (ps.(i) /. ps.(i-1)); *)
    let next = log (ps.(i) /. ps.(i-1)) in
    output_string oc (string_of_int i);
    output_string oc ", "; 
    Printf.fprintf oc "%.6f" next;
    output_string oc "\n";
    done;
    close_out oc
;;

(* 
let x = gen_log_returns 
~drift:1. ~volatility:0.05 ~duration:10. ~timestep:0.001 in
array2_to_csv ~filename:"logret4" ~array2:x
;; 
*)


(* --------------------- UNISWAP --------------------- *)
(* input: a (time,price) series, and a fee eta
   outputs: eta-filtered transforn of input price series *)
(* vf = viscous filter *)
(* eta is the fee *) 
let vf 
~driver_series:(p:(float*float) array) 
~viscosity:(eta:float) = 
  let u = ref 0 in
  let d = ref 0 in
  let n = Array.length p in
  let driven_series = Array.make n (0.,0.) in
  (* we copy the first price by convention so both series start at the same value *)
  driven_series.(0) <- p.(0); 
  for i = 1 to (n - 1) 
    do
    (* let _, current_driver_price = p.(i-1) in *)
    let s, next_driver_price = p.(i) in
    let _, current_driven_price = driven_series.(i-1) in
    driven_series.(i) <- (s,current_driven_price); (* copy new time, old price *)
    if (next_driver_price > current_driven_price *. (1. +. eta)) (* eta up crossing *)
      then 
        ( 
           driven_series.(i) <- (s, next_driver_price /. (1. +. eta)); (* catch up *)
           incr u; 
        );
    if (next_driver_price < current_driven_price /. (1. +. eta)) (* eta down crossing *)
      then 
        (
          driven_series.(i) <- (s, next_driver_price *. (1. +. eta)); (* catch down *)
          incr d;
        );
    done;
   driven_series, !u, !d
;;

(* testing driven price series for various values of viscosity *)
let test_filter () = 
  let ds = browmo 
  ~initial_value:1. ~drift:0. ~volatility:0.1 ~timestep:0.01 ~duration:10. in
  array2_to_csv ~filename:"vs/vs0" ~array2:ds;  
  let output1,_,_ = vf ~driver_series:ds ~viscosity:(0.2) in
  let output2,_,_ = vf ~driver_series:ds ~viscosity:(0.1) in
  let output3,_,_ = vf ~driver_series:ds ~viscosity:(0.05) in
  let output4,_,_ = vf ~driver_series:ds ~viscosity:(0.01) in
  (* let output4,_,_ = vf ~driver_series:ds ~viscosity:(0.005) in *)
  array2_to_csv ~filename:("vs/vs1") ~array2:output1;
  array2_to_csv ~filename:("vs/vs2") ~array2:output2;
  array2_to_csv ~filename:("vs/vs3") ~array2:output3;
  array2_to_csv ~filename:("vs/vs4") ~array2:output4;
;;

(* vsr = variation square root *)
(*  computing up/down square root variation of given price series *)
(* NB: les fees divergent quand dt -> 0 !! *)
let vsr (p: float array) = 
   let vsrb = ref 0. in
   let vsrq = ref 0. in
   for i = 1 to (Array.length p - 1)
     do
       if (p.(i) >= p.(i-1)) (* up move *)
         then 
          let sq = (sqrt(p.(i)) -. sqrt(p.(i-1))) in
          vsrq := !vsrq +. sq;
         else  (* down move *)
          let sb = (sqrt(1./. p.(i)) -. sqrt(1./. p.(i-1))) in
          vsrb := !vsrb +. sb; 
     done;
  !vsrq, !vsrb
;;

(* 
fees divergence 
# let p = gen_price_series 
~initial_value:1. ~drift:0. ~volatility:0.10 
~timestep:(1. /. 10000.) ~duration:1.
in vsr p;;
- : float * float = (1.95303288845039336, 2.06327514035797366)

# let p = gen_price_series 
~initial_value:1. ~drift:0. ~volatility:0.10 
~timestep:(1. /. 100000.) ~duration:1.
in vsr p;;
- : float * float = (6.25414645146637493, 6.36145002711934549)
─ 
*)

(* symmetric concentrator  *)
let concentrator p_0 rangeMultiplier = 
  assert(rangeMultiplier > 1.);
  let sqlambda = (sqrt rangeMultiplier) in (* > 1 *)
  0.5 *. sqlambda /. (sqlambda -. 1.0) *. 1. /. sqrt(p_0)
;;

(* feeq = capital(quote) * eta * (concentrator p_0 rangeMultiplier) * vsrq(eta) *)
(* feeb = capital(quote) * eta * concentrator * vsrb(eta) *)

(* 
0. gridstep GBM parameters 
initial value = 1, drift = 0, volatility in [0.055: 0.095], 
timestep = 0.001, duration = 1.; 
1. choose rangeMultiplier = 1.4 (symmetric rangeMultiplier);
2. symmetric rangeMultiplier implies p_0 * initialBase =  initialQuote [equipartition]; 
   hence for p_0=1, initialBase =  initialQuote
3. vary fee (just like gridstep) between 1.001 and 1.200
4. for fixed (vol,fee) compute mean return
(MtM au prix initial: 
a) append p(0), or not if faster 
b) return = feequote + feebase); do quadratic fit and plot 
*)

let unitFees ~viscosity:eta ~volatility:vol = 
  let entryPrice = 1.0 in
  let rangeMultiplier = 1.4 in
  let sqlambda = (sqrt rangeMultiplier) in (* > 1 *)
  let prefactor = sqlambda /. (sqlambda -. 1.0) /. (2. *. (sqrt entryPrice)) in
  let ds = gbrowmo 
  ~initial_value:entryPrice ~drift:0. ~volatility:vol 
  ~timestep:0.001 ~duration:1. in
  let viscous_price_series,_,_ = 
    vf ~driver_series:ds
       ~viscosity:eta in
  let viscous_price_series_without_time = Array.map (fun (x,y) -> y) viscous_price_series in
  let vsrq, vsrb = vsr viscous_price_series_without_time in
  let unitQuoteFee = prefactor *. eta *. vsrq in
  let unitBaseFee  = prefactor *. eta *. vsrb in
  (* assuming pfinal = pinitial to simplify *)
  unitQuoteFee +. entryPrice *. unitBaseFee 
;;

let fees_repeat ~nb_of_rays:n ~viscosity:eta ~volatility:vol = 
  let simres = ref 0. in
  for i = 1 to n
    do 
     simres := !simres +. (unitFees ~viscosity:eta ~volatility:vol);
   done;
!simres /. (float_of_int n)  
;;

(* fees_repeat ~nb_of_rays:1000 ~viscosity:0.01 ~volatility:0.055;;
- : float = 0.00219066824053387168 *)


let fees_repeat_eta ~nb_of_rays:n ~volatility:vol = 
  let output = Array.make 200 (0.,0.) in
  for i = 1 to 200
  do 
   let fee = (float_of_int i) *. 0.001 (* 0.001 -> 0.2 par step = 0.001 *) in
   let mean_return = fees_repeat ~nb_of_rays:n ~viscosity:fee ~volatility:vol in
   output.(i - 1) <- (fee, mean_return);
  done;
  let filename = "vs/unimr"^(string_of_float vol) in
  array2_to_csv ~filename:filename ~array2:output
;;


(* --------------------- DATA --------------------- *)
(* Loading price_series from a csv file into an array *)
(* we read nb_lines after dropping the first one *)
let h ~number_of_lines:nb_lines ~filename:filename = 
  (* let  nb_lines =  Sys.command ("wc -l "^filename) in *)
  (* print_int nb_lines; print_newline(); *)
  let ic = open_in filename in
  let _  = input_line ic in (* we ditch the first line of the file *)
  let price_series = Array.make nb_lines 1.0 in
  for i = 0 to (nb_lines - 1)
    do 
      let sline = input_line ic in
      let esline = String.split_on_char ',' sline in
      let current_price = List.nth esline 7 in (* here just the number of the column of interest *)
      price_series.(i) <- float_of_string current_price
    done;
  close_in ic;
  price_series
;;

(* 
let price_series  =  
(* h 6748 "Kandle_benchmark_data.csv"  *)
h ~number_of_lines:243780 ~filename:"csv/data_for_vd.csv" 
;; 
*)


(* 
let ps = Array.sub price_series 0 3;;
print_float ps.(0); print_newline ();
print_float ps.(1); print_newline ();
print_float ps.(999); print_newline ();
;; 
*)

(* I the index set for the various state elements, depends only on rangeMultiplier and step *)
(* let build_index_set rangeMultiplier step = 
  int_of_float (log(rangeMultiplier) /. log(step))  
;; *)

(* 
log linear price subdivision 
we build a multiplicatively symmetric price grid centered on p0
NB: mid >= 1
NB: total number of points = index_set >= 3
NB: pmin >= p0 /. rangeMultiplier and pmax <= p0 *. rangeMultiplier, not necessarily equal because of rounding
pmin/max = p0 * (gridstep ** +/- mid)
*)

(* --------------------- PRICE GRID --------------------- *)
let generate_price_grid 
~half_number_of_price_points:mid 
~gridstep:gridstep 
~initial_price:p0 = 
  Array.init 
    (2 * mid + 1) 
    (fun i -> let expo = float_of_int (i - mid) in
              p0 *. gridstep ** (expo)) 
;;
(* 
   NB: the price grid is uniform in p0 ;
   OPTIM: qd on itere le long de la price_series
   en utilisant les mêmes paramètres,
   et donc en ne changeant que le mid-price  
   definir statiquement le price_grid_skeleton := le price_grid pour p_0=1
   et faire ensuite a chaque composition
   Array.map (fun x -> p0 * x) price_grid_skeleton
   ou même 
   let pg = price_grid_as_a_function new_starting_price in
   et ensuite on compose: sigma(p2, d, (sigma(p1, d, sig(p0, d, qB0));
   si on a aussi acces au current_volatility et la Garchery map step(current_vol)
   on peut aussi mettre à jour le step
*)

(* 
   NB: number of points for various values of gridstep
   given by log(rangeMultiplier) /. log(gridstep);; 
*)

let generate_price_grid_shifted
~half_number_of_price_points:mid 
~shift:k
~gridstep:gridstep 
~initial_price:p0 = 
let refined_gridstep = gridstep ** (1. /. (float_of_int k)) in
let refined_mid = mid * k in
  Array.init 
    (2 * refined_mid + 1) 
    (fun i -> let expo = float_of_int (i - refined_mid) in
              p0 *. refined_gridstep ** (expo)) 
;;

(* 
let aux ~rangeMultiplier:lambda n = 
(* eg lambda = 1.4 *)
print_string "nb of points on half grid: ";
for i = 1 to n 
do 
let gridstep = 1. +. 0.001 *. (float_of_int i) in
print_int(int_of_float (log(lambda) /. log(gridstep)));
(* log(lambda)/log(ratio) = nb_of_points *)
print_string ", ";
done
;; 
*)


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

(* computing the amount of B in the book *)
let portfolio_B iib bid price_grid = 
  let simres = ref 0. in 
  for i = 0 to (iib) 
    do
    simres := !simres +. price_grid.(i) *. bid.(i)
    done  
    ;
  !simres 
;;

(* computing the amount of A in the book *)
let portfolio_A mid iia ask = 
  let simres = ref 0. in 
  for i = (iia) to (2 * mid)
    do
    simres := !simres +. ask.(i)
    done  
    ;
  !simres 
;;

(* 
let step =  1.1 in 
for i = 0 to 10
do
let x = geo (step ** (float_of_int i)) step in 
print_float(1_000_000_000_000. *. (x -. floor x));
  print_newline()
done
;; 
*)

(* --------------------- KANDLE SIMULATION --------------------- *)
(* 
inputs: 
1) strat parameters = price grid, and capital in cash (quote, written qB) and cashmix
2) price series 
3) start = when to enter game and duration = how long to play
outputs: 
return (the stochastic integral of strat against price)
number of up- and down-crossings
NB: duration could be a stopping time in general, eg looking at a price-crossing event 
NB: no need to slice the price series as the price series array is read only
*)

let sim 
(* the parameters chosen = investment decision *)
~rangeMultiplier:rangeMultiplier 
~gridstep:gridstep 
~quote:qB 
~cashmix:alpha (* 0≤alpha≤1 *)
~start:start 
~duration:duration 
(* the future *)
~price_series:price_series 
=
(* check we have enough points in the price series *)
assert(start + duration <= (Array.length price_series));

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

(* 
pra 12 bid; print_string "\n";
print_float (portfolio_B mid bid price_grid); print_newline();
pra 12 ask; print_string "\n";
print_float (p0 *. (portfolio_A mid (!ia) ask)); print_newline(); 
*)

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

    (* print_string "new price "; print_float(q); print_string "@ time "; print_int(i); print_newline();
    print_string "old price ";print_float(!current_price); print_newline(); *)
    (* pra 12 price_grid; print_newline(); *)
    (* print_string "> bids: "; print_newline(); pra 12 bid; print_newline();
    print_string "> asks: "; print_newline(); pra 12 ask; print_newline(); *)

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

    (* print_string "new price "; print_float(q); print_string "@ time"; print_int(i); print_newline();
    print_string "old price ";print_float(!current_price); print_newline(); *)

    (* pra 12 price_grid; print_newline();
    pra 12 bid; print_newline();
    pra 12 ask; print_newline(); *)
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
(* pra 6 price_grid; print_newline();
pra 6 bid; print_newline();
print_float (portfolio_B mid bid price_grid); print_newline();
pra 6 ask; print_newline();
print_float (p0 *. (portfolio_A mid (!ia) ask)); print_newline(); *)
(* we gridstepk up prices one at a time and update state *)

(* print_string "p0>"; print_float price_series.(start); print_newline();
print_string "p1>"; print_float price_series.(start + 1); print_newline();
print_string "p last>"; print_float price_series.(start + duration -1); print_newline(); *)

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
  (*
  print_string ("gridstep = ");
  print_float (step -. 1.);
  print_string ("start  = ");
  print_int (start + 1);
  print_string (" -> return = ");
  print_float(mtmf /. mtmi -. 1.); 
  print_string ("\n");
  print_string "current_price = ";
  print_float !current_price;
  print_string ("\n"); 
  *)
  (* 
  pra 6 price_grid; print_newline();
  pra 6 bid; print_newline();
  print_float (portfolio_B mid bid price_grid); print_newline();
  pra 6 ask; print_newline();
  print_float (!current_price *. (portfolio_A mid (!ia) ask)); print_newline(); 
  *)
(* print_float(!current_price);
print_string("\n");
print_float mtmf;
print_string("\n");
print_float mtmi;
print_string("\n"); *)
(mtmf /. mtmi -. 1.), !rebu, !cross_above, !rebd,  !cross_below
;;

(* test de sim *)
let ps = 
  gen_price_series 
  ~initial_value:1. ~drift:0. ~volatility:0.2 
  ~timestep:0.001 ~duration:1. in
sim 
  ~rangeMultiplier:(1.01 ** 20.000_000_1) 
  ~gridstep:1.01 
  ~quote:10_000.
  ~cashmix:0.5
  ~duration:1000
  ~price_series:ps
  ~start:0
;;


 (* 

 somme translatée 
 pg(mid, gs) + 
 pg(mid+1/2 gs, gs)
 ST k (pg(p0, mid, gs)) :=
 pg(p0, k*mid, gs/k)

  b_0_a_ +
  _b_0_a = 
  bb00aa -a-> bb00aa
    
  *)

let sim_stopped 
 ~rangeMultiplier:rangeMultiplier 
 ~gridstep:gridstep 
 ~quote:qB 
 ~cashmix:alpha (* 0≤alpha≤1 *)
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
 
let qB' = alpha *. qB in
let qA =  (1. -. alpha) *. qB /. p0 in

let ia, ib, ask, bid = 
populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB' 
~price_grid:price_grid
in


(*  tracking up-/down-crossings and upper/lower xits *)
let rebu = ref 0 in
let rebd = ref 0 in
let upper_exit = ref false in 
let lower_exit = ref false in 

 
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
     incr rebu; 
 
     let iia = !ia in 
     bid.(iia - 1) <- ask.(iia) *. price_grid.(iia) /. price_grid.(iia - 1); 
     (* +. bid.(iia - 1); second term off if there is a hole = no partial fill *)
     ask.(iia) <- 0.; 
     ib := iia - 1; (* self-filling if holed *)
     incr ia; 
     if (!ia >= index_set) then (upper_exit:=true)  
     done
 
 (* what we do when next price is lower than current *)
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

(* test price_series not exhausted and price still  in range *)
while (!current_index < price_series_length &&
       not !lower_exit && 
       not !upper_exit) 
    do 
     (* move to next price in price_series *)
      onestep price_series.(!current_index);
    done
;

print_string "index/price = "; 
pad_float (float_of_int !current_index) 3 0;
print_string ", ";
pad_float !current_price 7 5;
print_string "\n";

(* counting money *)
let qAf = portfolio_A mid (!ia) ask 
and qBf = portfolio_B (!ib) bid price_grid in
let mtmf = qAf *. !current_price +. qBf in
let mtmi = qB in 
mtmf /. mtmi, (* growth rate *)
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
tg', uc', hix',  dc', lox'
;;


(* test sim_reset vs sim (no reset) *)
(* maybe sim_reset vs  *)
let test_sim_reset_vs_sim ~gridstep:gridstep ~volatility:vol = 
let ps =
gen_price_series 
  ~initial_value:1. ~drift:0. ~volatility:vol
  ~timestep:0.001 ~duration:1. in

let rangeMultiplier = gridstep ** 2.000_1
and quote = 10_000.0
and mix = 0.5 in

sim_reset
  ~rangeMultiplier:rangeMultiplier 
  ~gridstep:gridstep
  ~quote:quote
  ~cashmix:mix
  ~start:0
  ~price_series:ps
  ~total_gamma:1.
  ~uc:0
  ~hiexit:0
  ~dc:0
  ~loexit:0
,
sim
    ~rangeMultiplier:rangeMultiplier
    ~gridstep:gridstep 
    ~quote:quote
    ~cashmix:mix
    ~start:0
    ~duration:(Array.length ps)
    ~price_series:ps  
;;

let ps = 
  gen_price_series 
    ~initial_value:1. ~drift:0. ~volatility:0.1 
    ~timestep:0.001 ~duration:1. in
  sim
    ~rangeMultiplier:(1.005 ** 2.000_1) 
    ~gridstep:1.005 
    ~quote:10_000.
    ~cashmix:0.5
    ~start:0
    ~duration:(Array.length ps)
    ~price_series:ps
;;
  



(* --------------------- REPEATS --------------------- *)

let bundle_sim ~start:start ~duration:duration ~repetitions:repetitions ~price_series:price_series = 
  let siim start duration = 
  sim 
  ~rangeMultiplier:(1.01 ** 20.000_000_1) 
  ~gridstep:1.01 
  ~quote:10_000.
  ~cashmix:0.5
  ~start:start 
  ~duration:duration
  ~price_series:price_series
  in
  let simres = Array.make repetitions (0., 0, 0, 0, 0) in 
  (* here: how many times we launch a simulation *)
  for i = 0 to (repetitions - 1)
    do
    (* fst arg of siim = where to start in the price series
       snd arg = how many prices to consume in this series *)
    simres.(i) <- siim (start + i * duration) duration
    done;
    Array.iter (fun (f,nb_upcrossing, nb_downcrossing, nb_high_exits, nb_low_exits) -> 
      print_float f; 
      print_string ", "; 
      print_int nb_upcrossing; 
      print_string ", ";
      print_int nb_downcrossing; 
      print_string ", ";
      print_int nb_high_exits; 
      print_string ", ";
      print_int nb_low_exits; 
      print_string "\n") simres
  ;;
  
  (* 
  bundle_sim ~start:0 ~duration:40_000 ~repetitions:6 ~price_series:price_series_to_define
  ;;
   *)
  
(* inputs: gridstep sampling parameters, GBM parameters *)
(* outputs: array of (gridstep, return, crossings) for one realisation *)
(* gridstep aka price increment aka gridstep *)
(* NB: we share the GBM realisation "ps" across all gridsteps *)
(* NB: rangeMultiplier too wide -> reduction of capital -> loss of return *)

let gridsampling 
~gridsteptick:tick ~maxtick:maxtick ~rangeMultiplier:rangeMultiplier
~quote:vQ
~cashmix:alpha
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration 
~noil:x = 

assert (tick >= 0.001);   (* minimum resolution of gridsteps tested *)
assert (tick <= maxtick); 
assert (1. +. maxtick <= rangeMultiplier); (* should stay in range *)

let ps = (* OPTIM: could be optimised with one less Array.map *)
gen_price_series  
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration in
(* conditional reset last price for IL *) 
if (x) then ps.(Array.length ps - 1) <- ps.(0);
  
(* iterating over gridsteps one with increment tick until maxtick: 
   r(1) = 1 + tick
   r(i) = 1 + i * tick 
   r(n) = 1 + n * tick = 1 + maxtick
   n * tick = maxtick
   eg 1.001, 1.002, ..., 1.1
   n = 100
*)
(* j'enleve le - 1 in (* - 1?? NB: array empty if nbs = 0 *)*)
let nbs = int_of_float (maxtick /. tick) in 
let simres = Array.make nbs (0., 0., 0) in
for i = 1 to nbs  
  do
  let gridstep = (1. +. (float_of_int i) *. tick) in
  (* gridstep < rangeMultiplier because gridstep <= 1 + maxtick <=  rangeMultiplier *)
  let r, u, hix, d, lox = 
  sim 
  ~rangeMultiplier:rangeMultiplier (* we fix the rangeMultiplier and vary only gridstep *)
  ~gridstep:gridstep
  ~quote:vQ
  ~cashmix:alpha
  ~start:0
  ~duration:(Array.length ps) (* <- number of price values in the time series, not to be confused with duration in time *)
  ~price_series:ps 
  in
  simres.(i-1) <- (gridstep, r, u+d)
  (* print_float r; print_string ", ";
     print_int u; print_string ", ";
     print_int d; print_string "\n"; *)
  done;
  simres
;;


(* 
fix rangeMultiplier and duration,
build correspondence: volatility |-> best gridStep 
to find optimal gridstep, we repeat gridsampling 
number_of_rays times and collect mean and std 
then it is up to us to define "best",
it can be: Sharp, meanreturn - gamma * varreturn, etc
*)

let barg 
~number_of_rays:number_of_rays 
~gridsteptick:tick ~maxtick:maxtick ~rangeMultiplier:rangeMultiplier
~quote:vQ
~cashmix:alpha
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration ~noil:x 
=
let nbs = int_of_float (maxtick /. tick) in
(* mean_return and std_return store sum and sumofsquares before normalising *)
let mean_return = Array.make nbs 0. 
and std_return  = Array.make nbs 0. 
and mean_crossings_int = Array.make nbs 0
in
for i = 1 to number_of_rays
  do  
  let sim_res = gridsampling 
  ~gridsteptick:tick ~maxtick:maxtick ~rangeMultiplier:rangeMultiplier
  ~quote:vQ
  ~cashmix:alpha
  ~initial_value:ival ~drift:drift ~volatility:vol  
  ~timestep:dt ~duration:duration ~noil:x in 
  Array.iteri 
  (
    fun i (gridstep,x,c)  -> 
    mean_return.(i)    <- mean_return.(i) +. x;
    std_return.(i)     <- std_return.(i)  +. x**2.0;
    mean_crossings_int.(i) <- mean_crossings_int.(i) + c
  )
  sim_res;
  done;
  let n = (float_of_int number_of_rays) in
  Array.iteri 
    (fun i x -> (mean_return.(i) <- x /. n)) 
    mean_return;
  Array.iteri 
    (fun i x -> (std_return.(i)  <-  sqrt (x /. n -. mean_return.(i) ** 2.0)))
    std_return;
  mean_return, 
  std_return, 
  Array.map
    (fun x -> ((float_of_int x) /. n)) 
    mean_crossings_int;
;;


(* 
   pourquoi tant de zeros "à droite" pour les valeurs plus hautes de gridstep
   dans mean (et donc dans std)? 
   return = mtmf/mtmi - 1 so perhaps because of reset ??
   resultats instables!
*)

(* quel est l'impact of rangeMultiplier?    *)

let bc 
~number_of_rays:n 
~volatility:vol 
~rangeMultiplier:rangeMultiplier 
= 
let mr, sr, mc = 
barg 
~number_of_rays:n
~gridsteptick:0.001
~maxtick:(rangeMultiplier -. 1.)
~rangeMultiplier:rangeMultiplier
~quote:20_000.
~cashmix:0.9
~initial_value:1.
~drift:0. (* (-0.1) *)
~volatility:vol
~timestep:0.001
~duration:1.
~noil:false in
(* 
print_string "mean ret  "; pra 7 3 mr; print_newline ();
print_string "std ret   "; pra 7 3 sr; print_newline ();
print_string "mean xing "; pra 7 3 mc; print_newline (); 
*)
let maxsh = ref (-1.0) in 
let index = ref 0 in
for i = 0 to (Array.length mr - 1)
 do
 let sh = mr.(i)/.sr.(i) in
 if  sh > !maxsh then (maxsh:= sh; index:=i)
 done;
 (* print_string "grid_step, max_sharpe, mean_return, std_return, mean_xing"; *)
((float_of_int !index) *. 0.001, !maxsh, mr.(!index),sr.(!index),mc.(!index))
;;

let () =
let nbr = int_of_string Sys.argv.(1) 
and vol =  float_of_string Sys.argv.(2) 
and rangeMultiplier = float_of_string Sys.argv.(3)
in
(* we slice the rangeMultipler into 50 ticks *)
let tronc = (rangeMultiplier -. 1.)/. 50. in
for i = 1 to 50
do
let range = 1. +. (float_of_int i) *. tronc in
let bestgs, maxsh, mr, sr, mc =
bc 
~number_of_rays:nbr 
~volatility:vol
~rangeMultiplier:range 
in
(* print_string Sys.argv.(1); print_string " reps, vol ";
print_string Sys.argv.(2); print_string ", range ";
print_string Sys.argv.(3); print_string "\n"; *)
print_string "-> gridstep(range) ";
print_float bestgs;
print_string "(";
print_float range;
print_string "), max sharpe ";
print_float maxsh;
(* pad_float maxsh 5 4; *)
print_string ", mean ret ";
print_float mr; 
(* pad_float mr 5 4; *)
print_string ", std ret ";
(* pad_float sr 5 4; *)
print_float sr; 
print_string ", mean xing ";
print_float mc; 
(* pad_float mc 5 4; *)
print_string "\n";
done
;;

(* 
parametres:
vol   0.01 -0.01-> 0.1

range 1.01 -0.01-> 1.4

= 10 v * 40 r 
soit par exemple 400 fichiers/tableaux 
exemple de syntaxe vol0.05_range1.20.csv 

si on prend gstick = 0.001 uniformément et
maxgstick = range - 1 (vu que les range sont des multiples de gstick)
le nb de lignes/elements du fichier/tableau 
volx_rangey.csv sera (y-1)/0.001 -> 
soit au max 0.4/0.001 = 400
et au min 0.01/0.001 = 10

si au contraire on fait varier le mid   1 -> 20 
les petits range vont avoir des tick plus petits: 
range = 1.01 -> gstick = 0.005
range = 1.2  -> gstick = 0.1 trop grossier??

en format csv:
1+4 col: # gridstep, #mr, #sr, #mc, #sharpe
et un nombre variable de lignes? 
dans l'experience classique range = 1.4, gstick = 0.001, maxgstick = 0.2, soit N = 200
en general N = maxgstick/gstick

OK suppose que tu as ces 400 fichiers/tableaux, 
tu fais quoi?
selectionne le best Sharpe pour lastSig?
*)




(*
let scan_vol n = 
for i = 1 to n
do  
let vol = 0.005 *. (float_of_int i) in
let volstring = string_of_float vol in 
let filename = "noil/meanret"^volstring in
let mr, sr, mc = barg 
~number_of_rays:1000 
~gridsteptick:0.001 ~maxtick:0.2 ~rangeMultiplier:1.4 ~quote:10_000.
~initial_value:1. ~drift:0. ~volatility:vol
~timestep:0.001 ~duration:1. ~noil:false in 
array2_to_csv ~filename:filename ~array2:mr
done
;;

let sweepy_barg ~number_of_rays:nr ~number_of_vols:nv =
for i = 1 to nv (* nv = 10 *)
  do
  let vol = 0.005 *. (float_of_int i) in
  let volstring = string_of_float vol in 
  let mr, sr, mc = barg ~number_of_rays:nr 
  ~gridsteptick:0.001 ~maxtick:0.2 ~rangeMultiplier:1.4 ~quote:10_000.
  ~initial_value:1. ~drift:0. ~volatility:vol 
  ~timestep:0.01 ~duration:1. in 
  let fileprefix = "gm/gm"^volstring in
  array2_to_csv ~filename:fileprefix ~array2:mr
  done
;;

*)

(* 
sweepy_barg ~number_of_rays:1 ~number_of_vols:1
;; 
*)


let parameterSampler 
(* tick = resolution, max tick = max value *)
~gridsteptick:gstick ~maxgridsteptick:maxgstick 
~rangeMultiplier:rmtick ~maxrangeMultiplier:maxrmtick 
=
assert (gstick >= 0.001);   (* minimum resolution of gridsteps tested *)
assert (rmtick >= 0.001);   (* minimum resolution of range tested *)
assert (gstick <= maxgstick); 
assert (maxgstick <= maxrmtick); (* should stay in range *)

let nbs = int_of_float (maxrmtick /. rmtick) in 
Array.init nbs 
(fun i -> 
  (
    (float_of_int (i + 1)) *. rmtick),
    let mbs = (min maxgstick maxrmtick) /. gstick in
    Array.init 
    (int_of_float mbs)
    (fun j -> (float_of_int (j + 1)) *. gstick)
)
;;

let prms = parameterSampler 
~gridsteptick:0.001 ~maxgridsteptick:0.1 
~rangeMultiplier:0.01 ~maxrangeMultiplier:0.5
;;
