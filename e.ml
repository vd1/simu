#use "utils.ml";;

(* TODO:

* fix range, duration: map construire la correspondance entre vol et meilleur price_inc for fixed range
suffit-il de calculer le nb de up- et down-crossing?

- maybe needs to move to log returns log(p_{n+1}/p_{n}) instead of prices

* decoupler les deux lambda max et min 

* compilation ocaml

* add std to barg

* mutualiser les realisation de GBM quand on enumere -eg les gridstep

*)

(* --------------------- BROWNIANS --------------------- *)
let pi = 4.0 *. atan 1.0;;

(* box_muller: [0,1]^2 -> R G(box_muller)(U_2) = N(0,1) *)
(* grad box_muller (u1,u2) =  ...  *)
let box_muller u1 u2 =
  let r = sqrt (-2.0 *. log u1) in
  let theta = 2.0 *. pi *. u2 in
  r *. cos theta
;;

let normal_random () =
  box_muller (Random.float 1.0) (Random.float 1.0)
;;

(* |X| > 2 sigma with probability 9% *)
let xyz n = 
let res = ref 0 in
for i = 1 to n 
  do
    let v = abs_float (normal_random ()) in
      if ( v > 2.) 
      then incr res
  done;
(float_of_int !res) /. (float_of_int n)
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

(* generating a csv output from a list *)
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

(* 
OLD TESTS

for i = 1 to 4
do
  let d1v1 = bromo 0. 1. 1. 0.01 10. in 
  let filename = "d1v1"^(string_of_int i) in
  array2_to_csv filename d1v1
done
;;

for i = 1 to 4
do
  let d1v1 = gbrowmo 1. 1. (sqrt 2.) 0.01 10. in 
  let filename = "gd1vsq2"^(string_of_int i) in
  array2_to_csv filename d1v1
done
;; 
*)

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

let gen_log_returns 
(* outputs: array of pairs (time, log return) *)
(* number of simulation steps = duration/dt + 1 *)
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

(* given a price series creates a csv file with the log returns *)
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

(* given a (time,price) series generates the eta-viscous filter of the input *)
(* vf = viscous filter *)
(* eta is the fee *) 

(* --------------------- UNISWAP --------------------- *)
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
let ds = browmo ~initial_value:1. ~drift:0. ~volatility:0.1 ~timestep:0.01 ~duration:10. in
  array2_to_csv ~filename:"vs/vs0"~array2:ds;  (* let output,_,_ =  vf ~driver_series:ds ~viscosity:(0.1 /. (float_of_int ( 2* i))) in *)
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
0. pick GBM parameters 
initial value = 1, drift = 0, volatility in [0.055: 0.095], 
timestep = 0.001, duration = 1.; 
1. choose rangeMultiplier = 1.4 (symmetric range);
2. symmetric range implies  p_0 * initialBase =  initialQuote [equipartition]; hence for p_0=1, initialBase =  initialQuote
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
  let ds = gbrowmo ~initial_value:entryPrice ~drift:0. ~volatility:vol 
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
  let res = ref 0. in
  for i = 1 to n
    do 
     res := !res +. (unitFees ~viscosity:eta ~volatility:vol);
   done;
!res /. (float_of_int n)  
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
 

(* 
   mu' = 1 - 0.05^2/2 
   constant in log return increment = mu' * dt *)

(* --------------------- UTILS --------------------- *)
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
  let tx = truncate_float x 18 in
  padded_float tx pad
;;

(* padded rounded array *)
let pra pad = Array.iter 
  (fun x -> pad_trunc_float x pad;   print_string "| ")
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
;;  *)


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

let price_series  =  
(* h 6748 "Kandle_benchmark_data.csv"  *)
h ~number_of_lines:243780 ~filename:"csv/data_for_vd.csv" 
;;


(* 
let ps = Array.sub price_series 0 3;;
print_float ps.(0); print_newline ();
print_float ps.(1); print_newline ();
print_float ps.(999); print_newline ();
;; 
*)

(* I the index set for the various state elements, depends only on width and step *)
(* let build_index_set width step = 
  int_of_float (log(width) /. log(step))  
;; *)

(* 
log linear price subdivision 
we build a multiplicatively symmetric price grid centered on p0
NB: mid >= 1
NB: total number of points = index_set >= 3
NB: pmin >= p0 /. width and pmax <= p0 *. width, not necessarily equal because of rounding
pmin/max = p0 * (gridstep ** +/- mid)
*)

let generate_price_grid 
~half_number_of_price_points:mid 
~gridstep:gridstep 
~initial_price:p0 = 
  Array.init 
    (2 * mid + 1) 
    (fun i -> let expo = float_of_int (i - mid) in
              p0 *. gridstep ** (expo)) 
;;
(* NB: on voit que c'est tres uniforme en p0 ;
   OPTIM: qd on itere le long de la price_series
   en utilisant les mêmes paramètres,
   et donc en ne changeant que le mid-price  
   definir statiquement le price_grid_skeleton := le price_grid pour p_0=1
   et faire ensuite a chaque composition
   Array.map (fun x -> p0 * x) price_grid_skeleton
   ou même 
   let pg = price_grid_as_a_function new_starting_price in
   et ensuite on compose: sigma(p2, d, (sigma(p1, d, sig(p0, d, qB0));
   si on a aussi acces au current_volatility et la Garcheryy map step(current_vol)
   on peut aussi mettre à jour le step
*)

(* number of points for various values of gridstep *)
(* given by log(rangeMultiplier) /. log(gridstep);; *)

let aux ~range:lambda n = 
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

(* capital in A and B is distributed uniformly on both sides *)
(* we don't fill the central slot = hole *)  
let populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB 
~price_point_series:pg = 
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
  let res = ref 0. in 
  for i = 0 to (iib) 
    do
    res := !res +. price_grid.(i) *. bid.(i)
    done  
    ;
  !res 
;;

(* computing the amount of A in the book *)
let portfolio_A mid iia ask = 
  let res = ref 0. in 
  for i = (iia) to (2 * mid)
    do
    res := !res +. ask.(i)
    done  
    ;
  !res 
;;


(* let step =  1.1 in 
for i = 0 to 10
do
let x = geo (step ** (float_of_int i)) step in 
print_float(1_000_000_000_000. *. (x -. floor x));
  print_newline()
done
;; *)

(* no need to slice the price series as the price series array is read only *)
(* sim takes as input: 
1) strat parameters = price grid and capital in cash (quote, written B)
2) price series (delimited by start and duration)
outputs the return (the stochastic integral of strat against price)
and number of up- and down-crossings
*)
let sim 
(* the parameters chosen = shape of the investment decision *)
~tightness:width ~price_increment:gridstep ~cash:qB 
(* when we enter game and how long we play *)
(* NB: duration could be a stopping time in general, eg looking at a price-crossing event *)
~start:start ~duration:duration 
(* the future *)
~price_series:price_series (* we call h = input data in the main bundle loop ?*)
=
(* state: static part *)
let mid = int_of_float (log(width) /. log(gridstep)) in
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

(* starting a 50/50 position *)
let qB' = 0.5 *. qB in
let qA =  qB' /. p0 in
(* state: dynamic part *)
let ia, ib, ask, bid = 
populate_book 
~half_number_of_price_points:mid 
~baseAmount:qA 
~quoteAmount:qB' 
~price_point_series:price_grid
in

(* pra 12 bid; print_string "\n";
print_float (portfolio_B mid bid price_grid); print_newline();
pra 12 ask; print_string "\n";
print_float (p0 *. (portfolio_A mid (!ia) ask)); print_newline(); *)

(*  tracking up- and down-crossings *)
let rebu = ref 0 in
let rebd = ref 0 in

(* what we do when next price is higher than current *)
let upmove q  =
  while ((!ia < index_set) && (q >= price_grid.(!ia))) 
  (* 
  in the case of a "tubular fat price" we check that the price up-crosses "frankly"
  q/0.998 >= price_grid.(!ia)) 
  with fees 1bps on both local mkt and arb's source
  can also add gasCosts = 0.02 but then the constraint for profitability depends on Volum traded:
  V * (1 - mangroveFee) * (1 - uniFee) * p(Uni) > V * p(askKandle) + gasCosts
  *)
  (* "left strict AND" semantics matters in the test above: 
      because !ia can point 1+ higher than the highest possible ask
      when price has escaped up, in which case price_grid would return 
      an out_of_bonds error 
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
    bid.(iia - 1) <- ask.(iia) *. price_grid.(iia) /. price_grid.(iia - 1); 
    (* +. bid.(iia - 1); take this second term off if there is a hole *)
    ask.(iia) <- 0.; 
    ib := iia - 1; (* self filling if holed *)
    incr ia; 
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
    ask.(iib + 1) <- bid.(iib) +. ask.(iib + 1); 
    (* second term can/should be taken off if there is a hole *)
    bid.(iib) <- 0.;
    ia := iib + 1;
    decr ib; 
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
(* we pick up prices one at a time and update state *)

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
  print_string ("price_increment = ");
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
  (mtmf /. mtmi -. 1.), !rebu, !rebd
;;   

let bundle_sim ~start:start ~duration:duration ~repetitions:repetitions = 
let siim start duration = 
sim 
~tightness:(1.01 ** 20.000_000_1) 
~price_increment:1.01 
~cash:10_000.
~duration:duration
~price_series:price_series
~start:start 
in
let rest = Array.make repetitions (0., 0, 0) in 
(* here: how many times we launch a simulation *)
for i = 0 to (repetitions - 1)
  do
  (* fst arg of siim = where to start in the price series
     snd arg = how many prices to consume in this series *)
  rest.(i) <- siim (start + i * duration) duration
  done;
  Array.iter (fun (f,nb_upcrossing, nb_downcrossing) -> 
    print_float f; print_string ", "; 
    print_int nb_upcrossing; print_string ", ";
    print_int nb_downcrossing; print_string "\n") rest
;;

bundle_sim ~start:0 ~duration:40_000 ~repetitions:6
;;

(* resolution of gridsteps tested > 0 *)
(* gridstep = price increment = pic *)
(* inputs: gridstep sampling parameters, GBM parameters *)
(* iterating over gridsteps one with increment tick until maxpic *)
(* range is a range multiplier really *)
(* outputs: array of (gridstep, return, crossings)   *)
(* NB: range too wide -> reduction of capital -> loss of return *)

let gridsampling 
~pictick:tick ~maxpic:maxpic ~tightness:range
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration = 
  assert (tick >= 0.001);
  assert (maxpic >= tick);
  assert (maxpic <= range);
  let ps = 
    gen_price_series  
    ~initial_value:ival ~drift:drift ~volatility:vol 
    ~timestep:dt ~duration:duration in
  let _ = ps.(Array.length ps - 1) <- ps.(0) (* reset *) in
  let fnbs = maxpic /. tick in
  let nbs = (int_of_float fnbs)  - 1 in 
  let res = Array.make nbs (0., 0., 0) in
  for i = 1 to nbs 
      do
      let pic = (1. +. (float_of_int i) *. tick) in (* we choose a gridstep *)
      if (pic < range) 
        then
          (
          let r, u, d = 
          sim 
          ~tightness:range (* we fix the range and vary the price increment *)
          ~price_increment:pic
          ~cash:10_000.
          ~start:0
          ~duration:(Array.length ps) 
          (* ^^ number of price points in the time series, not to be confused with duration *)
          ~price_series:ps 
          in
          res.(i-1) <- (pic, r, u+d)
          (* print_float r; print_string ", ";
          print_int u; print_string ", ";
          print_int d; print_string "\n"; *)
          )
      done;
      res
;;

(* sample linear calculation of mean and return *)
let simple nb_repeats =
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

(* we repeat gridsampling number_of_rays time and collect mean and std *)
let barg 
~number_of_rays:number_of_rays 
~pictick:tick ~maxpic:maxpic ~tightness:range
~initial_value:ival ~drift:drift ~volatility:vol 
~timestep:dt ~duration:duration =
let size = int_of_float (maxpic /. tick) - 1 in
(* mean_return and std_return store sum and sumofsquares before normalising *)
let mean_return = Array.make size 0. in
let std_return = Array.make size 0. in
for i = 1 to number_of_rays
  do  
  let res = gridsampling 
  ~pictick:tick ~maxpic:maxpic ~tightness:range
  ~initial_value:ival ~drift:drift ~volatility:vol  
  ~timestep:dt ~duration:duration in 
  (Array.iteri (fun i (_,x,_)  -> (mean_return.(i) <- (mean_return.(i) +. x))) res);
  (Array.iteri (fun i (_,x,_)  -> (std_return.(i)  <- (std_return.(i)  +. x**2.0))) res);
  done;
  let n = (float_of_int number_of_rays) in
  Array.iteri 
    (fun i x -> (mean_return.(i) <- mean_return.(i) /. n)) 
    mean_return;
  Array.iteri 
    (fun i x -> (std_return.(i)  <-  sqrt (std_return.(i) /. n -. mean_return.(i)**2.0)))
    std_return;
  mean_return, std_return
;;

  (* Array.mapi 
      (
       fun i x  -> 
       (
        1. +. (float_of_int i) *. tick, 
        x /. (float_of_int number_of_rays)
       )
      )
mean_return *)
;;


(* stocker les 1000 trajectoires *)
(* impact of tightness/range? *)

let scan_vol n = 
for i = 1 to n
do  
let vol = 0.005 *. (float_of_int i) in
let volstring = string_of_float vol in 
let filename = "noil/meanret"^volstring in
let mr = barg 
~number_of_rays:1000 
~pictick:0.001 ~maxpic:0.2 ~tightness:1.4
~initial_value:1. ~drift:0. ~volatility:vol
~timestep:0.001 ~duration:1. in 
array2_to_csv ~filename:filename ~array2:mr
done
;;

let sweepy_barg ~number_of_rays:nr ~number_of_vols:nv =
for i = 1 to nv (* nv = 10 *)
  do
  let vol = 0.005 *. (float_of_int i) in
  let volstring = string_of_float vol in 
  let gm = barg ~number_of_rays:nr 
  ~pictick:0.001 ~maxpic:0.2 ~tightness:1.4
  ~initial_value:1. ~drift:0. ~volatility:vol 
  ~timestep:0.01 ~duration:1. in 
  let fileprefix = "gm/gm"^volstring in
  array2_to_csv ~filename:fileprefix ~array2:gm
  done
;;

(* 
sweepy_barg ~number_of_rays:1 ~number_of_vols:1
;; 
*)

let moving_average n arr =
  let len = Array.length arr in
  let result = Array.make len 0. in
  for i = 0 to len - 1 do
    let sum = ref 0. in
    let count = ref 0 in
    for j = max 0 (i-n+1) to i do
      sum := !sum +. snd arr.(j);
      incr count;
    done;
    result.(i) <- (fst arr.(i), !sum /. float_of_int !count);
  done;
  result
;;

(* 

let array3_to_csv filename res = 
  let file_string_out = "csv/"^filename^".csv" in
  let oc = open_out file_string_out in
  output_string oc  "# pic ; ret; crossings\n";
  Array.iter 
  (fun (price_inc, return, nb_of_crossings) -> 
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
   *)

(* est-ce que les returns sont indépendants de ...*)

(* 
caveat: dt << price_increment sinon on sous-estime les crossings? 
in fact, the jump distribution after dt rather than dt itself
*)

(* we fix width = 1.2 + duration = ? *)




