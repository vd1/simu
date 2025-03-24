(* --------------------- BROWNIANS --------------------- *)

let s  = Random.State.make_self_init ()
and s' = Random.State.make_self_init ()
;;

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
let mom_estimate ~nb_repeats:n ~rv:gen =
  let sum, sumofsquares, sumofcubes = ref 0., ref 0., ref 0. in
  for i = 1 to n
    do
    let rand = gen() in
    sum := rand +. !sum;
    sumofsquares := rand ** 2. +. !sumofsquares;
    sumofcubes := rand ** 3. +. !sumofcubes
    done
    ;
  let n = float_of_int n in
  let mean = !sum /. n in
  let var = !sumofsquares /. n -. mean ** 2. in
  let skew = !sumofcubes /. n -. 3. *. mean *. var -. mean ** 3. in
  mean, sqrt var, skew
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
  (* let ival2 = log(ival) /. vol in *)
  let drift2 = drift -. (vol ** 2.)/. 2.  in
  let arr2 = browmo 
  ~initial_value:0.0 ~drift:drift2 ~volatility:vol
  ~timestep:dt ~duration:duration in
  Array.map 
    (fun (t,v) -> (t,ival *. exp(v)))
    arr2
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

let res =  
  gen_price_series ~initial_value:1. ~drift:0. ~volatility:0.1 ~timestep:0.01 
  ~duration:1.0 in 
  print_int (Array.length res); 
  print_string "\n";
  pra 8 4 res;;
*)

(* 
   inputs: discrete GBM parameters
   outputs: array of pairs (time, log return) 
   number of steps of simulation = duration/dt + 1 
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

let infer_vol ~freq:dt ~price_series:ps = 

  let sum = ref 0. in
  let sumofsquares = ref 0. in

  let n = Array.length ps -1 in
  (* let log_ret = Array.make (l - 1) 1. in *)
  (* let log_ret = Array.make n 0. in *)
  
  for i = 1 to n
    do  
    (* log_ret.(i-1) <- log (ps.(i) /. ps.(i-1)); *)
    let x = log (ps.(i) /. ps.(i-1)) in
    sum := x +. !sum;
    sumofsquares := x ** 2. +. !sumofsquares
    done;  

  let nf = float_of_int n in
  let mean = !sum /. nf in
  let var = !sumofsquares /. nf -. mean ** 2. in
    (* (mean -. var /. 2. ) /. dt, *)
    sqrt (var /. dt)
  ;;

(* s = sig * sqrt(dt) *)

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
~initial_value: 1.0
~drift:1. ~volatility:0.05 ~duration:10. ~timestep:0.001 in
array2_to_csv ~filename:"tmp" ~array2:x
;; 
*)