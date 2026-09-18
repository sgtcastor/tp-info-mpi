let (%) b t = if b then Some t else None ;;
let ($) o f = if o = None then f else Option.get o ;;

exception Return of int ;;
let return x = raise (Return x) ;;

type 'a wrapper = Wrapped of ('a wrapper -> 'a) ;;
let recursive f =
  let unwrap (Wrapped x) = x in
  let fcopy s x = f (unwrap s s) x in
  let y f = fcopy (Wrapped fcopy) in
  y f
;;

(* Recursivite classique *)
let fact = recursive
    (fun fact n -> if n = 0 then 1 else n * fact (n-1))
in fact 6 ;;

(* Double appel recursif *)
let fibo = recursive
    (fun fibo n -> if n <= 2 then n else fibo (n-1) + fibo (n-2))
in fibo 6 ;;

(* Multiples arguments *)
let inserer = recursive
    (fun inserer l x ->
       match l with
       | [] -> [x]
       | e::ll -> (e < x) % (e::(inserer ll x)) $ (x::e::ll)
    )
in inserer [1; 2; 5] 3 ;; 

(* Return *)
let choose = recursive
    (fun choose n k -> try
        if k < 0 || n < k then return 0;
        if k = 0 || k = n then return 1;
        choose (n-1) (k-1) + choose (n-1) k
      with Return x -> x
    )
in choose 4 2 ;;

(*
  _/\_
  |^|>
(___)
 '-'-
*)
