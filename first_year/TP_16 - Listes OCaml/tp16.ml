let liste0 = [25; 0; -8; 6]


(* Q3 *)
let longueur l =
  let rec aux l acc =
    match l with
    |[] -> acc
    |x::ll -> aux ll (acc + 1)
  in aux l 0
;;
assert (longueur liste0 = 4)


(* Q4 *)
let somme l =
  let rec aux l acc =
    match l with
    |[] -> acc
    |x::ll -> aux ll (acc + x)
  in aux l 0
;;
assert (somme liste0 = 23)


(* Q5 *)
let rec appartient x l =
  match l with
  |[] -> false
  |y::ll -> if x = y then true else appartient x ll
;;
assert (appartient 25 liste0) ;;
assert (appartient 42 liste0 = false)


(* Q6 Bonus*)
let rec appartient_bis x l =
  match l with
  |[] -> false
  |y::ll -> x = y || appartient x ll
;;
assert (appartient_bis 25 liste0) ;;
assert (appartient_bis 42 liste0 = false)

(* Q7 *)
let rec double l =
  match l with
  |[] | _::[] -> false
  |x::y::ll -> x = y || double ll
;;
assert (double liste0 = false) ;;
assert (double [0; 1; 2; 2; 3])


(* Q9 *)
let rec append l1 l2 =
  match l1 with
  |[] -> l2
  |x::ll -> x::(append ll l2)
;;
assert (append [11; 4] [3; 5] = [11; 4; 3; 5])


(* Q10 *)
let rec exists f l =
  match l with
  |[] -> false
  |x::ll -> f x || exists f ll
;;


(* Q11 *)
let rec filter f l =
  match l with
  |[] -> []
  |x::ll -> let y = filter f ll in if f x then x::y else y
;;


(* Q12 *)
let rec map f l =
  match l with
  |[] -> []
  |x::ll -> (f x)::(map f ll)
;;


(* Q13 *)
let rec iter f l =
  match l with
  |[] -> ()
  |x::ll -> f x ; iter f ll
;;


let liste2 = [3; -4; -5; 1; 7; 12] ;;


(* Q14 *)
print_string "Q14: "
let affiche l =
  iter (function x -> print_int x ; print_string "::") l ;
  print_string "[]"
;;
affiche liste2 ;;
print_newline () ;;


(* Q15 *)
print_string "Q15: " ;;
print_string (string_of_bool (exists ((<) 10) liste2)) ;;
print_newline () ;;


(* Q16 *)
print_string "Q16: " ;;
let mod3 = filter (fun x -> x mod 3 = 0) liste2 ;;
affiche (mod3) ;;
print_newline () ;;


(* Q17 *)
map string_of_int mod3 ;;


(* Q18 *)
print_string "Q16: " ;;
affiche (map (fun x -> if x >= 0 then 99 else x) liste2) ;;
print_newline ()
