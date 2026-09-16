(** TP  SAT FNC - MAXSAT **)

(* Type formule en FNC *)

type litt = Pos of int | Neg of int

type clause = litt list

type formule = clause list

(* Type valuation *)

type valuation = (int * bool) list

(* Affichage *)

let affiche_litt l = match l with
| Pos i -> Printf.printf "x%d" i
| Neg i -> Printf.printf "¬x%d" i

let rec affiche_clause cl = match cl with
 | [] -> ()
 | [litt] -> affiche_litt litt
 | litt :: ccl -> affiche_litt litt; Printf.printf " v "; affiche_clause ccl
;;

let rec affiche_formule f = match f with 
  | [] -> Printf.printf "\n"
  | [cl] ->  Printf.printf "["; affiche_clause cl; Printf.printf "]\n"
  | cl :: ff -> Printf.printf "["; affiche_clause cl; Printf.printf "] ^ "; affiche_formule ff
;;

let affiche_valuation v =
  Printf.printf "Valuation ";
  List.iter (fun (i,b) -> if b then Printf.printf "x%d:T " i else Printf.printf "x%d:F " i) v;
  Printf.printf " \n";;
  
(* Q1 Q2 *)

let f1 = [[Pos 0; Neg 2]; [Pos 1; Pos 2]; [Neg 0; Neg 1; Pos 2]] ;;
affiche_formule f1 ;;
let f2 = [[Pos 0; Pos 1]; [Neg 0; Pos 1]; [Pos 0; Neg 1]; [Neg 0; Neg 1]] ;;
affiche_formule f2 ;;

(* Q3 *)

(* evalue une formule *)
let rec evalue f v =
  let evalueL l =
    match l with
    | Pos x -> List.mem (x, true) v
    | Neg x -> List.mem (x, false) v
  in
  let rec evalueC cl =
    match cl with
    | [] -> false
    | litt::ccl -> evalueL litt || evalueC ccl
  in
  match f with
  | [] -> true
  | cl::ff -> evalueC cl && evalue ff v
;;

assert (evalue f1 [(0, true); (2, true)]) ;;
assert (evalue f2 [(0, true); (1, false)] = false) ;;

(* Q4 *)
(* renvoie la liste des variables présentes dans une formule (sans doublons) *)
let rec variables f =
  let rec union l1 l2 =
    match l1 with
    | [] -> l2
    | e::ll1 -> union ll1 (if List.mem e l2 then l2 else e::l2)
  in
  let rec variablesL l = match l with | Pos x | Neg x -> [x] in
  let rec variablesC cl =
    match cl with
    | [] -> []
    | litt::ccl -> union (variablesL litt) (variablesC ccl)
  in
  match f with
  | [] -> []
  | cl::ff -> union (variablesC cl) (variables ff)
;;

assert (variables f1 = [2; 1; 0]) ;;
assert (variables f2 = [1; 0]) ;;

(* Q5 *)
(* construit une valuation aletoire *)
Random.self_init () ;;
let rec valuation_alea lv = List.map (fun x -> (x, Random.int 2 = 1)) lv ;;

affiche_valuation (valuation_alea [0; 1; 2]) ;;

(* Q6 *)
let small_formula = [[Pos 0;Pos 7;Pos 6];[Neg 7;Neg 9];[Neg 1;Pos 2;Neg 4];[Pos 7;Neg 8];[Pos 8;Neg 4;Neg 1];[Pos 3;Neg 9;Pos 5];[Pos 9;Pos 5];[Neg 3;Neg 9;Neg 2];
                      [Neg 5;Pos 2;Neg 7];[Neg 7;Pos 8;Pos 5];[Neg 3;Pos 4;Neg 8];[Neg 5;Pos 0;Neg 2];[Neg 0;Neg 7;Pos 1];[Neg 6;Pos 3;Pos 7];[Neg 1;Neg 0;Pos 3]];;

let satLV f k  =
  let res = ref None in
  for i = 1 to k do
    if evalue f (f |> variables |> valuation_alea) then res := Some true
  done;
  !res
;;

(* tests *)
let affiche o =
  match o with
  | None -> Printf.printf "None\n"
  | Some x -> Printf.printf "Some %b\n" x
in
affiche (satLV f1 8);
affiche (satLV f2 4);
affiche (satLV small_formula 10);
affiche (satLV small_formula 32);
affiche (satLV small_formula 100) ;;


(* Q9 *)
(* compte le nombre de clauses satisfaites *)
let rec nb_clauses_ok f v = 
  let evalueL l =
    match l with
    | Pos x -> List.mem (x, true) v
    | Neg x -> List.mem (x, false) v
  in
  let rec evalueC cl =
    match cl with
    | [] -> false
    | litt::ccl -> evalueL litt || evalueC ccl
  in
  match f with
  | [] -> 0
  | cl::ff -> nb_clauses_ok ff v + if evalueC cl then 1 else 0
;;
  
  
(* Q10 *)
let maxSatBacktrack f =
  let maxi = ref 0 in (* nombre maximal de formules satisfaites *)
  let vmaxi = ref [] in (* valuation correspondante *)
  let rec retirer l x =
    match l with
    | [] -> raise Not_found
    | e::ll -> if e = x then ll else e::(retirer ll x)
  in
  let rec aux v reste =
    if reste = [] then begin
      let total = nb_clauses_ok f v in
      if total > !maxi then (maxi := total; vmaxi := v)
    end else begin
      let x = List.hd reste in
      aux ((x, true)::v) (retirer reste x);
      aux ((x, false)::v) (retirer reste x)
    end
  in
  aux [] (variables f);
  (!maxi, !vmaxi)
;;

let medium_formula = [[Pos 0;Pos 7;Pos 6];[Neg 7;Neg 9];[Neg 1;Pos 2;Neg 4];[Pos 7;Neg 8];[Pos 8;Neg 4;Neg 1];[Pos 3;Neg 9;Pos 5];[Pos 9;Pos 5];[Neg 3;Neg 9;Neg 2];
                      [Neg 5;Pos 2;Neg 7];[Neg 7;Pos 8;Pos 5];[Neg 3;Pos 4;Neg 8];[Neg 5;Pos 0;Neg 2];[Neg 0;Neg 7;Pos 1];[Neg 6;Pos 3;Pos 7];[Neg 1;Neg 0;Pos 3];
                      [Neg 4;Pos 2];[Neg 7;Neg 5];[Neg 1;Pos 4];[Neg 0;Pos 1];[Neg 3;Pos 1]];; (* 20 clauses *)

let big_formula = [[Neg 6;Pos 3;];[Neg 12;Pos 9;];[Neg 14;Neg 4;];[Pos 11;Neg 7;];[Neg 2;Pos 3;];[Pos 13;Neg 1;];[Neg 3;Neg 11;];[Neg 3;Pos 12;];
  [Neg 8;Pos 5;];[Pos 2;Neg 5;];[Pos 8;Neg 2;];[Neg 2;Pos 12;];[Neg 6;Pos 5;];[Neg 14;Neg 8;];[Neg 7;Pos 0;];[Pos 12;Neg 1;];[Pos 11;];[Pos 0;Neg 7;];
  [Neg 5;Pos 7;];[Pos 11;Pos 13;];[Neg 14;Pos 10;];[Neg 1;Neg 2;];[Neg 13;Neg 12;];[Neg 3;Neg 10;];[Neg 2;Pos 6;];[Pos 7;Pos 9;];[Pos 0;Neg 6;];
  [Neg 1;Neg 2;];[Pos 8;Pos 7;];[Neg 4;Pos 11;];[Neg 13;Pos 7;];[Neg 11;Pos 12;];[Neg 6;Neg 2;];[Pos 4;Pos 2;];[Pos 6;Pos 3;];[Neg 6;Neg 8;];
  [Neg 6;Neg 12;];[Pos 3;Neg 7;];[Neg 0;Pos 11;];[Neg 4;];[Pos 13;Pos 12;];[Pos 10;Pos 0;];[Neg 9;Pos 6;];[Pos 3;Pos 6;];[Pos 4;Pos 11;];
  [Neg 11;Pos 5;];[Neg 9;];[Pos 7;Neg 1;];[Neg 4;Neg 2;]];; (* 49 clauses *)

let very_big_formula = [[Neg 11; Neg 18; Neg 1]; [Neg 29; Neg 11; Pos 13]; (* 94 clauses *)
  [Pos 16; Pos 2; Neg 24]; [Pos 13; Neg 18];
  [Neg 6; Pos 17; Neg 23]; [Neg 0; Pos 10];
  [Neg 28; Pos 26; Pos 15]; [Pos 0; Neg 21; Pos 23];
  [Neg 1; Neg 23; Pos 11]; [Pos 2; Pos 19]; [Neg 2; Pos 25; Neg 19];
  [Pos 14; Neg 7; Pos 17]; [Pos 26; Neg 6]; [Neg 25; Pos 29; Pos 22];
  [Pos 8; Pos 5; Pos 4]; [Pos 6; Neg 11; Neg 10]; [Neg 9; Pos 22; Pos 24];
  [Neg 19; Neg 6; Neg 25]; [Neg 14; Neg 29; Neg 21];
  [Pos 25; Neg 28]; [Neg 12; Pos 17]; [Neg 14; Neg 23; Neg 19];
  [Pos 22; Neg 5; Pos 6]; [Neg 7; Pos 11; Neg 16]; [Pos 8; Neg 13; Pos 21];
  [Neg 26; Neg 21; Neg 11]; [Pos 24; Neg 6; Pos 0]; [Neg 17; Pos 1; Neg 28];
  [Neg 11; Pos 22; Pos 3]; [Neg 5; Pos 19]; [Neg 7; Pos 2; Neg 16];
  [Neg 6; Pos 17; Pos 16]; [Pos 24; Neg 5; Neg 14];
  [Neg 13; Neg 25; Neg 15]; [Neg 1; Neg 12; Pos 15]; [Neg 8; Pos 9];
  [Neg 21; Pos 14; Neg 6]; [Pos 21; Pos 16; Pos 2]; [Neg 9; Neg 28; Pos 23];
  [Neg 29; Pos 9; Pos 16]; [Neg 2; Neg 14]; [Neg 22; Neg 16; Neg 4];
  [Neg 19; Neg 17; Neg 24]; [Neg 4; Neg 1; Pos 25];[Pos 18; Neg 13];
  [Pos 16; Pos 23]; [Pos 26]; [Pos 23; Neg 17];
  [Pos 18; Neg 26]; [Pos 14; Neg 11]; [Pos 24; Neg 26]; [Pos 25; Neg 22];
  [Neg 0; Neg 26]; [Pos 27; Pos 0]; [Pos 25; Pos 23]; [Neg 16; Neg 19];
  [Pos 19; Pos 5]; [Neg 22; Neg 5]; [Pos 1; Pos 22]; [Neg 11; Neg 22];
  [Pos 19; Pos 10]; [Pos 25; Neg 29]; [Pos 4]; [Neg 10; Neg 19];
  [Neg 0; Neg 25]; [Pos 3; Neg 2]; [Pos 26; Pos 0]; [Neg 20; Neg 9];
  [Pos 1; Pos 17]; [Neg 12; Pos 24]; [Neg 28; Neg 4]; [Pos 4; Pos 25];
  [Neg 19; Neg 18]; [Pos 21; Neg 20]; [Neg 21; Neg 3]; [Pos 28; Pos 16];
  [Neg 8; Neg 7]; [Neg 13; Neg 9]; [Neg 26; Pos 29]; [Neg 15; Pos 25];
  [Neg 17; Neg 1]; [Pos 9; Neg 10]; [Neg 2; Pos 3]; [Neg 27; Pos 1];
  [Pos 0; Pos 26]; [Pos 23; Pos 26]; [Pos 3; Neg 20]; [Pos 22; Neg 8];
  [Neg 10; Neg 0]; [Neg 22; Neg 20]; [Neg 7; Neg 10]; [Neg 9; Pos 24];
  [Pos 19; Neg 22]; [Pos 24; Pos 6]]
;;

let (m, v) = maxSatBacktrack small_formula in
affiche_valuation v;
Printf.printf "%d\n" m ;;

let (m, v) = maxSatBacktrack medium_formula in
affiche_valuation v;
Printf.printf "%d\n" m ;;

let (m, v) = maxSatBacktrack big_formula in
affiche_valuation v;
Printf.printf "%d\n" m ;;

(* Q11 *)

let rec eval_p f v =
  let somme a b =
    let (u, v, w) = a in
    let (x, y, z) = b in
    (u + x, v + y, w + z)
  in
  let eval_p_L l =
    match l with
    | Pos x -> List.mem (x, true) v
    | Neg x -> List.mem (x, false) v
  in
  let rec eval_p_C cl =
    match cl with
    | [] -> (1, 0, 0)
    | litt::ccl ->
      match (eval_p_L litt, (eval_p_C ccl) = (1, 0, 0)) with
      | (true, true) -> (1, 0, 0)
      | (true, false) | (false, true) -> (0, 1, 0)
      | (false, false) -> (0, 0, 1)
  in
  match f with
  | [] -> (0, 0, 0)
  | cl::ff -> somme (eval_p_C cl) (eval_p ff)
;;

(* Q13 *)
let maxSatBnB f =
  failwith "maxSatBnB A FAIRE";
;;
