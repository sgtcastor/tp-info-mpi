(*** TP Automates ***)

(* on suppose l'alphabet a,b *)

type automaton = { (* n : le nombre d'états, 0 à n-1 *)
  initial : int; (* l'état initial *)
  final : bool array; (* tableau de n booléens *)
  transitions : int array array; (* tableau n lignes, 2 colonnes *)
  (* colonne 0 : transition à la lecture de 'a' *)
  (* colonne 1 : transition à la lecture de 'b' *)
}

(* Automate complet *)

let automate1 = { initial = 0; final = [|false;true|]; 
transitions = [| [|1;0|]; [|0;1|] |] };;

let lire0 aut mot =
  let etat = ref aut.initial in
  for i = 0 to (String.length mot - 1) do
    Printf.printf "(%d)--%c->" !etat mot.[i];
    etat := aut.transitions.(!etat).(int_of_char mot.[i] - int_of_char 'a')
  done;
  let resultat = aut.final.(!etat) in
  Printf.printf "(%d) %s\n" !etat (if resultat then "Reconnu" else "Rejeté");
  resultat
;;

Printf.printf "\n--- lire0 ---\n";;
assert(lire0 automate1 "baabba");;
assert(not (lire0 automate1 "abba"));;

(* Automates incomplets : on code l'absence de transition par -1 *)
let automate2 = {
  initial = 0; final = [|false; false; false; true|];
  transitions = [| [|1;-1|]; [|2;-1|]; [|3;-1|]; [|3;3|] |]
};;
(* automate2 reconnait les mots qui commencent par 3 'a' *)

let automate3 = {
  initial = 1; final = [|true; false; true|];
  transitions = [| [|0;-1|]; [|0;2|]; [|-1;2|] |]
};;
(* automate3 reconnait les mots qui ne contiennent que des 'a' ou que des 'b' *)


exception Blocked;; (* exception pour gérer le blocage de l'automate *)

let lire aut mot =
  let etat = ref aut.initial in
  try
    for i = 0 to (String.length mot - 1) do
      Printf.printf "(%d)--%c->" !etat mot.[i];
      etat := aut.transitions.(!etat).(int_of_char mot.[i] - int_of_char 'a');
      if !etat < 0 then raise Blocked
    done;
    let resultat = aut.final.(!etat) in
    Printf.printf "(%d) %s\n" !etat (if resultat then "Reconnu" else "Rejeté");
    resultat
  with
    Blocked -> Printf.printf " X Bloqué\n";
    false
;;

Printf.printf "\n--- lire ---\n";;

assert(lire automate1 "baabba");;
assert(not (lire automate1 "abba"));;

assert(not (lire automate2 "aa"));;
assert(not (lire automate2 "baaaa"));;
assert(lire automate2 "aaa");;
assert(lire automate2 "aaabaabba");;

assert(lire automate3 "aa");;
assert(not (lire automate3 "baaaa"));;
assert(lire automate3 "bbbb");;
assert(not (lire automate3 "bbaabba"));

(* Automate non déterministe *)

type automatonND = { (*n : le nombre d'états, numérotés de 0 à n-1 *)
  initial : int list; (* les états initiaux *)
  final : int list; (* les états finaux *)
  transitions : int list array array; (* n lignes, 2 colonnes *)
}

let automateND1 = {
  initial = [0]; final = [2]; 
  transitions = [|
    [| [0;1] ; [] |];
    [| [2] ; [0;2] |];
    [| [] ; [] |]
  |] 
};;

let automateND2 = {
 initial = [0]; final = [1];
 transitions = [|
   [| [0; 1]; [0] |];
   [| [0; 1]; [] |];
 |]
};;

type ens = int list;;

let rec unionLT e1 e2 =
  (* union de deux listes triees sans doublon *)
  match (e1,e2) with
  | (e::f, []) | ([], e::f) -> e::f
  | (x::f1, y::f2) ->
    if      x < y then x::(unionLT f1 e2)
    else if x > y then y::(unionLT e1 f2)
    else (* x = y *)   x::(unionLT f1 f2)
  | _ -> []
;;

let rec interLT e1 e2 =
  (* intersection de deux listes triees sans doublon *)
  match (e1, e2) with
  | (x::f1, y::f2) ->
    if      x < y then interLT f1 e2
    else if x > y then interLT e1 f2
    else (* x = y *)   x::(interLT f1 f2)
  | _ -> []
;;

let affiche_ens e =
  Printf.printf " |";
  match e with
  | [] -> ()
  | x::f -> Printf.printf "%d" x; List.iter (fun x->Printf.printf "-%d" x) f;
  Printf.printf "〉"
;;



let lireND aut mot =
  let etats = ref aut.initial in
  try
    for i = 0 to (String.length mot - 1) do
      affiche_ens !etats; Printf.printf "--%c->" mot.[i];
      let rec suivants e =
        match e with
        | [] -> []
        | x::f -> unionLT (aut.transitions.(x).(int_of_char mot.[i] - int_of_char 'a')) (suivants f)
      in
      etats := suivants !etats;
      if !etats = [] then raise Blocked
    done;
    affiche_ens !etats;
    let resultat = interLT !etats aut.final <> [] in
    Printf.printf (if resultat then " Reconnu\n" else " Rejeté\n");
    resultat
  with
    Blocked -> Printf.printf " |〉Bloqué\n";
    false
;;

Printf.printf "\n--- lireND ---\n";;
assert (lireND automateND1 "aaa");;
assert (not (lireND automateND1 "abbaa"));;
assert (lireND automateND2 "abbaa");;
assert (not (lireND automateND1 "bbaab"));;
assert (not (lireND automateND2 "bb"));;
