(* TP09 branch-and-bound *)

(* Avec 10 villes : 9! = 362880 circuits possibles à partir de Limoges *)
let villes = [|"Limoges"; "Tulle"; "Périgueux"; "Toulouse"; "Bordeaux"; 
"Brive"; "Guéret"; "Niort"; "Rochefort"; "Bergerac"|];;

let distances = [|
  [|  0;  90; 100; 290; 220;  90;  90; 230; 220; 200|]; (* distances depuis Limoges *)
  [| 90;   0; 120; 245; 245;  25; 175; 250; 360; 155|]; (* distances depuis Tulle *)
  [|100; 120;   0; 275; 140;  75; 185; 200; 250;  50|]; (* distances depuis Périgueux *)
  [|290; 245; 275;   0; 245; 205; 375; 425; 395; 210|]; (* distances depuis Toulouse *)
  [|220; 245; 140; 245;   0; 205; 300; 190; 165; 120|]; (* distances depuis Bordeaux *)
  [| 90;  25;  75; 205; 205;   0; 180; 255; 320; 130|]; (* distances depuis Brive *)
  [| 90; 175; 185; 375; 300; 180;   0; 230; 290; 300|]; (* distances depuis Gueret *)
  [|230; 250; 200; 425; 190; 255; 230;   0;  60; 260|]; (* distances depuis Niort *)
  [|220; 360; 250; 395; 165; 320; 290;  60;   0; 230|]; (* distances depuis Rochefort *)
  [|200; 155;  50; 210; 120; 130; 300; 260; 230;   0|]; (* distances depuis Bergerac *)
|];;

(* Avec 14 villes : 13! = 6 milliards de circuits possibles à partir de Limoges *)

let villes = [|"Limoges"; "Tulle"; "Périgueux"; "Toulouse"; "Bordeaux"; 
               "Brive"; "Guéret"; "Niort"; "Rochefort"; "Bergerac"; "Agen"; 
			   "Arras"; "Cahors"; "Villefranche de Rouergue"|];;

let distances = [|
  [|  0;  90;  100; 290; 220;  90;  90; 230; 220; 200; 320; 570; 190; 225|]; (* distances depuis Limoges *)
  [| 90;   0;  120; 245; 245;  25; 175; 250; 360; 155; 270; 650; 130; 165|]; (* distances depuis Tulle *)
  [|100; 120;    0; 275; 140;  75; 185; 200; 250;  50; 140; 660; 175; 210|]; (* distances depuis Périgueux *)
  [|290; 245;  275;   0; 245; 205; 375; 425; 395; 210; 115; 850; 110; 130|]; (* distances depuis Toulouse *)
  [|220; 245;  140; 245;   0; 205; 300; 190; 165; 120; 140; 760; 233; 290|]; (* distances depuis Bordeaux *)
  [| 90;  25;   75; 205; 205;   0; 180; 255; 320; 130; 240; 655; 100; 140|]; (* distances depuis Brive *)
  [| 90; 175;  185; 375; 300; 180;   0; 230; 290; 300; 410; 565; 270; 310|]; (* distances depuis Gueret *)
  [|230; 250;  200; 425; 190; 255; 230;   0;  60; 260; 315; 590; 350; 470|]; (* distances depuis Niort *)
  [|220; 360;  250; 395; 165; 320; 290;  60;   0; 230; 290; 655; 385; 440|]; (* distances depuis Rochefort *)
  [|200; 155;   50; 210; 120; 130; 300; 260; 230;   0;  90; 775; 110; 175|]; (* distances depuis Bergerac *)
  [|320; 270;  140; 115; 140; 240; 410; 315; 290;  90;   0; 895;  85; 165|]; (* distances depuis Agen *)
  [|570; 650;  660; 850; 760; 655; 565; 590; 655; 775; 895;   0; 750; 785|]; (* distances depuis Arras *)
  [|190; 130;  175; 110; 233; 100; 270; 350; 385; 110;  85; 750;   0;  60|]; (* distances depuis Cahors *)
  [|225; 165;  210; 130; 290; 140; 310; 470; 440; 175; 165; 785;  60;   0|]; (* distances depuis Villefranche de Rouergue*)
|];;


(* Fonction d'affichage *)

let affiche_trajet parcours =
  let lv = List.map (fun i -> villes.(i)) parcours in
  List.iter (fun v-> print_string "->"; print_string v) lv; print_newline ();;

(* Exemple d'affichage *)
(*
affiche_trajet [0;1;2;3;0];;
*)
(* Fonctions utilitaires *)

let rec supprime l x = 
  (* supprime une occurrence de l'élément x de la liste l *)
  match l with
  | [] -> raise Not_found
  | e::ll -> if e = x then ll else e::(supprime ll x)
;;

assert (supprime [2;3;5] 3 = [2;5]);;

let rec distance par = 
  (* calcule la longueur totale du parcours 
    : somme des distances entre deux villes successives de la liste *)
  match par with
  | a::b::next -> distances.(a).(b) + distance (b::next)
  | _ -> 0
;;

assert (distance [3;2;1;4] = 640);;

(* Backtracking classique *)

let meilleur_parcours = ref [];;
let meilleure_distance = ref max_int;;
let appels = ref 0;;

let rec backtrack par reste =
  (* par : parcours courant (en ordre inverse),
     reste : villes restant à visiter *)
  incr appels;
  if reste <> [] then
    List.iter (fun x -> backtrack (x::par) (supprime reste x)) reste
  else
    let d = distance (0::par) in
    if d < !meilleure_distance then
    begin
      meilleure_distance := d;
      meilleur_parcours := 0::par
    end
;;

backtrack [0] [1;2;3;4;5;6;7;8;9];;

Printf.printf "Meilleure solution : %d\n" !meilleure_distance;;
affiche_trajet !meilleur_parcours;;
Printf.printf "Nombre d'appels : %d\n" !appels


(* Backtracking b&b 1 *)

let meilleur_parcours = ref [];;
let meilleure_distance = ref max_int;;
let appels = ref 0;;

type 'a wrapper = Wrapped of ('a wrapper -> 'a) ;;
let recursive f =
  let unwrap (Wrapped x) = x in
  let fcopy s x = f (unwrap s s) x in
  let y f = fcopy (Wrapped fcopy) in
  y f
;;

let backtrack_bnb_1 = recursive (fun backtrack_bnb_1 d par reste ->
  incr appels;
  let head = List.hd par in
  if reste <> [] then
    let aux x =
      if d + distances.(x).(0) < !meilleure_distance then
        backtrack_bnb_1 (d + distances.(head).(x)) (x::par) (supprime reste x)
    in
    List.iter aux reste
  else
    if d + distances.(head).(0) < !meilleure_distance then
    begin
      meilleure_distance := d + distances.(List.nth par 0).(0);
      meilleur_parcours := 0::par
    end
) 0
;;

backtrack_bnb_1 [0] [1;2;3;4;5;6;7;8;9];;

Printf.printf "Meilleure solution B&B 1 : %d\n" !meilleure_distance;;
affiche_trajet !meilleur_parcours;;
Printf.printf "Nombre d'appels : %d\n" !appels;;

(* Backtracking b&b 2 *)

let meilleur_parcours = ref [];;
let meilleure_distance = ref max_int;;
let appels = ref 0;;

let backtrack_bnb_2 = recursive (fun backtrack_bnb_2 d par reste ->
  incr appels;
  let head = List.hd par in
  if reste <> [] then
    let aux x =
      let maxi = List.fold_left  (*dist max disponible*)
        (fun x y -> if distances.(head).(x) > distances.(head).(y) then x else y) 0 reste
      in
      let nd = d + distances.(head).(x) in
      if nd + distances.(maxi).(0) < !meilleure_distance then
        backtrack_bnb_2 nd (x::par) (supprime reste x)
    in
    List.iter aux (List.sort (fun x y -> distances.(head).(x) - distances.(head).(y)) reste)
  else
    if d + distances.(head).(0) < !meilleure_distance then
    begin
      meilleure_distance := d + distances.(List.nth par 0).(0);
      meilleur_parcours := 0::par
    end
) 0
;;

backtrack_bnb_2 [0] [1;2;3;4;5;6;7;8;9];;

Printf.printf "Meilleure solution B&B 2 : %d\n" !meilleure_distance;;
affiche_trajet !meilleur_parcours;;
Printf.printf "Nombre d'appels : %d\n" !appels;;

meilleur_parcours := [];;
meilleure_distance := max_int;;
appels := 0;;

backtrack_bnb_2 [0] [1;2;3;4;5;6;7;8;9;10;11;12];;

Printf.printf "Meilleure solution B&B 2 (12 villes) : %d\n" !meilleure_distance;;
affiche_trajet !meilleur_parcours;;
Printf.printf "Nombre d'appels : %d\n" !appels;;
