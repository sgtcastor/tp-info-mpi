(** TP table de hachage **)

(* Découpage d'une chaîne en liste de mots *)

let split str =
  let isalpha c = 
    ((Char.code c >= 97) && (Char.code c <= 122)) ||
    ((Char.code c >= 65) && (Char.code c <= 90)) 
  in
  let rec split_aux i n = 
    if i = n then []
    else begin
      if not (isalpha str.[i]) then
        split_aux (i+1) n
      else
        let len = ref 1 in
        while (i + !len < n) && isalpha str.[i + !len] do
          len := !len + 1
        done;
        String.sub str i !len :: (split_aux (i + !len) n)
    end
  in
  split_aux 0 (String.length str);;
  
assert (split "L'informatique, c'est fantastique !" = ["L";"informatique";"c";"est";"fantastique"]);;


(* Construction de la liste des mots d'un fichier *)

let recup_mots nom_de_fichier =
  (* lit un fichier et renvoie la table de hachage des compteurs de chaque mot *)
  let f = open_in  nom_de_fichier in 
  let mots = ref [] in
  begin
    try 
      while true do
        mots := (split (input_line f)) @ !mots
      done
    with 
      End_of_file -> ()
  end; 
  close_in f;
  !mots;;

(* Construction du dictionnaire de comptage *)
let construit_dico_comptage mots =
  let table = Hashtbl.create 1024 in
  let rec aux l =
    match l with
    | [] -> ()
    | e::ll ->
      if Hashtbl.mem table e then
        Hashtbl.replace table e (Hashtbl.find table e + 1)
      else
        Hashtbl.add table e 0;
      aux ll
  in
  aux mots;
  table
;;

let dico_comptage = construit_dico_comptage (recup_mots "mobydick.txt");;

(* Q13 *)
Printf.printf "Nombre de mots différents : %d\n" (Hashtbl.length dico_comptage);;

(* Q14 *)
Printf.printf "Mots aparaissant plus de 3000 fois\n";;
let f key value =
  if value > 3000 then
    Printf.printf " %s : %d\n" key value
in
Hashtbl.iter f dico_comptage;;

(* Q15 *)
Printf.printf "Mots aparaissant exactement 77 fois\n";;
let f key value =
  if value = 77 then
    Printf.printf " %s : %d\n" key value
in
Hashtbl.iter f dico_comptage;;

(* Q16 *)
let total = ref 0 in
let f key value =
  if value = 1 then
    total := !total + 1
in
Hashtbl.iter f dico_comptage;
Printf.printf "Nombre de mots n'aparaissant qu'une seule fois : %d\n" !total;;
