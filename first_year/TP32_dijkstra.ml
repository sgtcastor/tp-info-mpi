(* TP - Dijkstra *)

type sommet = int

type distance = int

type graphe = (sommet * distance) list array;;

(* Q1 *)

Printf.printf "\n--- Liste des villes ---\n" ;;

(* code exemple *)
let f = open_in "villes.txt" in
let n = int_of_string (input_line f) in
for i = 0 to (n-1) do
  Printf.printf "%s\n" (input_line f)
done; close_in f;;

let lire_villes nom_de_fichier =
  let f = open_in nom_de_fichier in
  let n = int_of_string (input_line f) in
  let villes = Array.make n "" in
  for i = 0 to (n-1) do
    villes.(i) <- input_line f
  done;
  close_in f;
  villes
;;

(* Q2 *)

let villes = lire_villes "villes.txt"

(* Q3 *)

let lireArete s = 
  try 
    Scanf.bscanf (Scanf.Scanning.from_string s) "%d %d %d" (fun x y d-> (x,y,d))
  with _ -> failwith ("ligne invalide : " ^ s)
;;

let lire_reseau nom_de_fichier =
  let f = open_in nom_de_fichier in
  let n_villes = int_of_string (input_line f) in
  let graphe = Array.make n_villes [] in
  let n_arretes = int_of_string (input_line f) in
  for i = 0 to n_arretes - 1 do
    let (x, y, d) = lireArete (input_line f) in
    graphe.(x) <- (y, d)::graphe.(x);
    graphe.(y) <- (x, d)::graphe.(y)
  done;
  close_in f;
  graphe
;;

(* Q4 *)

let reseau = lire_reseau "liaisons.txt"

(* Q5 *)

let afficheGraphe () =
  let rec afficheVoisins l =
    match l with
    | [] -> ()
    | (s, d)::ll ->
      Printf.printf "--> %s (%d min)\n" villes.(s) d;
      afficheVoisins ll
    in
  for i = 0 to Array.length villes - 1 do
    Printf.printf "%s\n" villes.(i);
    afficheVoisins reseau.(i)
  done
;;

Printf.printf "\n--- Graphe ---\n" ;;
afficheGraphe ()

(* Tas en OCaml *)

type 'a tas = {mutable n : int; mutable maxi : int; mutable data : 'a array; compare : 'a -> 'a -> int}

let nouveau_tas x comp = { n = 0; maxi = 16; data = Array.make 16 x; compare = comp};;

let est_vide tas = tas.n = 0;;

let echange tab i j = 
  let tmp = tab.(j) in
  tab.(j) <- tab.(i);
  tab.(i) <- tmp;;

let rec descendre tas i =
  if (2 * i + 1 < tas.n) then
    let f = ref (2 * i + 1) in (* indice du fils gauche *)
    if (2 * i + 2 < tas.n) && tas.compare tas.data.(2*i+2) tas.data.(2*i+1) < 0 then
      f := 2 * i + 2; (* indice du fils droit *)
    if tas.compare tas.data.(!f) tas.data.(i) < 0 then begin
      echange tas.data !f i;
      descendre tas !f;
    end;
;;

let retirer_min tas = 
  if tas.n = 0 then failwith "Tas vide" else
    let x = tas.data.(0) in
    tas.n <- tas.n - 1;
    tas.data.(0) <- tas.data.(tas.n);
    (* tamiser vers le bas *)
    descendre tas 0;
    x
  ;;


let rec remonter tas i =
  if i > 0 then
    let p = (i-1) / 2 in (* indice du pere *)
    if tas.compare tas.data.(i) tas.data.(p) < 0 then begin
      echange tas.data i p;
      remonter tas p;
    end
;;

let ajouter tas x = 
   if tas.n = tas.maxi then begin
      tas.maxi <- 2*tas.maxi;
      let data2 = Array.init tas.maxi (fun i -> if i < tas.n then tas.data.(i) else x) in
      tas.data <- data2;
   end;
   tas.data.(tas.n) <- x;
   tas.n <- tas.n + 1;
   (* tamiser vers le haut *)
   remonter tas (tas.n - 1)
;;

let comp (d1, s1) (d2, s2) = d1 - d2;;

let creer_tas () = nouveau_tas (0,0) comp;;

(* Q6 *)

Printf.printf "\n--- Test tas ---\n" ;;
let tas = creer_tas () in
for i = 0 to 9 do
  ajouter tas (Random.int 100, Random.int 100)
done;
for i = 0 to 9 do
  let (x, y) = retirer_min tas in
  Printf.printf "(%d, %d)\n" x y
done

(* Algo Dijkstra *)

(* Q7 *)

let dijkstra graphe s =
  let n = Array.length graphe in
  let dist = Array.make n Int.max_int in
  let closed = Array.make n false in
  let opened = creer_tas () in
  let rec traiter_voisins l d =
    match l with
    | [] -> ()
    | (ss, dd)::ll ->
      let new_d = d + dd in
      begin
        if (not closed.(ss)) && new_d < dist.(ss) then
          begin
            dist.(ss) <- new_d;
            ajouter opened (new_d, ss)
          end
        else ();
        traiter_voisins ll d
      end
  in
  dist.(s) <- 0;
  ajouter opened (0, s);
  while not (est_vide opened) do
    let (d, s) = retirer_min opened in
    if not closed.(s) then
      begin
        traiter_voisins graphe.(s) d;
        closed.(s) <- true
      end
    else ();
  done;
  dist
;;

Printf.printf "\n--- Dijkstra ---\n" ;;
let dist = dijkstra reseau 0 in
for i = 0 to Array.length villes -1 do
Printf.printf "%s --> %s\t: %03d min\n" villes.(0) villes.(i) dist.(i)
done

(* Q8 *)

let dijkstra2 graphe s =
  let n = Array.length graphe in
  let dist = Array.make n Int.max_int in
  let parents = Array.make n (-1) in
  let closed = Array.make n false in
  let opened = creer_tas () in
  let rec traiter_voisins s l d =
    match l with
    | [] -> ()
    | (ss, dd)::ll ->
      let new_d = d + dd in
      begin
        if (not closed.(ss)) && new_d < dist.(ss) then
          begin
            dist.(ss) <- new_d;
            parents.(ss) <- s;
            ajouter opened (new_d, ss)
          end
        else ();
        traiter_voisins s ll d
      end
  in
  dist.(s) <- 0;
  ajouter opened (0, s);
  while not (est_vide opened) do
    let (d, s) = retirer_min opened in
    if not closed.(s) then
      begin
        traiter_voisins s graphe.(s) d;
        closed.(s) <- true
      end
    else ();
  done;
  (dist, parents)
;;

(* Q9 *)

let plus_courts_chemins graphe s =
  let (dist, parents) = dijkstra2 graphe s in
  let rec afficheChemin l =
    match l with
    | [] -> ()
    | s::ll -> Printf.printf "%s " villes.(s); afficheChemin ll
  in
  let rec chemin dep arr l =
    if dep <> arr then
      chemin dep parents.(arr) (arr::l)
    else
      afficheChemin (dep::l)
  in
  Printf.printf "Aller de %s à :\n" villes.(s);
  for i = 0 to Array.length graphe -1 do
    Printf.printf "- %s (en %d min) : " villes.(i) dist.(i);
    chemin s i [];
    print_newline ()
  done
;;

Printf.printf "\n--- Plus courts chemins ---\n" ;;
plus_courts_chemins reseau 0
