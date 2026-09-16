(* Tentative de Résolution du Taquin ave A* *)

type taquin = { vide : int * int ; cases : int array array }

type deplacement = H | B | G | D

let taqWin = { vide = (0,0); cases = [| 
            [| 0; 1; 2; 3 |];
            [| 4; 5; 6; 7 |]; 
            [| 8; 9; 10; 11 |];
            [| 12; 13; 14; 15 |]; |]};;

(* taquins exemples *)

let taq1 = { vide = (0,1); cases = [|
          [| 1; 0; 2; 7 |];
          [| 4; 5; 3; 6 |];
          [| 8; 9; 10; 11 |];
          [| 12; 13; 14; 15 |]; |]};;

let taq2 = { vide = (1,1) ; cases = [|
          [| 4; 6; 1; 3 |];
          [| 8; 0; 2; 7 |];
          [| 10; 12; 14; 11 |];
          [| 9; 13; 5; 15 |]; |]};;

(* Fonctions utilitaires *)

let affiche_taquin taq =
  Printf.printf "-------------\n";
  for i = 0 to 3 do
    for j = 0 to 3 do
      let v = taq.cases.(i).(j) in
      if v = 0 then 
        Printf.printf "|  "
      else if  v < 10 then
        Printf.printf "| %d" v
      else
        Printf.printf "|%d" v
    done;
    Printf.printf "|\n"
  done;
  Printf.printf "-------------\n"
;;
(*
affiche_taquin taqWin;;
affiche_taquin taq1;;
affiche_taquin taq2;;
*)

(** Le jeu **)

(* Q1 *)
let coups_possibles (taq:taquin) : deplacement list =
  let n = Array.length taq.cases - 1 in
  (if fst taq.vide = 0 then [] else [H]) @
  (if fst taq.vide = n then [] else [B]) @
  (if snd taq.vide = 0 then [] else [G]) @
  (if snd taq.vide = n then [] else [D])
;;

(* Q2 *)
let joue (taq:taquin) (depl:deplacement) : taquin =
  if coups_possibles taq |> List.mem depl then begin
    let n = Array.length taq.cases in
    let cases = Array.make n [||] in
    for i = 0 to n - 1 do cases.(i) <- Array.copy taq.cases.(i) done;
    let i, j = taq.vide in
    let aux ib jb =
      cases.(i).(j) <- cases.(ib).(jb);
      cases.(ib).(jb) <- 0;
      {vide=(ib, jb); cases=cases}
    in
    match depl with
    | H -> aux (i-1) j
    | B -> aux (i+1) j
    | G -> aux i (j-1)
    | D -> aux i (j+1)
  end else taq
;;

(* Q3 *)
(*
joue taq1 B |> affiche_taquin ;;
joue taq1 G |> affiche_taquin ;;
joue taq1 D |> affiche_taquin ;;
joue taq2 H |> affiche_taquin ;;
joue taq2 B |> affiche_taquin ;;
joue taq2 G |> affiche_taquin ;;
joue taq2 D |> affiche_taquin ;;
*)

(* Q4 *)
let rec joue_partie (taq:taquin) (coups:deplacement list) : taquin =
  match coups with
  | [] -> taq
  | d::cc -> joue_partie (joue taq d) cc
;;


(** L' Heuristique **)

(* Q5 *)
let coord_cible : (int*int) array =
  let n = Array.length taqWin.cases in
  let c = Array.make (n * n) (n, n) in
  for i = 0 to n*n - 1 do c.(i) <- (i/n, i mod n) done; c
;;

(* Q6 *)
let heuristique (taq:taquin) : int =
  let s = ref 0 in
  let n = Array.length taq.cases in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let (x, y) = coord_cible.(taq.cases.(i).(j)) in
      s := !s + abs (x - i) + abs (y - j)
    done
  done; !s
;;

(* Q7 *)
Printf.printf "Heuristique de taq1: %d\n" (heuristique taq1) ;;
Printf.printf "Heuristique de taq2: %d\n" (heuristique taq2) ;;


(** La File de priorité : liste (element, prio) trié par priorité croissante **)

type 'a fileprio = { mutable liste : ('a * int) list };;

(* Q10 *)
let creeFP () : 'a fileprio = {liste=[]} ;;
let ajouterFP (fp:'a fileprio) (elt:'a) (prio:int) : unit =
  fp.liste <- List.sort (fun (_, a) (_, b) -> a - b) ((elt, prio)::fp.liste)
;;
let estVideFP (fp:'a fileprio) : bool = fp.liste = [] ;;
let extraireMinFP (fp:'a fileprio) : 'a =
  match fp.liste with
  | [] -> raise (Invalid_argument "Liste vide")
  | (elt, _)::ll -> fp.liste <- ll; elt
;;

(* Q11 *)
let testFP () =
  let fp = creeFP() in
  ajouterFP fp "test" 3;
  ajouterFP fp "ceci" 1;
  ajouterFP fp "est un" 2;
  while not (estVideFP fp) do
    let s = extraireMinFP fp in Printf.printf "%s " s
  done; print_newline ()
in testFP () ;;


(** Les associations : taquin ->  distance, deplacement **)

(* Q12 *)
let memoire = Hashtbl.create (-1) ;;

let dejavu = Hashtbl.mem memoire ;;

let recup_info = Hashtbl.find memoire ;;

let maj_info = Hashtbl.replace memoire ;;


(** Algo A* **)

exception Success ;;

(* Q13 *)
let explorer (taq:taquin) : unit =
  let fp = creeFP () in
  ajouterFP fp (taq, 0, None) (heuristique taq);
  while estVideFP fp |> not do
    let (taq, dist, depl) = extraireMinFP fp in
    if dejavu taq |> not || recup_info taq |> fst > dist then
      maj_info taq (dist, depl);
    if taq = taqWin then raise Success;
    let ajouter_voisin c =
      let voisin = joue taq c in
      ajouterFP fp (voisin, dist + 2, Some c) (heuristique voisin + dist + 2)
    in
    List.iter ajouter_voisin (coups_possibles taq)
  done
;;


(* Resolution *)

(* Q14 *)
let resoudre (taq:taquin) : unit =
  Hashtbl.reset memoire;
  try explorer taq
  with Success -> ();
  let rec chemin (taq:taquin) (coups:deplacement list) =
    let oppose (depl:deplacement) : deplacement =
      match depl with H -> B | B -> H | G -> D | D -> G
    in
    match recup_info taq with
    | _, None -> coups
    | _, Some c -> chemin (oppose c |> joue taq) (c::coups)
  in
  let partie = chemin taqWin [] in
  assert (joue_partie taq partie = taqWin);
  Printf.printf "[";
  List.iter (fun c -> Printf.printf "%c;" (match c with H->'H'|B->'B'|G->'G'|D->'D')) partie;
  print_endline "\b]"
;;


(* Q15 *)
resoudre taq1 ;;
resoudre taq2 ;;
