(* TP : Arbre binaire de recherche *)

type 'a abr = V | N of 'a abr * 'a * 'a abr


(* Fonctions utilitaires pour l'affichage cf TD14 : joli affichage *)
let rec repete n c = if n <= 0 then "" else c ^ (repete (n-1) c)

(* cree un bloc correspondant Ã  une valeur ainsi que les deux blocs des sous-arbres *)
let fusionne v (l1, b1) (l2, b2) =
  let nb_lignes = max (Array.length b1) (Array.length b2) in
  let block = Array.make (nb_lignes + 2) "" in
    let val_string = string_of_int v in
    let entete = repete ((l1+1) / 2) " " ^ repete ((l1+l2+2) / 4) "_" ^ val_string ^ repete ((l1+l2+2) / 4) "_" ^ repete ((l2+1) / 2) " " in
    let largeur = String.length entete in
    block.(0) <- entete; 
    block.(1) <- repete ((l1+1) / 2) " " ^ "|" ^ repete (largeur - (l1+1) / 2 - (l2+1) / 2 - 2) " " ^ "|" ^ repete ((l2+1) / 2) " ";
    assert (String.length block.(0) = String.length block.(1));
    for i = 0 to nb_lignes - 1 do
      if (i < Array.length b1 && i < Array.length b2) then
        block.(i+2) <- b1.(i) ^ repete (largeur - l1 - l2) " " ^ b2.(i)
      else if i < Array.length b1 then
        block.(i+2) <- b1.(i) ^ repete (largeur - l1) " "
      else if i < Array.length b2 then
        block.(i+2) <- repete (largeur - l2) " " ^ b2.(i)
    done;
    (largeur, block)


let rec block_of_arbre arbre = match arbre with
  | V -> (1, [|"V"|])
  | N (V, v, V) -> let s = string_of_int v in (String.length s, [|s|])
  (*| N (a, v, V) | N (V, v, a) -> etend v (block_of_arbre a)*)
  | N (ag, v, ad) -> 
    fusionne v (block_of_arbre ag) (block_of_arbre ad);;

let print_abr arbre = 
  let (largeur, block) = block_of_arbre arbre in
  for i = 0 to Array.length block - 1 do
    print_endline block.(i)
  done;;

(*** A vous de jouer ! ***)

(* Q1 *)
let arbre0 = 
  N(
    N (V, 5, N (V, 7, V)),
    8,
    N (N (V, 10, V), 11, V));;

print_abr arbre0;;
    
let arbre1 = 
  N(
    N (N (V, 4, V), 5, N (V, 9, V)),
    8,
    N (N (V, 10, V), 11, N (V, 12, V)));;

print_abr arbre1;;

let arbre2 = 
  N(
    N (V, 4, N (V, 8, V)),
    9,
    N (V, 10, V));;

print_abr arbre2;;


(* Q2 *)
let rec min_max a =
  match a with
  | V -> failwith "arbre vide"
  | N (V, e, V) -> (e, e)
  | N (V, e, aa) | N (aa, e, V) ->
    let (mini, maxi) = min_max aa in
    (min e mini, max e maxi)
  | N (ag, e, ad) ->
    let (minig, maxig) = min_max ag in
    let (minid, maxid) = min_max ad in
    (min e (min minig minid), max e (max maxig maxid))
;;

(* Q3 *)
let rec est_abr a =
  match a with
  | V -> true
  | N (ag, e, ad) ->
    let (_, maxig) = min_max ag in
    let (minid, _) = min_max ad in
    maxig < e && e <= minid && est_abr ag && est_abr ad
;;

(* Q4 *)
let rec est_present a x =
  match a with
  | V -> false
  | N (ag, e, ad) -> e = x || est_present ag x || est_present ad x
;;

(* Q5 *)
let rec ajouter a x =
  match a with
  | V -> N (V, x, V)
  | N (ag, e, ad) -> if x < e then N((ajouter ag x), e, ad) else N(ag, e, (ajouter ad x))
;;

(* Q7 *)
let rec suppr_mini a =
  match a with
  | V -> failwith "arbre vide"
  | N (V, e, V) -> (e, V)
  | N (V, e, ad) ->
    let (mini_d, suppr_d) = suppr_mini ad in
    (e, N (V, mini_d, suppr_d))
  | N (ag, e, ad) ->
    let (mini_g, suppr_g) = suppr_mini ag in
    (mini_g, N (suppr_g, e, ad))
;;

let rec supprimer a x =
  match a with
  | V -> V
  | N (ag, e, V) ->
    if e = x then ag
    else supprimer ag x
  | N (ag, e, ad) ->
    if e = x then
      let (mini_d, suppr_d) = suppr_mini ad in
      N (ag, mini_d, suppr_d)
    else if x < e then N (supprimer ag x, e, ad)
    else N(ag, e, supprimer ad x)
;;

(* Q8 *)
print_string "supprimer arbre0 7\n" ;
print_abr (supprimer arbre0 7) ;
print_string "supprimer arbre0 5\n" ;
print_abr (supprimer arbre0 5) ;
print_string "supprimer arbre0 8\n" ;
print_abr (supprimer arbre0 8) ;;

(* Q9 *)
let un = ref 50 ;;
let arbre_u = ref (N (V, !un, V)) ;;
for i = 1 to 500 do
  un := (11 * !un + 517) mod 1000 ;
  arbre_u := ajouter !arbre_u !un
done ;
for i = 500 to 700 do
  un := (11 * !un + 517) mod 1000 ;
  arbre_u := supprimer !arbre_u !un
done ;;


(* Q10 *)
let rec recherche_par_intervalle a mini maxi =
  match a with
  | V -> []
  | N (ag, e, ad) ->
    (recherche_par_intervalle ag mini maxi) @
    (if mini < e && e < maxi then [e] else []) @
    (recherche_par_intervalle ad mini maxi)
;;

(* Q11 *)
let rec print_liste l =
  match l with
  | [] -> print_string "[]"
  | e :: ll -> print_string ((string_of_int e) ^ "::") ; print_liste ll
;;

print_string "Valeurs dans arbre_u comprises entre 50 et 100:\n" ;
print_liste (recherche_par_intervalle !arbre_u 50 100)


(* Q12 *)  (*     NON FINI     *)
let tri_avec_abr l =
  let rec gene_abr l =
    match l with
    | [] -> V
    | e :: ll -> ajouter (gene_abr ll) e
  in
  let ll = ref [] in
  let abr = ref (gene_abr l) in
  while !abr <> V do
    (_, maxi) = min_max !abr ;  (* fix *)
    abr := supprimer !abr maxi ;
    ll := maxi::!ll
  done ;
  !ll
;;
