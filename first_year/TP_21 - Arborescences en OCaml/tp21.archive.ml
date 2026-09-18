(* Arbres en OCaml - Exercice 3 *)

type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre

(* Fonction d'affichage - ne pas modifier *)

let rec repete n c = if n <= 0 then "" else c ^ (repete (n-1) c);;

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
  | Vide -> (1, [|"V"|])
  | Noeud (v, Vide, Vide) -> let s = string_of_int v in (String.length s, [|s|])
  | Noeud (v, ag, ad) -> 
    fusionne v (block_of_arbre ag) (block_of_arbre ad);;

let print_arbre arbre = 
  let (largeur, block) = block_of_arbre arbre in
  for i = 0 to Array.length block - 1 do
    print_endline block.(i)
  done;;

(* A vous de jouer ! *)

(* Q10 *)
let arbre0 = Vide ;;
let arbre1 = Noeud (42, Noeud (15, Vide, Vide), Vide) ;;
let arbre2 = Noeud (0, Noeud (10, Vide, Noeud (210, Vide, Vide)), Noeud (20, Vide, Vide)) ;;
print_arbre arbre0 ;;
print_arbre arbre1 ;;
print_arbre arbre2

(* Q11 *)
let rec taille arbre =
  match arbre with
  | Vide -> 0
  | Noeud (n, g, d) -> 1 + taille g + taille d
;;
print_string "taille0: " ;; print_int (taille arbre0) ;; print_newline () ;;
print_string "taille1: " ;; print_int (taille arbre1) ;; print_newline () ;;
print_string "taille2: " ;; print_int (taille arbre2) ;; print_newline () ;;

let rec hauteur arbre =
  match arbre with
  | Vide -> 0
  | Noeud (n, g, d) -> let hg = (hauteur g) in let hd = (hauteur d) in 1 + if hg < hd then hd else hg
;;
print_string "hauteur0: " ;; print_int (hauteur arbre0) ;; print_newline () ;;
print_string "hauteur1: " ;; print_int (hauteur arbre1) ;; print_newline () ;;
print_string "hauteur2: " ;; print_int (hauteur arbre2) ;; print_newline ()