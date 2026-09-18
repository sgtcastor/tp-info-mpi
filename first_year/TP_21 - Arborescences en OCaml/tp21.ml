
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
  | Vide -> -1
  | Noeud (n, g, d) -> 1 + max (hauteur g) (hauteur d)
;;
print_string "hauteur0: " ;; print_int (hauteur arbre0) ;; print_newline () ;;
print_string "hauteur1: " ;; print_int (hauteur arbre1) ;; print_newline () ;;
print_string "hauteur2: " ;; print_int (hauteur arbre2) ;; print_newline ()

(* Q12 *)
let rec genere_peigne_gauche h =
  if h = -1
  then
    Vide
  else
    Noeud (Random.int 10, genere_peigne_gauche (h - 1), Vide)
;;
let arbre3 = genere_peigne_gauche 6 ;;
print_arbre arbre3 ;;

(* Q13 *)
let rec genere_parfait h =
  if h = -1
  then
    Vide
  else
    Noeud (Random.int 10, genere_parfait (h - 1), genere_parfait (h - 1))
;;
let arbre4 = genere_parfait 3 ;;
print_arbre arbre4 ;;

(* Q14 *)
let rec genere_arbre n =
  if n = 0
  then
    Vide
  else
    let x = Random.int (n) in
    Noeud (Random.int 10, genere_arbre x, genere_arbre (n - x - 1))
;;
let arbre5 = genere_arbre 20 ;;
print_arbre arbre5 ;;

(* Q15 *)
let est_peigne arbre = taille arbre = hauteur arbre + 1 ;;
assert (est_peigne arbre3) ;;

(* Q16 *)
let rec est_parfait arbre = taille arbre = int_of_float (2. ** (float_of_int (hauteur arbre + 1))) - 1 ;;
assert (est_parfait arbre4) ;;

(** 4 Parcours d'arbres **)

(* Q17 *)
let rec parcours_prefixe f arb =
  match arb with
  | Vide -> ()
  | Noeud (x, g, d) -> f x ; parcours_prefixe f g ; parcours_prefixe f d
;;
parcours_prefixe (fun x -> print_int x ; print_char ' ') arbre5 ;;
print_newline ()

(* Q18 *)
let rec parcours_infixe f arb =
  match arb with
  | Vide -> ()
  | Noeud (x, g, d) -> parcours_infixe f g ; f x ; parcours_infixe f d
;;
parcours_infixe (fun x -> print_int x ; print_char ' ') arbre5 ;;
print_newline ()

let rec parcours_postfixe f arb =
  match arb with
  | Vide -> ()
  | Noeud (x, g, d) -> parcours_prefixe f g ; parcours_prefixe f d ; f x
;;
parcours_postfixe (fun x -> print_int x ; print_char ' ') arbre5 ;;
print_newline ()

(* Q19 *)
let parcours_largeur f arb =
  let queue = Queue.create () in
  Queue.push arb queue ;
  while (not (Queue.is_empty queue))
  do
    let elt = Queue.pop queue in
    match elt with
    | Vide -> ()
    | Noeud (x, g, d) -> f x ; Queue.push g queue ; Queue.push d queue
  done
;;
parcours_largeur (fun x -> print_int x ; print_char ' ') arbre5 ;;
print_newline ()

(** 5 Arbre n-aire **)

type 'a arbre_naire = V | N of 'a * 'a arbre_naire list

(* Q20 *)
let n_arbre0 = N ("gp", [N ("p1", [V]);
                         N ("p2", [N ("e2", [V])]);
                         N ("p3", [N ("e31", [V]);
                                   N ("e32", [V])
                                   ])
                         ]) ;;

(* Q21 *)
let rec taille_n arb =
  match arb with
  | V -> 0
  | N (x, l) -> 1 + taille_foret l
and taille_foret foret =
  match foret with
  | [] -> 0
  | x::l -> taille_n x + taille_foret l
;;
print_string "taille n_arbre0 : " ;;
print_int (taille_n n_arbre0) ;;
print_newline ()

(** 6 **)
(* ----------- Non fini
(* Bonus *)
let blockify arb =
  match arb with
  | V -> [|"V"|]
  | N (x, l) -> [|string_of_int x; |]
;;

let merge b1 b2 =
  let h = max (Array.length b1) (Array.length b2) in
  let ret = Array.make h "" in
  for i = 0 to h - 1
  do
    ret.(i) = b1.(i) ^ " " ^ b2.(i)
  done ;
  ret
;;
*)