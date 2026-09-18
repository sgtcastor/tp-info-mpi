(*** TP : Algorithme de Berry-Sethi ***)

(* type pour les regexp et les regexp linéarisées *)
type regexp = Vide | Epsilon | C of char * int 
| Concat of regexp * regexp | Union of regexp * regexp | Etoile of regexp

(* type pour représenter l'automate final. *)
type autoND = { initial:int; final:int list; transitions:(int * char * int) list }


(* fonctions d'affichage *)

let rec afficheRegexp r = match r with
  | Vide -> print_string("vide")
  | Epsilon -> print_string("eps")
  | Concat (r1,r2) -> afficheRegexp r1; afficheRegexp r2
  | Etoile (r) -> print_string("("); afficheRegexp r; print_string(")*")
  | Union (r1,r2) -> print_string("("); afficheRegexp r1; 
    print_string("|"); afficheRegexp r2; print_string(")")
  | C (c,i) -> Printf.printf "%c%d" c i

let afficheAutomate aut =
  Printf.printf "initial: %d\n" aut.initial;
  print_string "final: ["; List.iter (fun q -> Printf.printf "%d, " q) aut.final; print_string "]\n";
  print_string "transitions: ["; List.iter (fun (q1,c,q2) -> Printf.printf "%d-%c-%d, " q1 c q2) aut.transitions; print_string "]\n"
;;


(* exemples *)

let r0 =  (*   a(ba)*c*   *)
  Concat (
    Concat (C('a', 0),
            Etoile (Concat (C('b',0), C('a',0)))),
    Etoile (C('c', 0))
  )
;;

let r1 =  (*   (ab)*ca(c|ba)*   *)
  Concat (
    Etoile (Concat (C('a',0), C('b',0))),
    Concat (Concat (C('c',0), C('a',0)),
            Etoile (Union (C('c', 0), Concat (C('b',0), C('a', 0))))))
;;


(* 1 - Calcul de P, S, F *)
Printf.printf "\n1 - Calcul de P, S, F\n" ;;

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | x :: ll1 ->
    union ll1 (if List.exists (fun y -> x = y) l2 then l2 else x::l2)
;;

let rec prod l1 l2 =
  match l1 with
  | [] -> []
  | x :: ll1 -> union (prod ll1 l2) (List.map (fun y -> (x, y)) l2)
;;

let rec contient_epsilon r =
  match r with
  | Vide -> false
  | Epsilon -> true
  | C (c, x) -> false
  | Union (r1, r2) -> contient_epsilon r1 || contient_epsilon r2
  | Etoile r -> true
  | Concat (r1, r2) -> contient_epsilon r1 && contient_epsilon r2
;;

let rec p r =
  match r with
  | Vide -> []
  | Epsilon -> []
  | C (c, x) -> [(c, x)]
  | Union (r1, r2) -> union (p r1) (p r2)
  | Etoile r -> p r
  | Concat (r1, r2) -> union (if contient_epsilon r1 then p r2 else []) (p r1)
;;

let rec s r =
  match r with
  | Vide -> []
  | Epsilon -> []
  | C (c, x) -> [(c, x)]
  | Union (r1, r2) -> union (s r1) (s r2)
  | Etoile r -> s r
  | Concat (r1, r2) -> union (if contient_epsilon r2 then s r1 else []) (s r2)
;;

let rec f r =
  match r with
  | Vide -> []
  | Epsilon -> []
  | C (c, x) -> []
  | Union (r1, r2) -> union (f r1) (f r2)
  | Etoile r -> union (f r) (prod (s r) (p r))
  | Concat (r1, r2) -> union (union (f r1) (f r2)) (prod (s r1) (p r2))
;;

(* tests *)
let afficheListe l = List.iter (fun (x, _) -> Printf.printf "%c " x) l; print_newline () ;;
afficheListe (p r0) ;;
afficheListe (p r1) ;;
afficheListe (s r0) ;;
afficheListe (s r1) ;;
List.map (fun ((x, _), (y, _)) -> Printf.printf "%c%c " x y) (f r0) ;; print_newline () ;;


(* 2 - Poursuivre l'implementation *)
Printf.printf "\n2 - Poursuivre l'implementation\n" ;;

let lineariser r =
  let i = ref 0 in
  let rec aux r0 =
    match r0 with
    | Vide | Epsilon -> r0
    | C (c, x) -> (incr i; C (c, !i))
    | Union (r1, r2) -> Union (aux r1, aux r2)
    | Etoile r1 -> Etoile (aux r1)
    | Concat (r1, r2) -> Concat (aux r1, aux r2)
  in
  aux r
;;

let automate r =
  let lr = lineariser r in
  let final =
    (if contient_epsilon r then [0] else []) @
    (List.map (fun (_, x) -> x) (s lr))
  in
  let transitions =
    (List.map (fun (c, x) -> (0, c, x)) (p lr)) @
    (List.map (fun ((_, x), (c, y)) -> (x, c, y)) (f lr)) in
  {initial = 0; final = final; transitions = transitions}
;;

(* tests *)
print_string "--- r0 ---\n" ;;
afficheRegexp r0 ;; print_string " --> " ;; afficheRegexp (lineariser r0) ;; print_newline () ;;
afficheAutomate (automate r0) ;;
print_string "--- r1 ---\n" ;;
afficheRegexp r1 ;; print_string " --> " ;; afficheRegexp (lineariser r1) ;; print_newline () ;;
afficheAutomate (automate r1) ;;
