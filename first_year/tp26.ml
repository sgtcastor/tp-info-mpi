(* Logique en OCaml *)

(* Formules propositionnelles *)
type formule = Top | Bottom | Var of string 
| Et of formule * formule | Ou of formule * formule | Non of formule

(* Fonction d'affichage *)
let rec print_formule f = match f with
| Top -> print_string "T"
| Bottom -> print_string "B"
| Var s -> print_string s
| Non f0 -> print_string "NON("; print_formule f0; print_string ")"
| Et (f1, f2) -> print_string "("; print_formule f1; print_string " ET "; print_formule f2; print_string ")"
| Ou (f1, f2) -> print_string "("; print_formule f1; print_string " OU "; print_formule f2; print_string ")";;

let formule0 = Ou (
        Et (Ou (Var "g", Var "d"), Non (Var "d")),
        Et (Non (Ou (Var "g", Var "d")), Var "d"));;

print_endline "-- Formules --";;
print_formule formule0; print_newline ();;

(* Q1 *)
let impl f1 f2 = Ou (Non f1, f2) ;;

(* Q2 *)
let equiv f1 f2 = Et (impl f1 f2, impl f2 f1 );;

(* Q3 *)
let formule1 = equiv (impl (Var "a") (Var "b"))
                     (impl (Var "b") (Var "a")) ;;

(* Q4 *)
let formule2 = equiv (impl      (Var "a")       (Var "b"))
                     (impl (Non (Var "b")) (Non (Var "a"))) ;;

print_formule formule1; print_newline ();;
print_formule formule2; print_newline ();;

(* Q5 *)
let rec fusion l1 l2 = 
  (* fusionne deux listes sans doublons *)
  match l1 with
  | [] -> l2
  | e::ll -> if List.mem e l2 then fusion ll l2 else fusion ll (e::l2)
;;

(* Q6 *)
let rec vars_of_formule f =
  (* renvoie la liste des variables de la formule (sans doublon) *)
  match f with
  | Var v -> [v]
  | Non f -> vars_of_formule f
  | Et (f1, f2) | Ou (f1, f2) -> fusion (vars_of_formule f1) (vars_of_formule f2)
  | _ -> []
;;

assert (vars_of_formule formule0 = ["g"; "d"]);;
assert (vars_of_formule formule1 = ["a"; "b"]);;
assert (vars_of_formule formule2 = ["a"; "b"]);;

(* Q7 *)
let rec assoc l x =
  match l with
  | [] -> failwith "cle absente"
  | (k, v)::ll -> if k = x then v else assoc ll x
;;

(* Q8 *)
let rec evalue f l =
  match f with
  | Top -> true
  | Bottom -> false
  | Var x -> assoc l x
  | Non b -> not (evalue b l)
  | Ou (b1, b2) -> evalue b1 l || evalue b2 l
  | Et (b1, b2) -> evalue b1 l && evalue b2 l
;;

let valuation01 = [("g", true);("d", false)];;
let valuation02 = [("g", true);("d", true)];;
let valuation03 = [("a", true);("b", false)];;

assert (evalue formule0 valuation01);;
assert (not (evalue formule0 valuation02));;
assert (not (evalue formule1 valuation03));;
assert (evalue formule2 valuation03);;

(* algorithme de Quine *)

(* Q9 *)
let simplifie f =
  match f with
  | Non Top -> Bottom
  | Non Bottom -> Top
  | Et (Bottom, _) | Et (_, Bottom) -> Bottom
  | Ou (Top, _) | Ou (_, Top) -> Top
  | Et (Top, b) | Et (b, Top) | Ou (Bottom, b) | Ou (b, Bottom) -> b
  | _ -> f
;;

(* Q10 *)
let rec substitue f x g =
  match f with
  | Var v -> if v = x then g else f
  | Non ff -> simplifie (Non (substitue ff x g))
  | Ou (f1, f2) -> simplifie (Ou (substitue f1 x g, substitue f2 x g))
  | Et (f1, f2) -> simplifie (Et (substitue f1 x g, substitue f2 x g))
  | _ -> f
;;

print_endline "-- Substitution --";;
print_formule (substitue formule0 "g" Top); print_newline();;
print_formule (substitue formule0 "g" Bottom); print_newline();;

(* Q11 - Q12 *)
let print_valuation vl assign = 
  (* on affiche les variables assignees *)
  List.iter (fun (x,b) -> print_string x; print_string " := "; if b then print_string "T | " else print_string "F | ") assign;
  (* on affiche les variables non assignees *)
  List.iter (fun x -> print_string x; print_string ":?-" ) vl; print_newline ()
;;

let satisfiable f =
  let rec quine f vl assign =
    (* renvoie un booleen indiquant si la formule est satisfiable, 
    f formule, 
    vl liste des variables non assignees,
    assign liste des couples (variable, bool) *)
    match vl with
    | [] -> if evalue f assign then (print_valuation vl assign ; true) else false
    | v::ll -> quine (substitue f v Top) ll ((v, true)::assign) || quine (substitue f v Bottom) ll ((v, false)::assign)
  in
  quine f (vars_of_formule f) []
;;

(* Q13 *)
let antilogie f = not (satisfiable f) ;;

print_endline "-- Satisfiable --";;
assert (satisfiable formule0);;
assert (satisfiable formule1);;
assert (satisfiable formule2);;


(* Exercice - Reglement du club *)
let reglement = Et (Et (Et (Et (Et (impl (Non (Var "écossais")) (Var "chaussures oranges"),
                                    Ou (Var "jupe", Non (Var "chaussures oranges"))),
                                impl (Var "marié") (Non (Var "sort le dimanche"))),
                            equiv (Var "sort le dimanche") (Var "écossais")),
                        impl (Var "jupe") (Et (Var "écossais", Var "marié"))),
                    impl (Var "écossais") (Var "jupe"))
;;

print_endline "-- Antilogie --" ;;
print_string (if antilogie reglement then "Le règlement est contradictoire" else "Le règlement est cohérent") ;;
print_newline ()

(* Exercice - Bonus *)
let tautologie f = antilogie (Non f) ;;

assert (tautologie Top) ;;
assert (tautologie (Ou (Ou (Var "a", Var "b"), Et (Non (Var "a"), Non (Var "b"))))) ;;  (* a | b | (-a & -b) *)
assert (tautologie formule2)

let satisfiable2 f =
  let rec quine f vl assign =
    (* Algorithme de Quine modifié *)
    (* On effectue d'abord les deux substitutions, puis on renvoie le résulat.
      Ainsi, l'algorithme ne "saute" pas de possibilités à cause d'un opérateur parresseux. *)
    match vl with
    | [] -> if evalue f assign then (print_valuation vl assign ; true) else false
    | v::ll ->
      let t = quine (substitue f v Top) ll ((v, true)::assign) in
      let f = quine (substitue f v Bottom) ll ((v, false)::assign) in
      t || f
  in
  quine f (vars_of_formule f) []
;;

print_endline "-- Satisfiable 2 --" ;;
print_endline " Formule 0:" ;;
assert (satisfiable2 formule0) ;;
print_endline " Formule 1:" ;;
assert (satisfiable2 formule1) ;;
print_endline " Formule 2:" ;;
assert (satisfiable2 formule2)
