(* TP03 - Exercice 2 *)

type regex = Vide | Eps | C of char | Sigma
| Concat of regex * regex
| Union of regex * regex
| Etoile of regex

(* teste si une regexp matche sur une chaine de caractères *)
let rec matche reg str = 
  let n = String.length str in
  match reg with
  | Vide -> false
  | Eps -> n = 0
  | C c -> n = 1 && str.[0] = c
  | Sigma -> n = 1
  | Union (reg_a, reg_b) -> matche reg_a str || matche reg_b str
  | Concat (reg_a, reg_b) ->
    let matche_i i = matche reg_a (String.sub str 0 i) && matche reg_b (String.sub str i (n-i)) in
    let range = List.init (n+1) (fun i -> i) in
    List.exists matche_i range
  | Etoile r -> n = 0 ||
    let matche_i i = matche r (String.sub str 0 (i+1)) && matche reg (String.sub str (i+1) (n-i-1)) in
    let range = List.init (n) (fun i -> i) in
    List.exists matche_i range
;;


print_endline "Debut des tests";;
(* la regexp vide *)
assert (not (matche Vide "hello"));;
assert (not (matche Vide ""));;
(* la regexp epsilon *)
assert (not (matche Eps "hello"));;
assert (matche Eps "");;
(* regexp à une lettre *)
assert (not (matche (C 'a') "h"));;
assert (not (matche (C 'a') ""));;
assert (matche (C 'z') "z");;
assert (not (matche (C 'z') "zz"));;
assert (not (matche (C 'z') "az"));;
(* regexp sigma *)
assert (not (matche Sigma "ah"));;
assert (not (matche Sigma ""));;
assert (matche Sigma "z");;
assert (matche Sigma "a");;
(* union *)
assert (matche (Union (Eps, C 'h')) "h");;
assert (matche (Union (Eps, C 'h')) "");;
assert (matche (Union (Vide, C 'h')) "h");;
assert (matche (Union (C 'a', C 'b')) "a");;
assert (matche (Union (C 'a', C 'b')) "b");;
assert (not (matche (Union (Vide, C 'h')) ""));;
assert (not (matche (Union (Eps, C 'h')) "a"));;
(* concat *)
assert (matche (Concat (Eps, C 'h')) "h");;
assert (matche (Concat (C 'h', C 'e')) "he");;
assert (not (matche (Concat (C 'h', C 'e')) "h"));;
assert (not (matche (Concat (C 'h', C 'e')) "ae"));;
assert (not (matche (Concat (C 'h', C 'e')) "hee"));;
(* etoile *)
assert (matche (Etoile (C 'e')) "");;
assert (matche (Etoile (C 'e')) "e");;
assert (matche (Etoile (C 'e')) "eeeee");;
assert (not (matche (Etoile (C 'e')) "eeeeef"));;
assert (not (matche (Etoile (C 'e')) "eefee"));;
assert (matche (Etoile (Etoile (C 'e'))) "");;
assert (matche (Etoile (Etoile (C 'e'))) "eeeee");;
assert (not (matche (Etoile (Etoile (C 'e'))) "eeeeef"));;

(* la regexp (a.a|b.b)  *)
let reg0 = Union (Concat (C 'a', Concat (Sigma, C 'a')),
                  Concat (C 'b', Concat (Sigma, C 'b')));;
assert (matche reg0 "aaa");;
assert (matche reg0 "ara");;
assert (matche reg0 "bib");;
assert (matche reg0 "bob");;
assert (not (matche reg0 "aab"));;
assert (not (matche reg0 "abba"));;
assert (not (matche reg0 "a"));;

(* la regexp (aa|b)*  *)
let reg1 = Etoile (Union (Concat (C 'a', C 'a'), C 'b'));;
assert (matche reg1 "");;
assert (matche reg1 "aa");;
assert (matche reg1 "bbbb");;
assert (matche reg1 "aaaa");;
assert (matche reg1 "baabbbaaaa");;
assert (matche reg1 "aabaabbb");;
assert (not (matche reg1 "a"));;
assert (not (matche reg1 "aba"));;
assert (not (matche reg1 "aabaaa"));;

(* la regexp a*ab  *)
let reg2 = Concat (Concat (Etoile (C 'a'), C 'a'), C 'b');;
assert (matche reg2 "ab");;
assert (matche reg2 "aab");;
assert (matche reg2 "aaaaab");;
assert (not (matche reg2 "b"));;
assert (not (matche reg2 "aba"));;
assert (not (matche reg2 "aaaa"));;

print_endline "Fin des tests";;

type eregex = Vide | Eps | C of char | Sigma
| Concat of eregex * eregex
| Union of eregex * eregex
| Etoile of eregex
| Plus of eregex
| Question of eregex
| Puissance of eregex * int

let rec convert ereg =
  match ereg with
  | Plus e -> let r = convert e in Concat (r, (Etoile r))
  | Question e -> Union (Eps, (convert e))
  | Puissance (e, x) ->
    if x = 0 then Eps
    else Union ((convert e), (convert (Puissance (e, x-1))))
  | _ -> ereg
;;
