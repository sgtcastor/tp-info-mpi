(* TP 13 - Arbres rouge noir *)

type couleur = R | B ;;
type arn = V | N of (couleur * arn * int * arn) ;;

let a0 = V;; (* ok *)
let a1 = N (R, N (B, V, 0, V), 3, N (B, V, 5, V));; (* non *)
let a2 = N (B, N (R, V, 0, V), 3, N (R, V, 5, V));; (* ok *)
let a3 = N (B, N (R, V, 0, V), 3, N (B, V, 5, V));; (* non *)
let a4 = N (B, N (B, V, 0, V), 3, N (B, V, 5, V));; (* ok *)
let a5 = N (B, N (B, V, 0, V), 3, N (R, N (B, V, 4, V), 5, V));; (* non *)
let a6 = N (B, N (B, V, 0, V), 3, N (R, N (B, V, 4, V), 5, N (B, V, 7, V)));; (* ok *)
let a7 = N (B, N (B, V, 0, V), 3, N (R, N (B, V, 4, V), 5, N (R, V, 7, N (B, V, 8, V) )));; (* non *)

(* Q14 la pire question (pour l'instant) *)

let valide a =
  let rec aux a =
    match a with
    | V -> (true, 0)
    | N (c, V, x, V) -> (true, if c = B then 1 else 0)
    | N (c, ag, x, V) ->
        let N (cg, _, g, _) = ag in 
        if c = R && cg = R then (false, 0)
        else
          let (b, h) = aux ag in
          (b && h = 0 && g < x, h + if c = B then 1 else 0)
    | N (c, V, x, ad) ->
        let N (cd, _, d, _) = ad in 
        if c = R && cd = R then (false, 0)
        else
          let (b, h) = aux ad in
          (b && h = 0 && x < d, h + if c = B then 1 else 0)
    | N (c, ag, x, ad) ->
        let N (cg, _, g, _) = ag in
        let N (cd, _, d, _) = ad in
        if c = R && cg = R || c = R && cd = R then (false, 0)
        else
          let (bg, hg) = aux ag in
          let (bd, hd) = aux ad in
          (bg && bd && hg = hd && g < x && x < d, hg + if c = B then 1 else 0)
  in
  match a with
  | V -> true
  | N (c, _, x, _) -> if c = R then false
      else let (b, _) = aux a in b
;; 


assert (valide a0);;
assert (not (valide a1));;
assert (valide a2);;
assert (not (valide a3));;
assert (valide a4);;
assert (not (valide a5));;
assert (valide a6);;
assert (not (valide a7));;


(* Q15 *)

let rec hauteur_noire a =
  match a with
  | V -> 0
  | N (c, ag, _, _) -> hauteur_noire ag + if c = B then 1 else 0
;;


let rec hauteur a =
  match a with
  | V -> 0
  | N (c, ag, _, ad) -> max (hauteur ag) (hauteur ad) + 1
;;


let rec rechercher a x =
  match a with
  | V -> false
  | N (_, ag, y, ad) ->
      if y < x then rechercher ad x else
      if x < y then rechercher ag x else
        true
;;


let correction_rr a =
  match a with
  | N (R, N (R, N (_, a, x, b), y, c), z, d)  (* x/y/z *)
  | N (R, N (R, a, x, N (_, b, y, c)), z, d)  (* x\y/[z] *)
  | N (R, a, x, N (R, N (_, b, y, c), z, d))  (* [x]\y/z *)
  | N (R, a, x, N (R, b, y, N (_, c, z, d)))  (* x\y\z *)
  -> N(R, N (B, a, x, b), y, N (B, c, z, d))
  | _ -> a
;;


let rec inserer_rec a x =
  match a with
  | V -> N (B, V, x, V)
  | N (c, V, y, V) -> if x <= y then
    N (c, N (R, V, x, V), y, V) else
    N (c, V, y, N (R, V, x, V))
  | N (c, ag, y, ad) -> if x <= y then
    correction_rr (N (c, inserer_rec ag x, y, ad)) else
    correction_rr (N (c, ag, y, inserer_rec ad x))
;;


let inserer a x =
  let aa = inserer_rec a x in
  match aa with
  | V -> assert false
  | N (_, ag, y, ad) -> N (B, ag, y, ad)
;;


let a = ref V in
for i = 0 to 1000 do a := inserer !a i done;
assert (valide !a);
Printf.printf "hn: %d\nh: %d\n" (hauteur_noire !a) (hauteur !a);;

(*
  _/\_
  |^|>
(___)
 '-'-
*)
