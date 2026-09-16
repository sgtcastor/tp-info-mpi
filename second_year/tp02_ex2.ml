(* TP 02 - Exercice 2 - Plus longue sous-séquence commune en OCaml *)

(* Détermine la longueur de la plus longue sous-séquence commune *)
let lplssc m1 m2 =
  let n1 = (Array.length m1) in
  let n2 = (Array.length m2) in
  let mat = Array.make_matrix (n1+1) (n2+1) 0 in
  for i = 1 to n1 do
    for j = 1 to n2 do
      mat.(i).(j) <- if m1.(i-1) = m2.(j-1)
        then mat.(i-1).(j-1) + 1
        else max mat.(i-1).(j) mat.(i).(j-1)
    done
  done ;
  mat.(n1).(n2)
;;


(* Détermine la (une des) plus longue sous-séquence commune *)
let plssc m1 m2 =
  let n1 = (Array.length m1) in
  let n2 = (Array.length m2) in
  let mat = Array.make_matrix (n1+1) (n2+1) (0, []) in
  for i = 1 to n1 do
    for j = 1 to n2 do
      mat.(i).(j) <- if m1.(i-1) = m2.(j-1)
        then let (x, l) = mat.(i-1).(j-1) in (x + 1, m1.(i-1) :: l)
        else
          let (x, l) = mat.(i-1).(j) in
          let (y, ll) = mat.(i).(j-1) in
          if x > y then (x, m2.(j) :: l) else (y, m1.(i) :: ll)
    done
  done ;
  let (x, l) = mat.(n1).(n2) in l
;;


let rec printl l =
  match l with
  |e::ll -> (Printf.printf "%d::" e; printl ll)
  |[] -> Printf.printf "[]"
;;

let m1 = [|1;7;4|];;
let m2 = [|3;1;4;3|];;
Printf.printf "lplssc %d\n" (lplssc m1 m2) ;;
(*Printf.printf "plssc " ;; printl (plssc m1 m2) ;; Printf.printf "\n" ;;*)


let m3 = [|2;4;1;2;4;4;2;1;2;7;2|];;
let m4 = [|1;2;4;6;4;5;2;4;1;7|];;
Printf.printf "lplssc %d\n" (lplssc m3 m4);;
(*Printf.printf "plssc " ;; printl (plssc m3 m4) ;; Printf.printf "\n" ;;*)
