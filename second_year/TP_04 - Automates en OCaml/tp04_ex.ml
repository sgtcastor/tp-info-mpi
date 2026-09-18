(* Programmer avec des exceptions *)

exception Success;;

let test f =
  assert(not(f [| 1 ; 0 ; 1 ; 3|])); (*tableau parcouru en entier *)
  assert(f [| -1 ; 0 ; 1 ; 3|]); (* seul le premier element est examiné *)
  assert(f [| 1 ; -1 ; 1 ; 3|]); (* seuls les deux premiers elements sont examinés *)
  assert(f [| 1 ; -1 ; 1 ; -3|]); (* seuls les deux premiers elements sont examinés *)
  assert(f [| 1 ; 0 ; 1 ; -3|]); (*tableau parcouru en entier *)
;;

let contientNeg tab =
  let n = Array.length tab in
  try
    for i = 0 to n-1 do
      if tab.(i) < 0 then raise Success
    done;
    false
  with
    Success -> true
;;

test contientNeg;;

let contientNegBoolFor tab =
  let n = Array.length tab in
  let success = ref false in
  for i = 0 to n-1 do
    success := tab.(i) < 0 || !success
  done;
  !success
;;

test contientNegBoolFor;;

let contientNegBoolWhile tab =
  let n = Array.length tab in
  let success = ref false in
  let i = ref 0 in
  while not !success && !i < n do
    success := tab.(!i) < 0 || !success;
  incr i
  done;
  !success
;;

test contientNegBoolWhile;;

let contientNegRec tab =
  let n = Array.length tab in
  let rec aux i =
    if i = n then false
    else
      if tab.(i) < 0 then true
      else aux (i+1)
  in
  aux 0
;;

test contientNegRec;;

let contientNegExists tab =
  Array.exists (fun x -> x < 0) tab
;;

test contientNegExists;;
