(* TP07 - Ricochet Robots - partie 2 *)

let obs_l = [| [|0; 4; 10; 16|]; [|0; 14; 16|]; [|0; 6; 16|];
   [|0; 9; 16|]; [|0; 3; 15; 16|]; [|0; 7; 16|]; [|0; 1; 12; 16|]; [|0; 7; 9; 16|];
   [|0; 7; 9; 16|]; [|0; 4; 13; 16|]; [|0; 6; 16|]; [|0; 10; 16|]; [|0; 8; 16|];
   [|0; 2; 15; 16|]; [|0; 4; 10; 16|]; [|0; 5; 12; 16|] |];;

let obs_c = [| [|0; 5; 11; 16|]; [|0; 6; 13; 16|]; [|0; 4; 16|];
   [|0; 15; 16|]; [|0; 10; 16|]; [|0; 3; 16|]; [|0; 10; 16|]; [|0; 6; 7; 9; 12; 16|];
   [|0; 7; 9; 16|]; [|0; 3; 12; 16|]; [|0; 14; 16|]; [|0; 16|]; [|0; 7; 16|];
   [|0; 2; 10; 16|]; [|0; 4; 13; 16|]; [|0; 2; 12; 16|] |];;

(* Q1 *)
let dichotomie a t =
  let i = ref 0 in (* inclus *)
  let j = ref (Array.length t) in (* exclu *)
  while (!i + !j) / 2 > !i do
    if a < t.((!i + !j) / 2) then
      j := (!i + !j) / 2
    else i := (!i + !j) / 2
  done;
  !i
;;

(* Tests dichotomie *)
Printf.printf "Test dichotomie : %d\n " (dichotomie 3 [|0;1;5;15;21;30|]);;
assert (dichotomie 3 [|0;1;5;15;21;30|] = 1);;
assert (dichotomie 5 [|0;1;5;15;21;30|] = 2);;
assert (dichotomie 29 [|0;1;5;15;21;30|] = 4);;

(* Q2 *)
let deplacements_grille (a,b) =
  let i = dichotomie a obs_c.(b) in  (* vertical *)
  let j = dichotomie b obs_l.(a) in  (* horizontal *)
  [|(a, obs_l.(a).(j)); (a, obs_l.(a).(j+1)-1);
    (obs_c.(b).(i), b); (obs_c.(b).(i+1)-1, b)|]
;;

(* affichage d'un tableau de positions *)
let affichePos tab =
Array.iter (fun (a,b)->Printf.printf "(%d,%d)" a b) tab; print_newline ();;

Printf.printf "Test deplacement solo \n";;
affichePos (deplacements_grille (0,0));;
assert (deplacements_grille (0,0) = [|(0,0);(0,3);(0,0);(4,0)|]);;
assert (deplacements_grille (2,7) = [|(2,6);(2,15);(0,7);(5,7)|]);;
assert (deplacements_grille (9,7) = [|(9,4);(9,12);(9,7);(11,7)|]);;

(* Q4 *)
let matrice_deplacements () =
  let m = Array.make_matrix 16 16 (Array.make 4 (0, 0)) in
  for i = 0 to 15 do for j = 0 to 15 do
    m.(i).(j) <- deplacements_grille (i, j)
  done done;
  m
;;

  (* Q6 *)
let mat_depl = matrice_deplacements ();;

let modif t (a,b) (c,d) =
  if a = c then
    begin
      let (i, j) = t.(0) in
      if j <= d && d < b then t.(0) <- (i, d+1);
      let (i, j) = t.(1) in
      if b < d && d <= j then t.(1) <- (i, d-1)
    end;
  if b = d then
    begin
      let (i, j) = t.(2) in
      if i <= c && c < a then t.(2) <- (c+1, j);
      let (i, j) = t.(3) in
      if a < c && c <= i then t.(3) <- (c-1, j)
    end
;;

Printf.printf "Test deplacement vs. 1 robot :\n";;
let t0 = deplacements_grille (2,7) in
modif t0 (2,7) (2,12);
affichePos t0;
assert (t0 = [|(2,6);(2,11);(0,7);(5,7)|]);;


(* Q8 *)
let deplacements_robots (a,b) q =
  let d = mat_depl.(a).(b) in
  List.iter (fun x -> modif d (a, b) x) q;
  d
;;

Printf.printf "Test deplacement vs. plusieurs robots :\n";;
let t1 = deplacements_robots (2,7) [(2,6); (4,7); (2,15); (2,10)] in
affichePos t1;
assert (t1 = [|(2,7);(2,9);(0,7);(3,7)|]);;

(* Q10 *)

(* affichage d'un tableau de positions *)
let direction_of_int x =
  match x with
  | 0 -> "ouest"
  | 1 -> "est"
  | 2 -> "nord"
  | 3 -> "sud"
  | _ -> assert false
;;

let afficheSol lst =
  List.iter (fun (a,b)->Printf.printf "robot %d vers %s\n" a (direction_of_int b)) lst
;;

let rec resol_backtracking deplacements robots p =
  if p <= 0 then
    if robots.(0) = (14, 3) then afficheSol deplacements
    else ()
  else
  for i = 0 to Array.length robots - 1 do  (* robots *)
    let bt = robots.(i) in
    let d = deplacements_robots robots.(i) (Array.fold_right (fun x y -> x::y) robots []) in
    for j = 0 to 3 do  (* directions *)
      robots.(i) <- d.(j);
      resol_backtracking ((i, j)::deplacements) robots (p-1)
    done;
    robots.(i) <- bt
  done
;;

Printf.printf "configuration facile:\n";
resol_backtracking [] [|(5,12);(4,0);(3,1);(0,7)|] 6;;

Printf.printf "configuration initiale:\n";
resol_backtracking [] [|(5,12);(0,0);(2,1);(4,7);|] 8;;
