(* KNN version simple *)

(* run:
ocaml -I ~/.opam/default/lib/graphics graphics.cma knn.ml
*)

let nb_cl = 3;;  (* nombre de classes ; de 0 à nb_cl - 1 *)

let data = [|  (* ensemble d'entrainement *)
([| 149. ; 304. |], 0);
([| 207. ; 346. |], 0);
([| 79. ; 155. |], 0);
([| 257. ; 287. |], 0);
([| 499. ; 86. |], 0);
([| 432. ; 309. |], 0);
([| 498. ; 343. |], 0);
([| 356. ; 178. |], 0);
([| 401. ; 224. |], 0);
([| 75. ; 119. |], 0);
([| 334. ; 338. |], 0);
([| 374. ; 242. |], 0);
([| 608. ; 378. |], 1);
([| 310. ; 496. |], 1);
([| 610. ; 485. |], 1);
([| 544. ; 298. |], 1);
([| 550. ; 483. |], 1);
([| 670. ; 227. |], 1);
([| 719. ; 237. |], 1);
([| 673. ; 386. |], 1);
([| 319. ; 359. |], 1);
([| 560. ; 252. |], 1);
([| 395. ; 250. |], 1);
([| 299. ; 529. |], 1);
([| 449. ; 533. |], 1);
([| 463. ; 387. |], 1);
([| 265. ; 324. |], 1);
([| 716. ; 429. |], 1);
([| 481. ; 371. |], 1);
([| 502. ; 280. |], 1);
([| 428. ; 623. |], 1);
([| 355. ; 424. |], 1);
([| 796. ; 301. |], 2);
([| 938. ; 430. |], 2);
([| 507. ; 320. |], 2);
([| 519. ; 336. |], 2);
([| 573. ; 269. |], 2);
([| 557. ; 198. |], 2);
([| 761. ; 285. |], 2);
([| 829. ; 434. |], 2);
([| 522. ; 182. |], 2);
([| 766. ; 105. |], 2);
([| 803. ; 124. |], 2);
([| 884. ; 420. |], 2);
([| 769. ; 235. |], 2);
([| 912. ; 151. |], 2);
([| 833. ; 387. |], 2);
|];;

(* distance entre deux points *)
let dist pt1 pt2 = ((pt1.(0) -. pt2.(0))**2. +. (pt1.(1) -. pt2.(1))**2.)**0.5;;

(* classifieur knn *)
let classifie k data pt =
  let classes = [|0; 0; 0|] in
  let ldist = Array.map (fun (pt2, c) -> (dist pt pt2, c)) data in
  Array.sort (fun (d1, _) (d2, _) -> int_of_float (d1 -. d2)) ldist;
  for i = 0 to k-1 do let c = snd ldist.(i) in classes.(c) <- classes.(c) + 1 done;
  if classes.(0) > classes.(1) && classes.(0) > classes.(2) then 0 else
  if classes.(1) > classes.(0) && classes.(1) > classes.(2) then 1 else
  if classes.(2) > classes.(0) && classes.(2) > classes.(1) then 2 else
  -1
;;


(* Q3 *)
Printf.printf "       k=\t1\t2\t3\t4\t5\n";
Printf.printf "(500,345)\t";
for k = 1 to 5 do Printf.printf "%d\t" (classifie k data [|500.; 345.|]) done;
print_newline ();
Printf.printf "(550,300)\t";
for k = 1 to 5 do Printf.printf "%d\t" (classifie k data [|550.; 300.|]) done;
print_newline (); print_newline ();;


(* couleurs associées aux classes / zones*)
let cpb = Graphics.rgb 0 0 0;;
let cz0 = Graphics.rgb 150 250 150;;
let cp0 = Graphics.rgb 35 250 35;;
let cz1 = Graphics.rgb 250 150 150;;
let cp1 = Graphics.rgb 250 35 35;;
let cz2 = Graphics.rgb 150 150 250;;
let cp2 = Graphics.rgb 35 35 250;;

let set_color_of_classe cl z =
  let couleur = 
    match cl with
    | 0 -> if z then cz0 else cp0
    | 1 -> if z then cz1 else cp1
    | 2 -> if z then cz2 else cp2
    | _ -> Graphics.white
  in  
  Graphics.set_color couleur;;

(* affichage d'un ensemble d'entraînement *)
let affichage_points data =
  Graphics.set_line_width 3;
  for i = 0 to Array.length data - 1  do
    let coord, cl = data.(i) in
    set_color_of_classe cl false;
    Graphics.draw_rect (int_of_float coord.(0)-1) (int_of_float coord.(1)-1) 3 3
  done;
  Graphics.set_color cpb;
  Graphics.draw_rect 499 344 3 3;
  Graphics.draw_rect 549 299 3 3
;;


(* Q4 *)
let classifie_zone k data =
  Graphics.set_line_width 3;
  for x = 0 to 100 do
    let x = 10 * x in
    for y = 0 to 70 do
      let y = 10 * y in
      set_color_of_classe (classifie k data [|float_of_int x; float_of_int y|]) true;
      Graphics.draw_rect (x-1) (y-1) 3 3
    done
  done
;;


(* Affichage *)

Graphics.open_graph " 1000x700" ; (* ouverture de la fenetre graphique *)

(* Q5 *)
(* classifie_zone 3 data;; *)
affichage_points data;;

Graphics.read_key ();; (* attente d'une saisie clavier *)


(* Q6 *) type k_meilleurs = (float array * float * int) array;;

(* Q7 *)
let creerKM k : k_meilleurs = Array.make k ([||], infinity, -1);;

let ajouterKM (km:k_meilleurs) ((pt, dist, cl):(float array * float * int)) =
  let rec bubble i =
    let c1, c2 = 2*i+1, 2*i+2 in
    let k = Array.length km in
    if c1 >= k then ()
    else begin
      let (_, d1, _) = km.(c1) in
      if c2 >= k
        then (if dist <= d1 then (km.(i) <- km.(c1); km.(c1) <- (pt, dist, cl); bubble c1))
      else
      let (_, d2, _) = km.(c2) in
      if d1 > d2
        then (if dist <= d1 then (km.(i) <- km.(c1); km.(c1) <- (pt, dist, cl); bubble c1))
        else (if dist <= d2 then (km.(i) <- km.(c2); km.(c2) <- (pt, dist, cl); bubble c2))
    end
  in
  let _, m_dist, _ = km.(0) in
  if dist <= m_dist then (km.(0) <- (pt, dist, cl); bubble 0)
;;


(* Q8 *)
let classifieKM k data pt =
  let classes = [|0; 0; 0|] in
  let km = creerKM k in
  Array.iter (fun (pt2, c) -> ajouterKM km (pt2, dist pt pt2, c)) data;
  Array.iter (fun (_, _, c) -> classes.(c) <- classes.(c) + 1) km;
  if classes.(0) > classes.(1) && classes.(0) > classes.(2) then 0 else
  if classes.(1) > classes.(0) && classes.(1) > classes.(2) then 1 else
  if classes.(2) > classes.(0) && classes.(2) > classes.(1) then 2 else
  -1
;;

Printf.printf "       k=\t1\t2\t3\t4\t5\n";
Printf.printf "(500,345)\t";
for k = 1 to 5 do Printf.printf "%d\t" (classifieKM k data [|500.; 345.|]) done;
print_newline ();
Printf.printf "(550,300)\t";
for k = 1 to 5 do Printf.printf "%d\t" (classifieKM k data [|550.; 300.|]) done;
print_newline (); print_newline ();;
