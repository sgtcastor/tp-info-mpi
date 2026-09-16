(* arbre 2-dimensionnel *)

let d = 2;; 

type point = float array;; (* tableaux de longueur d *)

type kdtree = E | N of kdtree * point * kdtree;; 

let pt_test = [|
[| 3.3 ; 0.9 |];
[| 2.0 ; 4.5 |];
[| 3.7 ; 5.1 |];
[| 8.4 ; 6.0 |];
[| 2.7 ; 1.8 |];
[| 1.2 ; 4.4 |];
[| 5.1 ; 3.6 |];
[| 5.7 ; 1.8 |];
[| 4.0 ; 1.4 |];
[| 6.6 ; 2.4 |];
[| 6.4 ; 0.7 |];
[| 7.9 ; 5.5 |];
|];;


let affiche_kdtree tree =
  let rec affiche_espace n = 
    if n > 0 then begin print_string " "; affiche_espace (n-1) end
  in
  let rec affiche_aux decalage tree =  match tree with
    | E -> ()
    | N (ag, pt, ad) -> begin
        affiche_aux (decalage + 3) ag;
        affiche_espace decalage; Printf.printf "(%f,%f) \n" pt.(0) pt.(1);
        affiche_aux (decalage + 3) ad;
    end
  in
  affiche_aux 0 tree
;;


(* Q10 *)
let rec inserer (tree:kdtree) (pt:point) (p:int) : kdtree =
  match tree with
  | E -> N(E, pt, E)
  | N(l, m, r) ->
    let i = p mod (Array.length m) in
    if pt.(i) <= m.(i)
      then N(inserer l pt (p+1), m, r)
      else N(l, m, inserer r pt (p+1))
;;


(* Q11 *)
let construire (arr:point array) : kdtree =
  let tree = ref E in
  Array.iter (fun pt -> tree := inserer !tree pt 0) arr;
  !tree
;;
let tree = construire pt_test;;
(* affiche_kdtree tree;; *)


(* Q12 *)
let dist (p1:float array) (p2:float array) : float =
  let sum = ref 0. in
  for i = 0 to Array.length p1 - 1 do
    sum := !sum +. (p1.(i) -. p2.(i))**2.
  done;
  (!sum)**0.5
;;

let nn (tree:kdtree) (pt:point) : point =
  let npt = ref ([||], infinity) in
  let rec aux (tree:kdtree) (p:int) =
    match tree with
    | E -> ()
    | N(l, m, r) ->
      let d = dist pt m in
      if d < snd !npt then npt := (m, d) else ();
      let i = p mod Array.length m in
      let diff = pt.(i) -. m.(i) in
      aux (if diff <= 0. then l else r) (p+1);
      if snd !npt > abs_float diff then
        aux (if diff <= 0. then r else l) (p+1)
  in
  aux tree 0;
  fst !npt
;;


(* Q13 *)
let pt = nn tree [|3.2; 4.4|] in
Array.iter (Printf.printf "%f ") pt;
print_newline();;


(* Q14 *)
type maxheap = (float array * float) array;;

let creerMH k : maxheap = Array.make k ([||], infinity);;

let ajouterMH (mh:maxheap) e f =
  let dist = f e in
  let rec bubble i =
    let c1, c2 = 2*i+1, 2*i+2 in
    let k = Array.length mh in
    if c1 >= k then ()
    else begin
      let d1 = f mh.(c1) in
      if c2 >= k
        then (if dist <= d1 then (mh.(i) <- mh.(c1); mh.(c1) <- e; bubble c1))
      else
      let d2 = f mh.(c2) in
      if d1 > d2
        then (if dist <= d1 then (mh.(i) <- mh.(c1); mh.(c1) <- e; bubble c1))
        else (if dist <= d2 then (mh.(i) <- mh.(c2); mh.(c2) <- e; bubble c2))
    end
  in
  let d = f mh.(0) in
  if dist <= d then (mh.(0) <- e; bubble 0)
;;

let knn (k:int) (tree:kdtree) (pt:point) : point array =
  let npt = creerMH k in
  let rec aux (tree:kdtree) (p:int) =
    match tree with
    | E -> ()
    | N(l, m, r) ->
      let d = dist pt m in
      if d < snd npt.(0) then ajouterMH npt (m, d) snd else ();
      let i = p mod Array.length m in
      let diff = pt.(i) -. m.(i) in
      aux (if diff <= 0. then l else r) (p+1);
      if snd npt.(0) > abs_float diff then
        aux (if diff <= 0. then r else l) (p+1)
  in
  aux tree 0;
  Array.map fst npt
;;
