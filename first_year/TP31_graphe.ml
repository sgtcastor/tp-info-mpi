(* types pour représenter les graphes *)

type grapheLA = int list array (* listes d'adjacence *)

(* Q1 *)

let g0 = [|[2];  (* voisins de 0 *)
           [0; 2];  (* voisins de 1 *)
           [1]|] ;;  (* voisins de 2 *)

(* Q2 *)

let affiche_graphe (g : grapheLA) = 
  let n = Array.length g in
  for i = 0 to n-1 do
    Printf.printf "Le sommet %d a pour successeurs : " i;
    List.iter (Printf.printf "%d ") g.(i);
    print_newline ()
  done
;;

print_endline "--- g0 ---" ;;
affiche_graphe g0 ;;
print_newline () ;;

(* Q3 *)
let g1 = [|[1; 2];  (* successeurs de 0 *)
           [3; 4];  (* successeurs de 1 *)
           [];  (* successeurs de 2 *)
           [0];  (* successeurs de 3 *)
           [3; 5];  (* successeurs de 4 *)
           [2; 6];  (* successeurs de 5 *)
           [4]|] ;;  (* successeurs de 6 *)

print_endline "--- g1 ---" ;;
affiche_graphe g1 ;;
print_newline () ;;

(* Q4 *)
let parcoursP (g:grapheLA) (s:int) = 
  let closed = Array.make (Array.length g) false in
  let rec parcours (s:int) =
    if not closed.(s) then
      (Printf.printf "%d " s; closed.(s) <- true; List.iter parcours g.(s))
    else () in
  parcours s
;;

let test f (s:int) =
  Printf.printf "depuis le sommet %d\n" s;
  f g1 s;
  print_newline ()
;;

(* Q5 *)
print_endline "--- parcoursP ---" ;;
test parcoursP 0 ;;
test parcoursP 1 ;;
test parcoursP 2 ;;
print_newline ()

(* Q6 *)
let parcoursPile (g:grapheLA) (s:int) =
  let closed = Array.make (Array.length g) false in
  let opened = Stack.create () in
  Stack.push s opened;
  while not (Stack.is_empty opened) do
    let s = Stack.pop opened in
    if not closed.(s) then
      begin
        Printf.printf "%d " s;
        closed.(s) <- true;
        List.iter (fun s -> Stack.push s opened) g.(s)
      end
    else ()
  done
;;

print_endline "--- parcoursPile ---" ;;
test parcoursPile 3 ;;
test parcoursPile 4 ;;
test parcoursPile 2 ;;
print_newline ()

(* Q7 *)
let parcoursFile (g:grapheLA) (s:int) =
  let closed = Array.make (Array.length g) false in
  let opened = Queue.create () in
  Queue.push s opened;
  while not (Queue.is_empty opened) do
    let s = Queue.pop opened in
    if not closed.(s) then
      begin
        Printf.printf "%d " s;
        closed.(s) <- true;
        List.iter (fun s -> Queue.push s opened) g.(s)
      end
    else ()
  done
;;

print_endline "--- parcoursFile ---" ;;
test parcoursFile 0 ;;
test parcoursFile 1 ;;
test parcoursFile 6 ;;
