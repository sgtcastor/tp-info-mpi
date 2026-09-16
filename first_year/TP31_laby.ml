let passage = 1 ;;
let chemin = 2 ;;
let culdesac = 3 ;;
let small_laby = 
  [|[|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|];
    [|0;1;0;1;0;1;0;1;1;1;0;1;0;1;1;1;1;1;0;1;0|];
    [|0;1;0;1;0;1;0;1;0;0;0;1;0;1;0;0;0;1;0;1;0|];
    [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;1;1;1;0|];
    [|0;1;0;1;0;0;0;0;0;0;0;0;0;1;0;1;0;1;0;0;0|];
    [|0;1;1;1;1;1;0;1;1;1;0;1;1;1;0;1;1;1;1;1;0|];
    [|0;1;0;0;0;1;0;1;0;1;0;1;0;0;0;0;0;1;0;0;0|];
    [|0;1;1;1;0;1;1;1;0;1;1;1;1;1;0;1;1;1;1;1;0|];
    [|0;1;0;1;0;1;0;0;0;1;0;0;0;1;0;1;0;0;0;1;0|];
    [|0;1;0;1;0;1;1;1;0;1;1;1;0;1;1;1;1;1;0;1;0|];
    [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|]|];;

(* Q8 *)
let voisins_inexplore (laby:int array array) (i:int) (j:int) =
  List.filter (fun (i, j) -> laby.(i).(j) = passage) [(i, j+1); (i+1, j); (i-1, j); (i, j-1)]
;;

(* Q9 *)
let parcours_profondeur (laby:int array array) =
  let explo = ref 0 in
  let pred = Stack.create () in
  let rec aux (i, j) =
    explo := !explo + 1;
    if (i, j) = (Array.length laby - 2, Array.length laby.(i) - 2) then  (*-- Q9 --*)
      begin
        Printf.printf "Nombre de cases explorées: %d\n" !explo;
        Printf.printf "(%d, %d)" i j;
        while not (Stack.is_empty pred) do
          let (x, y) = Stack.pop pred in
          Printf.printf " (%d, %d)" x y
        done;
        print_newline ();
      end  (* --- *)
    else
    if voisins_inexplore laby i j = [] then
      (laby.(i).(j) <- culdesac;
      aux (Stack.pop pred))
    else
      (laby.(i).(j) <- chemin;
      Stack.push (i, j) pred;
      List.iter aux (voisins_inexplore laby i j))
  in
  aux (1, 1)
;;

parcours_profondeur small_laby
