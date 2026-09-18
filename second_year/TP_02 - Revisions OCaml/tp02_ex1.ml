(* TP 02 - Exercice 1 - N Reines en OCaml *)

(* affiche un plateau nxn, et les reines positionnées *)
let affiche n lpos =
  (* n : la taille de l'échiquier  -  lpos : la liste des positions des reines *)
  let echiquier = Array.make_matrix n n false in
  let num_col = ref 0 in
  let marque i = 
    echiquier.(i).(!num_col) <- true;
    incr num_col
  in
  List.iter marque (List.rev lpos);
  print_endline "-PLATEAU-";
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      print_string (if echiquier.(i).(j) then "X" else "O")
    done;
    print_newline ()
  done;
  print_endline "--------"
;;

affiche 8 [4;2;0];;


(* affiche les solutions au problème des n-reines, ainsi que le nombre de solutions *)
let pb_reines n =
  let rec valide pos col nlig ncol =
    match pos with
    | [] -> true
    | lig::ll ->
      nlig != lig &&  (*ligne differente*)
      abs(col - ncol) != abs(lig - nlig) &&  (*diagonale differente*)
      valide ll (col-1) nlig ncol
  in
  let rec place pos col tot =
    if col = n then (
      affiche n pos ;
      incr tot
    ) else
      for lig = 0 to n-1 do
        if valide pos (col-1) lig col then
          place (lig :: pos) (col+1) tot
      done ;
  in
  let tot = ref 0 in
  place [] 0 tot ;
  Printf.printf "Nombre total de plateaux: %d\n" !tot
;;

pb_reines 10 ;;
