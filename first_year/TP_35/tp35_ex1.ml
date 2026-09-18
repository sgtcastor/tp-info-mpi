(******* TP table de hachage ******)
    
let dict = Hashtbl.create 10;;

(* Q1 *)
Hashtbl.add dict "Guéret" "23000";;

(* Q4 *)
Hashtbl.add dict "Limoges" "87000";;
Hashtbl.add dict "Limoges" "87100";;
Hashtbl.add dict "Tulle" "19000";;

(* Q5 *)
Printf.printf "Guéret : %s\n" (Hashtbl.find dict "Guéret");;

(* Q6 *)
(*
Printf.printf "Périgueux : %s\n" (Hashtbl.find dict "Périgueux");;  (* Exception: Not_found. *)
*)
Printf.printf "Périgueux : Not_found.\n";;

(* Q7 *)
Printf.printf "Limoges : %s\n" (Hashtbl.find dict "Limoges");;

(* Q8 *)
Hashtbl.remove dict "Limoges";;
Printf.printf "-Suppression de Limoges\n";;
Printf.printf "Limoges : %s\n" (Hashtbl.find dict "Limoges");;

(* Q9 *)
Hashtbl.remove dict "Limoges";;
Printf.printf "-Suppression de Limoges\n";;
(*
Printf.printf "Limoges : %s\n" (Hashtbl.find dict "Limoges");;  (* Exception: Not_found. *)
*)
Printf.printf "Limoges : Not_found.\n";;

(* Q10 *)
Hashtbl.add dict "Limoges" "87000";;
Hashtbl.add dict "Limoges" "87100";;

Printf.printf "\n--- Itération ---\n";;
Hashtbl.iter (fun key value -> Printf.printf "%s : %s\n" key value) dict;;
