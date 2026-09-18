type ('a, 'b) hashmap = {
  mutable data : ('a * 'b) list array;
  mutable length : int
}

let create length =
  {data = (Array.make length []); length = length}
;;

let add hm key value =
  let i = Hashtbl.hash key mod (Array.length hm.data) in
  hm.data.(i) <- (key, value)::hm.data.(i);
  hm.length <- hm.length + 1
;;

let find hm key =
  let rec aux l =
    match l with
    | [] -> failwith "Not_found"
    | (k, v)::ll -> if k = key then v else aux ll
  in
  aux hm.data.(Hashtbl.hash key mod (Array.length hm.data))
;;

let remove hm key =
  let rec aux l =
    match l with
    | [] -> failwith "Not_found"
    | (k, v)::ll -> if k = key then ll else (k, v)::(aux ll)
  in
  let i = Hashtbl.hash key mod (Array.length hm.data) in
  hm.data.(i) <- aux hm.data.(i);
  hm.length <- hm.length - 1
;;

let replace hm key value =
  remove hm key;
  add hm key value
;;


(******* Tests ******)


let dict = create 10;;

(* Q1 *)
add dict "Guéret" "23000";;

(* Q4 *)
add dict "Limoges" "87000";;
add dict "Limoges" "87100";;
add dict "Tulle" "19000";;

(* Q5 *)
Printf.printf "Guéret : %s\n" (find dict "Guéret");;

(* Q6 *)
(*
Printf.printf "Périgueux : %s\n" (find dict "Périgueux");;  (* Exception: Not_found. *)
*)
Printf.printf "Périgueux : Not_found.\n";;

(* Q7 *)
Printf.printf "Limoges : %s\n" (find dict "Limoges");;

(* Q8 *)
remove dict "Limoges";;
Printf.printf "-Suppression de Limoges\n";;
Printf.printf "Limoges : %s\n" (find dict "Limoges");;

(* Q9 *)
remove dict "Limoges";;
Printf.printf "-Suppression de Limoges\n";;
(*
Printf.printf "Limoges : %s\n" (find dict "Limoges");;  (* Exception: Not_found. *)
*)
Printf.printf "Limoges : Not_found.\n";;

(* Q10 *)
add dict "Limoges" "87000";;
add dict "Limoges" "87100";;
