(* Algorithme ID3 *)

(* Encodage des données *)

(* attr0 : Meteo - valeurs : soleil 0, nuageux 1, pluie 2 *)
(* attr1 : Temperature - chaud 0, moyen 1, froid 2 *)
(* attr2 : Humidité - normal 0, elevé 1 *)
(* attr3 : Vent - non 0, oui 1 *)

(* classe : NON 0, OUI 1 *)

let nb_cl = 2;; (* nombre de classes ; de 0 à nb_cl - 1 *)

let noms_attr = [| "Meteo"; "Temperature"; "Humidité"; "Vent" |];;

let nb_val = [| 3; 3; 2; 2|];; (* nombre de valeurs par attribut ; de 0 à na - 1 *)

let data = [ ([|0; 0; 1; 0|], 0); (* les 14 exemples *)
             ([|0; 0; 1; 1|], 0);
             ([|1; 0; 1; 0|], 1);
             ([|2; 1; 1; 0|], 1);
             ([|2; 2; 0; 0|], 1);
             ([|2; 2; 0; 1|], 0);
             ([|1; 2; 0; 1|], 1);
             ([|0; 1; 1; 0|], 0);
             ([|0; 2; 0; 0|], 1);
             ([|2; 1; 0; 0|], 1);
             ([|0; 1; 0; 1|], 1);
             ([|1; 1; 1; 1|], 1);
             ([|1; 0; 0; 0|], 1);
             ([|2; 1; 1; 1|], 0) ];;

type arbre = 
    F of (int * float) (* la classe et la proportion associée *)
  | N of (int * arbre array) (* l'attribut et le tableau des sous arbres *)


let arbre_test = N (0, [| F (0, 1.); F (1, 0.7); F (-1, 1.)|]);;

let affiche_arbre arb =
  let rec affiche_espace n = 
    if n > 0 then begin print_string " "; affiche_espace (n-1) end
  in
  let rec affiche_aux decalage arb = affiche_espace decalage; match arb with
    | F (c, prob) -> Printf.printf "[Classe %d (%f)]\n" c prob
    | N (attr, fils_tab) -> begin
        Printf.printf "%s \n" noms_attr.(attr);
        Array.iter (fun a -> affiche_aux (decalage+3) a) fils_tab;
    end
  in
  affiche_aux 0 arb
;;

affiche_arbre arbre_test;;


(* Q2 *)
let rec decide (arb:arbre) (obj:int array) =
  match arb with
  | N(attr, sarb) -> decide sarb.(obj.(attr)) obj
  | _ -> arb
;;


(* Q3 *)
let rec construire (e:bool) (data:(int array * int) list) (attr_list:int list) : arbre =
  let entropie data =
    let log2 x = log x /. log 2. in
    let cl = Array.make nb_cl 0 in
    let length = ref 0 in
    List.iter (fun x -> let (_,s) = x in cl.(s) <- cl.(s) + 1; incr length) data;
    let e = ref 0. in
    Array.iter (fun x -> let p = (float_of_int x)/.(float_of_int !length) in e := !e +. p*.(log2 p)) cl;
    -.e
  in
  let gain data attr =
    let rec filter data lisarr lenarr =
      match data with
      | [] -> lisarr,lenarr
      | (f,s)::ddata ->
        let fa = f.(attr) in
        lisarr.(fa) <- (f,s)::lisarr.(fa);
        lenarr.(fa) <- lenarr.(fa) + 1;
        filter ddata lisarr lenarr
    in
    let tot = List.length data in
    let g = entropie data |> ref in
    let (lisarr, lenarr) = filter data (Array.make nb_val []) (Array.make nb_val 0);
    for i = 0 to nb_val-1 do
      g := !g -. entropie lisarr.(i) *. lenarr.(i) /. tot
    done;
    !g
  in
  let meilleur_attr =
    ()
  in
  match attr_list with
  | [] ->
    let cl = Array.make nb_cl 0 in
    let cl_max = ref (-1) in
    let length = ref 0 in
    List.iter (fun x -> let (_,s) = x in cl.(s) <- cl.(s) + 1; if !cl_max = (-1) || cl.(!cl_max) < cl.(s) then cl_max := s; incr length) data;
    F(!cl_max, if !length <> 0 then (float_of_int cl.(!cl_max))/.(float_of_int !length) else 1.)

  | attr::attr_llist ->
    if data = [] then F(-1, 1.)
    else
    let cl = List.hd data |> snd in
    if List.for_all (fun (_,s) -> s = cl) data then F(cl, 1.)
    else
    let sarb i = construire e (List.filter (fun (f,_) -> f.(attr) == i) data) attr_llist in
    N(attr, Array.init nb_val.(attr) sarb)
;;


(* Q4 * construire false data [2; 3; 1; 0] |> affiche_arbre; *)

(* Q5 * construire true data [2; 3; 1; 0] |> affiche_arbre *)
