(* Ricochet Robots - partie 3 *)

(* Correction partie 2 - ce qui sera utile pour la partie 3 *)

let obs_l = [| [|0; 4; 10; 16|]; [|0; 14; 16|]; [|0; 6; 16|];
   [|0; 9; 16|]; [|0; 3; 15; 16|]; [|0; 7; 16|]; [|0; 1; 12; 16|]; [|0; 7; 9; 16|];
   [|0; 7; 9; 16|]; [|0; 4; 13; 16|]; [|0; 6; 16|]; [|0; 10; 16|]; [|0; 8; 16|];
   [|0; 2; 15; 16|]; [|0; 4; 10; 16|]; [|0; 5; 12; 16|] |];;

let obs_c = [| [|0; 5; 11; 16|]; [|0; 6; 13; 16|]; [|0; 4; 16|];
   [|0; 15; 16|]; [|0; 10; 16|]; [|0; 3; 16|]; [|0; 10; 16|]; [|0; 6; 7; 9; 12; 16|];
   [|0; 7; 9; 16|]; [|0; 3; 12; 16|]; [|0; 14; 16|]; [|0; 16|]; [|0; 7; 16|];
   [|0; 2; 10; 16|]; [|0; 4; 13; 16|]; [|0; 2; 12; 16|] |];;

let dichotomie a t =
  let i = ref 0 in  (* inclus *)
  let j = ref (Array.length t) in  (* exclu *)
  while !j - !i > 1 do
    let m = (!i + !j) / 2 in
    if t.(m) > a then j := m else i := m
  done;
  !i
;;

let deplacements_grille (a,b) =
   let imurG = dichotomie b obs_l.(a) in
   let imurH = dichotomie a obs_c.(b) in
   [| (a,obs_l.(a).(imurG));  (* ouest : même ligne, mur Gauche *)
      (a,obs_l.(a).(imurG+1)-1);  (* est : même ligne, mur Droit -1 *)
      (obs_c.(b).(imurH),b);  (* nord : mur Haut, même colonne *)
      (obs_c.(b).(imurH+1)-1,b)|]  (* sud : mur Bas -1, même colonne *)
;;

let matrice_deplacements () =
  let m = Array.make_matrix 16 16 [||] in
  for i = 0 to 15 do
    for j = 0 to 15 do
      m.(i).(j) <- deplacements_grille (i,j)
    done
  done;
  m
;;

let mat_depl = matrice_deplacements ();;

let modif t (a,b) (c,d) =
  if (a = c && b < d) then (* gene à l'est ? *)
     let (x,y) = t.(1) in
     t.(1) <- (x , min y (d-1))
  else if (a = c && b > d) then (* gene à l'ouest ? *)
     let (x,y) = t.(0) in
     t.(0) <- (x, max y (d+1))
  else if (b = d && a > c) then (* gene au nord ? *)
     let (x,y) = t.(2) in
     t.(2) <- (max x (c+1), y)
  else if (b = d && a < c) then (* gene au sud ? *)
     let (x,y) = t.(3) in
     t.(3) <- (min x (c-1), y)
;;

(* calcule les 4 déplacements possibles pour le robot  (a,b) en fonction de la liste des autres robots *)
let deplacements_robots (a,b) q =
  let t = Array.copy mat_depl.(a).(b) in  (* les deplacements sans gene *)
  List.iter (fun (c,d) -> modif t (a,b) (c,d)) q;  (* modification pour chaque robot *)
  t
;;


(* Partie 3 : on démarre ici ! *)

(* Fonctions utiles *)
let rec inserer liste x = 
  (* insere x dans une liste triée par ordre croissant.
     renvoie une nouvelle liste *)
  match liste with
  | [] -> [x]
  | e::ll -> if x < e then x::liste else e::(inserer ll x)
;;

let rec exclure liste x =
  (* exclure x d'une liste, supposé présent. 
     renvoie une nouvelle liste *)
  match liste with
  | [] -> raise Not_found
  | e::ll -> if x = e then ll else e::(exclure ll x)
;;

(* Une configuration du jeu. 
   robot : robot principal
   autres_robots : liste maintenue triée *)
type config = { robot : int * int; autres_robots : (int * int) list }

let affiche_config cfg =
  Printf.printf "robot:(%d,%d) autres:" (fst cfg.robot) (snd cfg.robot);
  List.iter (fun (x,y) -> Printf.printf "(%d,%d)" x y ) cfg.autres_robots;
  print_newline ()
;;

let config_voisines cfg =
  (* A partir d'une configuration renvoie la liste
     des 16 configurations voisines *)
  let voisins = ref [] in
  let d = deplacements_robots cfg.robot cfg.autres_robots in
  for i = 0 to 3 do
    voisins := {robot = d.(i) ; autres_robots = cfg.autres_robots}::!voisins
  done;
  let aux r =
    let excl = exclure cfg.autres_robots r in
    let d = deplacements_robots r (inserer excl cfg.robot) in
    for i = 0 to 3 do
      voisins := {robot = cfg.robot ; autres_robots = inserer excl d.(i)}::!voisins
    done;
  in
  List.iter aux cfg.autres_robots;
  !voisins
;;

let resol_largeur (cfg0:config) fin =
  (* A partir d'une configuration initiale cherche a atteindre
     une configuration finale par un parcours en largeur *)
  let gagne = ref false in
  let closed = Hashtbl.create 1 in Hashtbl.add closed cfg0 cfg0;
  let opened = Queue.create () in Queue.push cfg0 opened;
  let rec afficheSol cfg =
    affiche_config cfg;
    let parent = Hashtbl.find closed cfg in
    if cfg <> parent then afficheSol parent
  in
  while not (!gagne) do
    let cfg = Queue.pop opened in
    if cfg.robot = fin then (gagne := true; afficheSol cfg)
    else
      List.iter
        (fun cv -> if not (Hashtbl.mem closed cv) then (Queue.push cv opened; Hashtbl.add closed cv cfg))
        (config_voisines cfg)
  done;
  if (not !gagne) then Printf.printf "Pas de solution\n"
;;

Printf.printf "configuration facile:\n";
resol_largeur {robot = (5,12); autres_robots = [(0,7);(3,1);(4,0)]} (14, 3);;

Printf.printf "configuration initiale:\n";
resol_largeur {robot = (5,12); autres_robots = [(0,0);(2,1);(4,7)]} (14, 3);;

(*
⢀⡴⠑⡄⠀⠀⠀⠀⠀⠀⠀⣀⣀⣤⣤⣤⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ 
⠸⡇⠀⠿⡀⠀⠀⠀⣀⡴⢿⣿⣿⣿⣿⣿⣿⣿⣷⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠑⢄⣠⠾⠁⣀⣄⡈⠙⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⢀⡀⠁⠀⠀⠈⠙⠛⠂⠈⣿⣿⣿⣿⣿⠿⡿⢿⣆⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⢀⡾⣁⣀⠀⠴⠂⠙⣗⡀⠀⢻⣿⣿⠭⢤⣴⣦⣤⣹⠀⠀⠀⢀⢴⣶⣆ 
⠀⠀⢀⣾⣿⣿⣿⣷⣮⣽⣾⣿⣥⣴⣿⣿⡿⢂⠔⢚⡿⢿⣿⣦⣴⣾⠁⠸⣼⡿ 
⠀⢀⡞⠁⠙⠻⠿⠟⠉⠀⠛⢹⣿⣿⣿⣿⣿⣌⢤⣼⣿⣾⣿⡟⠉⠀⠀⠀⠀⠀ 
⠀⣾⣷⣶⠇⠀⠀⣤⣄⣀⡀⠈⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀ 
⠀⠉⠈⠉⠀⠀⢦⡈⢻⣿⣿⣿⣶⣶⣶⣶⣤⣽⡹⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⠀⠉⠲⣽⡻⢿⣿⣿⣿⣿⣿⣿⣷⣜⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣷⣶⣮⣭⣽⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⣀⣀⣈⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⠀⠹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀ 
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠻⠿⠿⠿⠿⠛⠉
*)
