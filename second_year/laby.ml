(* Labyrinthe *)

type sommet = (int*int)

type arete = (sommet*sommet)

let afficheAretes (la:arete array) n m = 
  let cote = 10 in
  let s = " " ^ (string_of_int ((m - 1) * cote + 3)) ^ "x" ^ (string_of_int ((n - 1) * cote + 3)) in
  Graphics.open_graph s;
  Graphics.set_window_title "Labyrinthe";
  let affiche ((a,b),(c,d)) =
    Graphics.moveto (b * cote + 1) (a * cote + 1);
    Graphics.lineto (d * cote + 1) (c * cote + 1) 
  in 
  Array.iter affiche la
;;

let genereAretes n m =
  let aretes = Array.make (n*(m-1) + (n-1)*m) ((0,0),(0,0)) in
  let k = ref 0 in
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      if i < n-1 then aretes.(k) = ((i,),(i,))
  done

afficheAretes [| ((20,20),(20,21)); ((20,21),(21,21))  |] 40 60;;

let _ = Graphics.read_key ();;
