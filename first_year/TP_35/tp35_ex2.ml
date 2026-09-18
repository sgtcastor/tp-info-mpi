(******* TP table de hachage ******)

Random.self_init ();; (* initialisation aléatoire du générateur *)

let table = Hashtbl.create 30 in
let nb_dbl = ref 0 in
for i = 1 to 30 do
  let x = 1 + Random.int 365 in
  if Hashtbl.mem table x then
    nb_dbl := !nb_dbl + 1
  else
    Hashtbl.add table x ()
done;
Printf.printf "%d doublon%s !" !nb_dbl (if !nb_dbl = 1 then "" else "s")
