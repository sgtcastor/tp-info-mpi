(* Implementation Union-Find *)

type unionfind = {parent:int array; rang:int array}

let afficheUF (uf: unionfind) =
  let n = Array.length uf.parent in
  Printf.printf "uf: ";
  for i = 0 to n-1 do
    Printf.printf "(%d->%d) " i uf.parent.(i)
  done;
  print_newline ()
;;


let creeUF n = {parent=Array.init n (fun i -> i); rang=Array.make n 0};;

let rec find uf x = let y = uf.parent.(x) in if y = x then x else find uf y;;

let union uf x y =
  let rx = find uf x in
  let ry = find uf y in
  if rx <> ry then
    uf.parent.(rx) <- ry
;;

let uf = creeUF 5 in
union uf 2 4;
union uf 1 3;
union uf 2 3;
afficheUF uf;;

let unionR uf x y =
  let rx = find uf x in
  let ry = find uf y in
  if rx <> ry then begin
    let rgx = uf.rang.(rx) in
    let rgy = uf.rang.(ry) in
    if rgx = rgy then uf.rang.(rx) <- uf.rang.(rx)+1;
    if rgx < rgy then uf.parent.(rx) <- ry else uf.parent.(ry) <- rx
  end
;;

let uf = creeUF 5 in
unionR uf 2 4;
unionR uf 1 3;
unionR uf 2 3;
afficheUF uf;;

let rec findRC uf x =
  let y = uf.parent.(x) in
  if y = x then x
  else
    let r = find uf y in
    uf.parent.(x) <- r; r
;;

let unionRC uf x y =
  let rx = findRC uf x in
  let ry = findRC uf y in
  if rx <> ry then begin
    let rgx = uf.rang.(rx) in
    let rgy = uf.rang.(ry) in
    if rgx = rgy then uf.rang.(rx) <- rgx + 1;
    if rgx < rgy then uf.parent.(rx) <- ry else uf.parent.(ry) <- rx
  end
;;

let uf = creeUF 5 in
unionRC uf 2 4;
unionRC uf 1 3;
unionRC uf 2 3;
afficheUF uf;;

let rec profondeur uf x =
  let y = uf.parent.(x) in
  if y = x then 0 else profondeur uf y + 1
;;

let hauteur_max uf =
  let mh = ref 0 in
  for i = 0 to Array.length uf.parent - 1 do
    let h = profondeur uf i in
    if h > !mh then mh := h
  done; !mh
;;

let test f uf =
  let n = Array.length uf.parent in
  for i = 0 to n - 1 do
    f uf (Random.int n) (Random.int n)
  done
;;

Random.self_init ();;
let uf = creeUF   10_000 in test union   uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF   10_000 in test unionR  uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF   10_000 in test unionRC uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF  100_000 in test unionR  uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF  100_000 in test unionRC uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF 1000_000 in test unionR  uf; hauteur_max uf |> print_int; print_newline ();;
let uf = creeUF 1000_000 in test unionRC uf; hauteur_max uf |> print_int; print_newline ();;
