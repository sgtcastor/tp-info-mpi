(* Q1 *)
type arbre_calcul =
  | Op of (float -> float -> float) * arbre_calcul * arbre_calcul
  | Val of float
;;

(* Q2 *)
let arbre0 = Op((+.), Val 5., Op((/.), Op(( *.), Val 3., Val 4.), Op((+.), Val 1., Op((+.), Val 1., Val 1.))))

(* Q3 *)
let rec evalue arbre =
  match arbre with
  | Op (f, arbreG, arbreD) -> f (evalue arbreG) (evalue arbreD)
  | Val x -> x
;;
print_float (evalue arbre0) ;
print_newline ()

(* Q4 *)
let rec print_arbre arbre =
  match arbre with
  | Val x -> print_float x
  | Op (f, arbreG, arbreD) ->
    begin
      print_char '(' ;
      match f with
      | (+.) -> print_arbre arbreG ; print_string " +. " ; print_arbre arbreD
      | (-.) -> print_arbre arbreG ; print_string " -. " ; print_arbre arbreD
      | ( *.) -> print_arbre arbreG ; print_string " *. " ; print_arbre arbreD
      | (/.) -> print_arbre arbreG ; print_string " /. " ; print_arbre arbreD
      ;
      print_char ')'
    end
;;
print_arbre arbre0 ;
print_newline ()
