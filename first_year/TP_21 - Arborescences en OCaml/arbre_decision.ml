(* Exercice 2 *)

(* Type arbre de décision : *)

type arbre_decision = 
  | Question of string * arbre_decision * arbre_decision 
  | Decision of string 

(* NB : Question (question, arbre_oui, arbre_non) *)

let arbre_medical = 
  Question ("mal au ventre ?", 
    Decision "appendicite",
    Question ("mal à la gorge ?",
      Question ("fièvre ?",
        Decision "rhume",
        Decision "mal de gorge"),
      Question ("toux ?",
        Question ("fièvre ?",
          Decision "rhume",
          Decision "refroidissement"),  
        Decision "rien"
      )
    )
  );;

(* Q7 *)
let rec questionner arbre =
  match arbre with
  | Question (q, o, n) -> print_string q ; if read_line () = "o" then questionner o else questionner n
  | Decision d -> d
;;

(* Q8 *)
let docteur =
  print_endline "Bienvenue chez le docteur" ;
  print_endline "Je vais vous poser quelques questions" ;
  let diag = questionner arbre_medical in
  print_string "Voici mon diagnostic: " ;
  print_endline diag
;;
docteur