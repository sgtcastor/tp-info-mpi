(* run: ocaml str.cma tp21.ml prog1.mc *)

(***** Langage Mini-C ****)

(** Analyse lexicale **)

(* les tokens *)

type tok_t = PtV | Plus | Prt | DPt (* ; + print : *)
     | Eq | ParG | ParD  (* = ( ) *)
     | Ident of string  (* identifiant *)
     | Num of int  (* nombre *)

let tok_of_string s : tok_t =
  (* transforme une chaîne de caractère matchée par la Regexp en token *)
  (* A COMPLETER Q2 *)
  match s with
  | ";" -> PtV | "+" -> Plus | "print" -> Prt
  | ":" -> DPt | "=" -> Eq   | "(" -> ParG | ")" -> ParD
  | _ -> try Num (int_of_string s) with _ -> Ident s
;;

let analyse_lexicale (texte:string) : tok_t list =
  let re = Str.regexp "[;+:=()]\\|[a-z]+\\|[0-9]+" in (* regexp A COMPLETER Q1 *)
  let rec tokenize texte i = (* renvoie la liste de tokens pour texte, pris à partir de l'indice i *)
    if i = String.length texte then
      []
    else 
      try
        let j = Str.search_forward re texte i in
        let str = Str.matched_string texte in
        if str = " " || str = "\n" then
          tokenize texte (j + String.length str)
        else
          (tok_of_string str) :: tokenize texte (j + String.length str)
      with 
        Not_found -> []
  in
  tokenize texte 0
;;

(* Tests Q3 *)
assert (analyse_lexicale "x := 3; print(x)" = [Ident "x"; DPt; Eq; Num 3; PtV; Prt; ParG; Ident "x"; ParD]);;
assert (analyse_lexicale "a b+33 c ==" = [Ident "a"; Ident "b"; Plus; Num 33; Ident "c"; Eq; Eq]);;
assert (analyse_lexicale "(aaa(:print:" = [ParG; Ident "aaa"; ParG; DPt; Prt; DPt]);;


(** Analyse syntaxique **)

(* arbre de syntaxe abstraite *)

type progr_t = Uniq of instr_t | Seq of instr_t * progr_t

and instr_t = Affiche of expr_t | Affect of string * expr_t

and expr_t = Val of string | Add of expr_t * expr_t | Entier of int


let rec parse_expr (l:tok_t list) : expr_t * tok_t list = 
  match l with
  | (Ident s)::Plus::l2 -> let (expr, r) = parse_expr l2 in Add (Val s, expr), r
  | (Num x)::Plus::l2 -> let (expr, r) = parse_expr l2 in Add (Entier x, expr), r
  | (Ident s)::r -> Val s, r
  | (Num x)::r -> Entier x, r
  | _ -> failwith "Syntax error"
;;

let rec parse_instr (l:tok_t list) : instr_t * tok_t list =
  match l with
  | Prt::ParG::l2 -> let (expr, l3) = parse_expr l2 in
    (match l3 with ParD::r ->  Affiche expr, r | _ -> failwith "Syntax error")
  | (Ident s)::DPt::Eq::l2 -> let (expr, r) = parse_expr l2 in Affect (s, expr), r
  | _ -> failwith "Syntax error"
;;

let rec parse_progr (l:tok_t list) : progr_t * tok_t list = 
  let (instr, l2) = parse_instr l in
  match l2 with
  | [PtV] -> Uniq (instr), []
  | PtV::l3 -> let (progr, r) = parse_progr l3 in Seq (instr, progr), r
  | _ -> failwith "Syntax error"
;;

let analyse_syntaxique tok_list = 
  let prog, l = parse_progr tok_list in
  if l = [] then prog else failwith "Erreur"
;;


(** Evaluation de l'arbre de syntaxe abstraite **)

(* Environnement d'évaluation Q5 *)

let new_env () = Hashtbl.create 0;;
let get_val = Hashtbl.find;;
let set_val = Hashtbl.replace;;

(* Fonctions d'évaluation Q6 *)

let rec evalue_expr expr env =
  match expr with
  | Val s -> get_val env s
  | Add (e1, e2) -> (evalue_expr e1 env) + (evalue_expr e2 env)
  | Entier x -> x
;;

let rec evalue_instr instr env =
  match instr with
  | Affiche expr -> Printf.printf "%d\n" (evalue_expr expr env)
  | Affect (s, e) -> set_val env s (evalue_expr e env)
;;

let rec evalue_progr progr env =
  match progr with
  | Uniq instr -> evalue_instr instr
  | Seq (instr, progr) -> evalue_instr instr env; evalue_progr progr env
;;

(** Fonction d'évaluation **)

let interprete filename =

  (* lecture du code source fichier *)
  let ch = open_in filename in
  let texte = ref "" in
  begin
  try 
    while true do
      let ligne = input_line ch in
      texte := !texte ^ ligne ^ "\n"
    done
  with
    End_of_file -> ()
  end;
  close_in ch;
  (* print_endline !texte; *)

  ((analyse_lexicale !texte |> parse_progr).fst |> evalue_progr) (new_env ())
;;

interprete Sys.argv.(1);;