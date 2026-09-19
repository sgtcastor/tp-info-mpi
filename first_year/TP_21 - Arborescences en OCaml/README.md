# TP 21 : Arborescences en OCaml

## 1 Exemple : arbre d'expression arithmétique

Dans un fichier `arbre_calcul.ml`

**Q1.** Définir un type somme `arbre_calcul` décrivant une expression arithmétique sur les réels (avec les 4 opérations habituelles)

**Q2.** Définir une variable `arbre0` correspondant à l'expression : `5 + ((3 * 4) / (1 + (1 + 1)))`

**Q3.** Écrire une fonction `evalue:arbre_calcul->float` évaluant une expression arithmétique. Évaluer `arbre0`.

**Q4.** Écrire une fonction `print_arbre_calcul` qui affiche sur une ligne l'expression à calculer. Afficher `arbre0`. (**★ appel professeur**)

**Q5.** _Bonus_ n'afficher que les parenthèses nécessaires, en tenant compte des priorités opératoires usuelles. Dans l'expression précédente, une seule paire de parenthèses est nécessaire.

## 2 Exemple : arbre de décision

Le fichier `arbre_decision.ml` contient la définition d'un type permettant de représenter un arbre de décision, ainsi qu'un exemple.

**Q6.** Représenter l'arbre de décision exemple.

**Q7.** Écrire une fonction `questionner:arbre_decision->string` qui réalise le parcours prévu par l'arbre en interrogeant un utilisateur et renvoie la décision finale.

**NB :** _utiliser `read_line:unit->string` cf Mémo OCaml_

**Q8.** Écrire une fonction `docteur:unit->unit` qui simule une visite chez le docteur (**★ appel professeur**) :

```
Bienvenue chez le docteur
Je vais vous poser quelques questions
...
Voici mon diagnostic ...
```

## 3 Arbre binaire polymorphe en OCaml

**Q9.** Définir le type `'a arbre` permettant de décrire des arbres binaires à étiquettes de type `'a`

Le fichier `tp21.ml` contient la définition du type polymorphe ci-dessus, ainsi qu'une fonction d'affichage `print_arbre:int arbre -> unit`, dont on ne cherchera pas à comprendre le code.

**Q10.** Définir 3 arbres à étiquettes entières : `arbre0` l'arbre vide, `arbre1` un arbre à 2 noeuds, `arbre2` un arbre à 4 noeuds de hauteur 2. Afficher ces arbres.

**Q11.** Définir les fonctions `taille:'a arbre->int` et `hauteur:'a arbre->int`. Tester vos fonctions sur les 3 arbres.

**NB :** _utiliser la fonction **max**, cf Mémo OCaml._

**Q12.** Définir une fonction `genere_peigne_gauche:int->int arbre` qui génère un arbre peigne à gauche de hauteur `h` avec étiquettes entières alétoires entre 0 et 9. Générer `arbre3`, un arbre peigne de hauteur 6, et l'afficher.

**NB :** _`Random.int n` renvoie un nombre entier aléatoire entre 0 et n-1_

**Q13.** Définir une fonction `genere_parfait:int->int arbre` qui génère un arbre parfait de hauteur `h` avec étiquettes entières alétoires entre 0 et 9. Générer `arbre4`, un arbre parfait de hauteur 3, et l'afficher.

**Q14.** Définir une fonction `genere_arbre:int->int arbre` qui génère un arbre de `n` noeuds. La répartition des noeuds dans chaque sous-arbre sera faite aléatoirement. Générer `arbre5`, un arbre aléatoire contenant 20 noeuds, et l'afficher. (**★ appel professeur**)

**Q15.** Définir une fonction `est_peigne:'a arbre->bool`

**Q16.** Définir une fonction `esr_parfait:'a arbre->bool`. (Ce n'est pas trivial, et essayez de ne pas faire de parcours superflu) (**★ appel professeur**)

## 4 Parcours d'arbres

**Q17.** Écrire les fonctions `parcours_prefixe:('a arbre->unit)->'a arbre->unit` telle que `parcours_prefixe f arb` applique une fonction `f` à chaque noeud de l'arbre `arb` dans l'ordre préfixe. Tester sur `arbre5` avec la fonction `print_int`.

**Q18.** Écrire de même les fonctions `parcours_infixe` et `parcours_postfixe`. Tester sur `arbre5`.

Le parcours en largeur est plus difficile à implémenter. On utilise une file (module `Queue`, file polymorphe en OCaml), et on applique l'algorithme suivant :

- On place l'arbre initial dans la file
- Tant que la file n'est pas vide
  - on défile, et si l'arbre n'est pas vide ...
  - ... on applique `f` à la racine
  - ... on enfile les sous-arbres gauches et droit

**NB :** _la correction de cet algorithme est loin d'être triviale. On se convaincra sur un exemple qu'il semble fonctionner._

**Q19.** Écrire la fonction `parcours_largeur` qui utilisera les fonctions `Queue.create`, `Queue.is_empty`, `Queue.push`, et `Queue.pop` (voir Memo OCaml). Tester sur `arbre` (**★ appel professeur**)

**Q20.** Définir un arbre n-aire `n_arbre0` de hauteur 2 tel que la racine ait 3 fils, son premier fils n'ait aucun fils, son second fils ait un unique fils et son troisième fils ait deux fils.

**Q21.** Définir deux fonctions mutuellement récursives permettant de calculer la taille d'un arbre et d'une forêt. Tester sur `n_arbre0`. (**★ appel professeur**)

**NB :** _pour définir `f1` et `f2` mutuellement récursives : `let rec f1 ... = ... and f2 ... = ...`_

## 6 Joli affichage

_En bonus_ : effacer la fonction `print_arbre` et la réimplémenter par vous-même !
**Principe** : à chaque arbre, on associe un _bloc_ rectangulaire représentant les lignes à afficher. En pratique, ce bloc est un tableau de chaînes de caractères de même longueurs.
