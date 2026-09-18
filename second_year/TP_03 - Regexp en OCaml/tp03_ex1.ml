(* TP03 - Exercice 1 *)

(* Renvoie la liste des sous-chaînes (facteurs) de texte qui matchent la regexp re *)
let find_all re texte = 
  let rec find_all_aux re texte i (* a partir de la position i *) =
    try 
      let j = Str.search_forward re texte i in
      let str = Str.matched_string texte in
      begin
        (*Printf.printf "pos : %d - len : %d\n" j (String.length mstr);*)
        str :: find_all_aux re texte (j + String.length str)
      end
    with
      Not_found -> []
  in
  find_all_aux re texte 0
  
let affiche = List.iter (fun x -> Printf.printf "match:%s\n" x);;

(* Exemple d'utilisation *)

let reg = Str.regexp "[ab]+";;

let str0 = "cbaabcaaaabca";;

affiche (find_all reg str0);;

(* Un texte dont on veut extraire des informations *)

let lettreRentree = "Madame, Monsieur, Chers parents d'eleves du lycee Gay-Lussac,
Tous nos eleves sont maintenant rentres. J'en profite pour souhaiter la bienvenue aux nouveaux eleves de
seconde et de premiere annee de CPGE, en esperant que vos conges de juillet et aout aient ete reposants.
Le lycee dispose de son equipe complete d'enseignants. Tous les cours seront donc assures.
L'emplois du temps de tous les eleves du lycee et des etudiants des classes preparatoires debutera donc ce
lundi.
Mme Nicolas, proviseure adjointe et moi-meme reuniront les lundi 5 et mardi 6 septembre tous les eleves du
lycee afin de leur exposer les grandes lignes du fonctionnement de l'etablissement.
De plus, une premiere rencontre permettra aux familles de rencontrer les equipes de professeurs les :
- jeudi 15 septembre 2022 a 18h00 pour les classes de seconde.
- jeudi 29 septembre 2022 a 17h45 pour les classes de premiere.
Vous recevrez tres prochainement le detail de ces rencontres.
A cette occasion, les deux associations de parents d'eleves seront presentes a partir de 18H45 pour vous
accueillir et repondre a vos questions : AAPE et FCPE.
Je souhaite enfin vous donner ou vous rappeler quelques informations importantes sur lesquelles votre appui et
votre soutien sont absolument indispensables :
1. La reussite scolaire de nos eleves et de vos enfants s'appuie sur une exigence d'assiduite, de serieux et de
regularite dans le travail. C'est a ce prix que nous pourrons, en juillet 2023, confirmer ou meme depasser
l'excellent 96,6 % de reussite obtenu au baccalaureat 2022.
2. L'etablissement fonctionne sur la base d'un reglement interieur (joint) qui s'impose a tous les eleves de
l'etablissement. Je compte sur eux pour en respecter les termes, source, comme l'an dernier, d'un climat
scolaire extremement propice a la reussite de tous. A ce sujet, je souhaite notamment rappeler l'exigence de
respect d'autrui, des personnes et des biens ainsi que le refus de toute violence, de tout harcelement et le
strict respect du principe de laïcite.
3. Avec la reforme du baccalaureat, l'evaluation des eleves sous la forme du contrôle continu represente 40 %
du resultat au baccalaureat. En consequence les eleves doivent etre informes que leur presence a tous les
contrôles est rigoureusement obligatoire et que toute absence devra donner lieu a une justification dont la
recevabilite sera acceptee ou non par le service de la vie scolaire. Une absence recevable pourra, si le
professeur le juge necessaire, donner lieu a un devoir de rattrapage le samedi matin. Une absence non
recevable a un devoir se traduira par la note de 0/20 a ce devoir.
4. Vos premiers interlocuteurs dans la scolarite de vos enfants sont les professeurs (professeurs principaux,
professeurs des differentes matieres). Je vous encourage, lorsque des questions se posent, a les contacter
directement via le logiciel Pronote qui est l'outil central des echanges pedagogiques. Toute interrogation ou
toute information concernant les conditions de la scolarite de vos enfants dans telle ou telle matiere sont
importantes. N'attendez pas pour prendre contact avec les professeurs.
5. Les conseillers principaux d'education et les assistants d'education assurent un contact permanent avec les
familles et les eleves. Leur rôle est primordial pour veiller a la bonne scolarite de vos enfants et ils assurent
cette mission avec beaucoup de serieux et d'engagement. Reservez-leur le meilleur accueil lorsqu'ils
prennent contact avec vous. C'est toujours dans l'interet du bon deroulement de la scolarite de votre enfant.
Voici leur adresse : Vie-scolaire1.0870015u@ac-limoges.fr
6. Une assistante sociale : marie-francoise.richard-majewski@ac-limoges.fr est presente au lycee deux jours par
semaine. Aucune famille ne doit hesiter a lui presenter d'eventuelles difficultes materielles ou financieres
afin de rechercher avec elle les solutions qui permettront de ne pas penaliser les eleves. L'anonymat du
traitement des dossiers est garanti, soyez en assures.

7. Les infirmieres de l'etablissement sont au service des eleves et de leurs familles afin de prendre en compte
les difficultes medicales qui peuvent survenir tout au long de l'annee. Voici leur adresse, n'hesitez pas a
prendre contact en cas de besoin : infirmerie.0870015u@ac-limoges.fr. Il est preferable d'utiliser
prioritairement cette adresse pour joindre le service infirmier du lycee (et non Pronote).
8. Les conseilleres psychologues sont egalement au service des eleves pour toute question liee a l'orientation
ou au vecu de la scolarite. Voici leurs adresses : cathy.murs@ac-limoges.fr et surya.antona@ac-limoges.fr. 
Pour le passage de seconde en premiere, de premiere en terminale et pour les choix lies a l'orientation vers
l'enseignement superieur, ce sont, avec les professeurs principaux, vos interlocutrices privilegiees.
9. Les services de l'intendance, pour toute question materielle ou financiere, sont joignables a cette adresse :
intendance.lyc-gaylussac@ac-limoges.fr.

10. Enfin, si vous souhaitez joindre l'equipe de direction de l'etablissement, merci d'ecrire a ce.0870015u@ac-limoges.fr. 

L'ensemble des personnels de l'etablissement, et j'y associe tous les personnels administratifs et techniques qui
offrent a nos eleves les meilleures conditions d'accueil, souhaite a vos enfants une excellente rentree 2022.

Bien cordialement et a bientôt pour d'autres informations,

M. Didier Leroy-Lusson
Proviseur.

Annuaire des services

Service Mail Telephone
Etablissement ce.0870015u@ac-limoges.fr 05 55 79 70 01
Proviseur et proviseur adjoint ce.0870015u@ac-limoges.fr 05 55 79 70 01
Secretariat de direction, partie CPGE Didier.beulle@ac-limoges.fr 05 55 79 33 23
Secretariat de direction, partie lycee ce.0870015u@ac-limoges.fr 05 55 79 02 63
Secretariat de la scolarite des eleves du lycee
(hors CPGE)
 Anne-marie.faucher@ac-limoges.fr 05 55 79 04 68
 Nathalie.dejoie@ac-limoges.fr 05 55 79 29 28
Intendance intendance.lyc-gaylussac@ac-limoges.fr. 05 55 79 01 07
CPE, vie scolaire Vie-scolaire1.0870015u@ac-limoges.fr 05 55 79 38 39
Infirmerie infirmerie.0870015u@ac-limoges.fr 05 55 79 07 54

Assistante sociale marie-francoise.richard-majewski@ac-limoges.fr 

Les eleves passent a la vie scolaire pour fixer un rendez-vous.
Sinon, N° du standard : 05 55 79 70 01

Psychologues de l'Education cathy.murs@ac-limoges.fr et surya.antona@ac-limoges.fr 

Les eleves passent a la vie scolaire pour fixer un
rendez-vous.

Sinon, N° du standard : 05 55 79 70 01";;

Printf.printf "\n--- Sigles:\n" ;;
let sigles = Str.regexp "[A-Z][A-Z]+" in
affiche (find_all sigles lettreRentree) ;;

Printf.printf "\n--- Numeros:\n" ;;
let numeros = Str.regexp "\\([0-9][0-9] \\)+[0-9][0-9]" in
affiche (find_all numeros lettreRentree) ;;

Printf.printf "\n--- Mails:\n" ;;
let mails = Str.regexp "[A-Za-z0-9-.]+@[A-Za-z0-9-]+.[a-z]+" in
affiche (find_all mails lettreRentree) ;;
