# ---------------------- Mini-projet Mastermind ---------------------- #

# -- import de modules

import random as rd

# -- constantes du programme

NC = 8   # nombre de couleurs,  de 0 à NC-1. NC est une couleur invalide.
NT = 12  # nombre d'essais maximal
LC = 4   # longueur de la combinaison


# -- fonctions

def creeCombinaison():
    """ cree une combinaision aleatoire. renvoie cette combinaison """
    Cc = [NC]*LC  # NC valeur invalide
    for i in range(LC):
        Cc[i] = rd.randint(0, NC-1)
    return Cc


def choixCombinaisonHumain():
    """ demande à l'utilisateur une combinaison. renvoie cette combinaison """
    Cj = [NC]*LC
    for i in range(LC):
        x = int(input('Entrez un nombre entre 0 et '+str(NC-1)+' :\n'))
        assert (x >= 0 and x < NC)  # programmation défensive
        Cj[i] = x
    return Cj


def choixCombinaisonIA(wFc):
    """ demande à l'IA une combinaison. renvoie cette combinaison """
    Cj = [NC]*LC
    for i in range(LC):
        x = wFc[i][rd.randint(0, NC-1)]
        while x == -1:
            x = wFc[i][rd.randint(0, NC-1)]
        Cj[i] = x
    return Cj


def calculReponse(Cc, Cj):
    """ compare la combinaision cachée à la combinaison proposée par le joueur.
    renvoie le nombre de bienPlaces,  et le nombre de bonnesCouleurs """
    assert len(Cc) == len(Cj)  # programmation défensive
    # on travaille sur une copie de Cc et Cj
    # on remplacera par NC tous les pions comptabilisés
    N = len(Cc)
    Cc2 = [NC]*N
    Cj2 = [NC]*N
    for i in range(N):
        Cc2[i] = Cc[i]
        Cj2[i] = Cj[i]
    # on compte les bien places,  et on les remplace par NC dans Cc2 et Cj2
    nbP = 0
    for i in range(N):
        if (Cc2[i] == Cj2[i]):  # bp trouvé
            nbP = nbP + 1
            Cc2[i] = NC
            Cj2[i] = NC
    # on compte les bonnes couleurs,  et on les remplace par NC dans Cc2 et Cj2
    nbC = 0
    for i in range(N):
        if (Cj2[i] != NC):
            # on cherche si Cj2[i] est dans la liste Cc2
            for j in range(N):
                if Cc2[j] == Cj2[i]:  # bc trouvé
                    nbC = nbC + 1
                    Cj2[i] = NC
                    Cc2[j] = NC
                    break  # on ne cherche pas plus loin
    assert nbP + nbC <= N  # programmation défensive
    return (nbP, nbC)


# Quelques tests

assert (calculReponse([1, 4, 6, 2],  [1, 4, 6, 2]) == (4, 0))
assert (calculReponse([1, 4, 6, 2],  [1, 0, 6, 2]) == (3, 0))
assert (calculReponse([1, 4, 6, 2],  [1, 0, 6, 0]) == (2, 0))
assert (calculReponse([1, 4, 6, 2],  [3, 0, 5, 1]) == (0, 1))
assert (calculReponse([1, 4, 6, 2],  [3, 1, 5, 1]) == (0, 1))
assert (calculReponse([1, 4, 6, 2],  [0, 1, 1, 1]) == (0, 1))
assert (calculReponse([1, 4, 6, 2],  [1, 1, 1, 1]) == (1, 0))
assert (calculReponse([1, 4, 6, 2],  [2, 2, 1, 1]) == (0, 2))
assert (calculReponse([1, 4, 6, 4],  [4, 4, 6, 6]) == (2, 1))
assert (calculReponse([1, 4, 6, 4],  [4, 4, 4, 6]) == (1, 2))


# ----- fonctions principales

def mastermindHumain():
    # initialisation
    CC = creeCombinaison()
    print("Combinaison cachée", CC)

    compt = 0  # compteur de coups
    victoire = False  #

    # déroulement du jeu
    while compt < NT and not victoire:
        compt += 1

        print("Choisissez votre combinaison...")
        CJ = choixCombinaisonHumain()
        (nbp, nbc) = calculReponse(CC, CJ)

        print("Tentative", compt, CJ, " : il y a ", nbp, "bien Placé(s), ",
              nbc, "bonne(s) Couleur(s)")

        if nbp == LC:
            victoire = True

    # affichage du résultat
    if victoire:
        print("BRAVO ! Vous avez gagne en", compt, "coups")
    else:
        print("PERDU ... la combinaison était", CC)


def mastermindIA():
    # initialisation
    CC = creeCombinaison()
    print("Combinaison cachée",  CC)

    compt = 0  # compteur de coups
    victoire = False  #
    total_comb = [[0]*LC]*LC**NC
    nvlle_comb = [0]*LC
    for i in range(LC**NC):
        for j in range(4):
            total_comb[i][j] = nvlle_comb[j]
        print(nvlle_comb)
        idx = 0
        nvlle_comb[idx] += 1
        while nvlle_comb[idx] == NC and idx < LC-1:
            nvlle_comb[idx] = 0
            if idx < LC-1:
                idx += 1
                nvlle_comb[idx] += 1
    print('ttcomb',total_comb[:10])

    # déroulement du jeu
    while compt < NT and not victoire:
        compt += 1

        print("Choisissez votre combinaison...")
        CJ = choixCombinaisonIA(total_comb)
        (nbp, nbc) = calculReponse(CC, CJ)

        print("Tentative", compt, CJ, " : il y a ", nbp, "bien Placé(s),  ",
              nbc, "bonne(s) Couleur(s)")

        if nbp == LC:
            victoire = True

    # affichage du résultat
    if victoire:
        print("BRAVO ! Vous avez gagne en",  compt,  "coups")
    else:
        print("PERDU ... la combinaison était",  CC)


# C'est parti !

# ipt = input("Qui joue ? (Humain/Machine)(H/M): ")
ipt = 'M'
if ipt == 'Humain' or ipt == 'H':
    mastermindHumain()
elif ipt == 'Machine' or ipt == 'M':
    mastermindIA()
