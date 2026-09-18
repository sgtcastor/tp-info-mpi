# -- TP03 - Complexité Temporelle -- #

#########################
# Evaluation d'un polynome particulier
#########################

import time


print("### Question 3")


def eval1_P(x):
    """
    eval1_P(x:float) -> float
    evaluation classique de P : avec additions et multiplications uniquement
    """
    return -4*x*x*x*x + 3*x*x*x + 2*x*x + 8*x + 1


def eval2_P(x):
    """
    eval2_P(x:float) -> float
    evaluation astucieuse de P : avec additions et multiplications uniquement
    """
    return 1 + x*(8 + x*(2 + x*(3 - x*4)))


print("Test eval2_P", eval1_P(0.5))  # résultat attendu 5.625
assert (eval1_P(0.5) == 5.625)
assert (eval2_P(0.5) == eval1_P(0.5))
assert (eval2_P(9.0) == eval1_P(9.0))
assert (eval2_P(-107.25) == eval1_P(-107.25))


print("### Question 4")

import random as rd


def generateurAlea(nb, a, b):
    """ generateurAlea(nb:int, a:float, b:float) -> [float]
    génère nb valeurs aléatoires entre a et b """
    liste = [0]*nb
    for i in range(nb):
        liste[i] = a + rd.random()*(b - a)
    return liste


print("Test generateurAlea", generateurAlea(5, -2, 2))


print("### Question 5")


def testPerf(nb):
    liste = generateurAlea(nb, -10, 10)
    t1 = time.time()
    for i in liste:
        eval1_P(i)
    t2 = time.time()
    print("Exécution eval1 pour", nb, "éléments: ", t2-t1)
    t1 = time.time()
    for i in liste:
        eval2_P(i)
    t2 = time.time()
    print("Exécution eval2 pour", nb, "éléments: ", t2-t1)
    return


"""
testPerf(10000)
testPerf(100000)
testPerf(1000000)
testPerf(10000000)
"""


#########################
# Evaluation d'un polynome en général
#########################


print("### Question 8")


def evalNaif(L, x):
    """ evalNaif(L:[float], x:float) -> float
    evaluation classique : avec additions et multiplications uniquement
    """
    assert len(L) > 0
    somme = 0
    for i in range(len(L)):
        produit = 1
        for j in range(i):
            produit *= x
        somme += L[i] * produit
    return somme


assert (evalNaif([1, 8, 2, 3, -4], 0.5) == eval1_P(0.5))
assert (evalNaif([1, 8, 2, 3, -4], -107) == eval1_P(-107))


print("### Question 9")


def evalHorner(L, x):
    """ evalHorner(L:[float], x:float)->float
    evaluation astucieuse : avec additions et multiplications uniquement
    """
    assert len(L) > 0
    resultat = L[len(L)-1]
    for i in range(1, len(L)):
        resultat *= x
        resultat += L[len(L)-i-1]
    return resultat


assert (evalHorner([1, 8, 2, 3, -4], 0.5) == eval1_P(0.5))
assert (evalHorner([1, 8, 2, 3, -4], -107) == eval1_P(-107))


#########################
# Complexité asymptotique
#########################


print("### Question 11")


def testPerf2(L: [float], nb: int):
    """ L:la liste représentant le polynome   nb:nombre de valeurs """
    liste = generateurAlea(nb, -10, 10)
    t1 = time.time()
    for i in liste:
        evalNaif(L, i)
    t2 = time.time()
    print("Exécution evalnaif pour", len(L)-1, "degrés: ", t2-t1)
    t1 = time.time()
    for i in liste:
        evalHorner(L, i)
    t2 = time.time()
    print("Exécution evalHorner pour", len(L)-1, "degrés: ", t2-t1)
    return


print("### Question 12")

P4 = [3, 6, 2, 7, 4]  # 4 X^4 + 7 X^3 + 2 X ^ 2 + 6 X + 3

P8 = [1, 8, 2, 3, -4, 7, 1, 11, -2]
# -2 X^8 + 11 X^7 + X^6 + 7 X^5 - 4 X^4 + 3 X^3 + 2 X ^ 2 + 8 X + 1

P16 = [1, 8, 2, 3, -4, 7, 1, 11, -2, 8, 2, 3, -4, 7, 1, 11, -2]
# -2 X^16 + 11 X^15 + X^14 + 7 X^13 - 4 X^12 + 3 X^11 + 2 X ^ 10 + 8 X^9
# -2 X^8 + 11 X^7 + X^6 + 7 X^5 - 4 X^4 + 3 X^3 + 2 X ^ 2 + 8 X + 1

P32 = [1, 8, 2, 3, -4, 7, 1, 11, -2, 8, 2, 3, -4, 7, 1, 11, -2, 8, 2, 3, -4,
       7, 1, 11, -2, 8, 2, 3, -4, 7, 1, 11, -2]
# ....


# """
testPerf2(P4, 100000)
testPerf2(P8, 100000)
testPerf2(P16, 100000)
testPerf2(P32, 100000)
# """


#########################
# Exemple : calcul de puissance
#########################


def puiss(x, n):
    """ puiss(x:float, n:int) -> float """
    res = 1
    for i in range(n):
        res = res * x
    return res


def puissRapide(x, n):
    """ puissRapide(x:float, n:int) -> float """
    res = 1
    while (n > 0):
        if (n % 2 == 1):
            res = res*x
            n -= 1
        x = x*x
        n = n//2
    return res


assert (puiss(7, 5) == puissRapide(7, 5))
assert (puiss(7, 19) == puissRapide(7, 19))


print("### Question 16")


def testPerf3(n, nb):
    """ n : exposant,   nb : nombre de valeurs """
    # A COMPLETER
    return

# A COMPLETER
