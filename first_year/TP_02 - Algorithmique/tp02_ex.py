# -- TD02 - Algorithmique -- #

#########################
# Manipulations diverses
#########################

print("### Question 1")


def intersecte(a, b, c, d):
    """ les intervalles [a, b] et [c, d] ont-ils une intersection commune ? """
    assert (a <= b and c <= d)
    return a <= d and b >= c


assert (intersecte(0, 30, 12, 25) == True)
assert (intersecte(0, 30, 30, 31) == True)
assert (intersecte(10, 30, 9, 12) == True)
assert (intersecte(31, 35, 4, 12) == False)
assert (intersecte(31, 35, 44, 120) == False)

################

print("### Question 2")


def racTrinome(a, b, c):
    """ renvoie la liste des racines réelles de aX²+bX+c """
    assert (a != 0)
    delta = - c*a*4 + b**2
    if delta < 0:
        return []
    if delta == 0:
        return [(-b)/(a*2)]
    return [(-delta**0.5-b)/(a*2), (delta**0.5-b)/(a*2)]


assert (racTrinome(2, 4, 2) == [-1.0])
assert (racTrinome(1, -1, 0) == [0.0, 1.0] or racTrinome(1, -1, 0) == [1.0, 0.0])
assert (racTrinome(1, 1, 1) == [])

################

print("### Question 3")


def max3(a, b, c):
    """ renvoie la plus grande valeur des trois """
    maxi = a
    if a < b:
        maxi = b
    if maxi < c:
        maxi = c
    return maxi


assert (max3(4, 7, 2) == 7)
assert (max3(1, 0, 2) == 2)
assert (max3(7, 4, 1) == 7)
assert (max3(4, 5, 12) == 12)
assert (max3(1, 3, 2) == 3)

#################

print("### Question 4")


def deplace(x0, y0, mvts):
    for mvmt in mvts:
        if mvmt == 'H':
            y0 += 1
        elif mvmt == 'B':
            y0 -= 1
        elif mvmt == 'D':
            x0 += 1
        elif mvmt == 'G':
            x0 -= 1
    return x0, y0


assert (deplace(0, 0, "DHH") == (1, 2))
assert (deplace(-1, -1, "HDBG") == (-1, -1))
assert (deplace(2, 7, "HHGGBD") == (1, 8))
assert (deplace(10, 10, "") == (10, 10))

#################

print("### Question 5")

import random as rd


def devine(N):
    nb = rd.randint(0, N)
    ipt = -1
    while int(ipt) != nb:
        ipt = input("Deviner le nombre: ")
        if int(ipt) < nb:
            print("Ce nombre est trop petit")
        if int(ipt) > nb:
            print("Ce nombre est trop grand")
    return


# devine(1000)

#################
# Notes de Lola
#################
print("### Question 6")


def moyenne(Notes):
    """ calcule la moyenne des Notes """
    assert (len(Notes) > 0)
    total = 0
    for note in Notes:
        total += note
    return total/len(Notes)


assert (moyenne([12, 14, 13, 18, 13]) == 14)
assert (moyenne([12]) == 12)
assert (moyenne([17, 20]) == 18.5)

print("### Question 7")


def existeSupA(Notes, x):
    """ renvoie True si une des notes est sup ou egale à x, False sinon """
    for note in Notes:
        if note >= x:
            return True
    return False


assert (existeSupA([], 18) == False)
assert (existeSupA([18], 18) == True)
assert (existeSupA([14, 9, 11, 17], 18) == False)

print("### Question 8")


def compteSupA(Notes, x):
    """ compte le nombre de notes sup ou églaes à x """
    superieur = 0
    for note in Notes:
        if note >= x:
            superieur += 1
    return superieur


assert (compteSupA([14, 9, 11, 17], 3) == 4)
assert (compteSupA([14, 9, 11, 17], 15) == 1)
assert (compteSupA([14, 9, 11, 17], 17) == 1)
assert (compteSupA([14, 9, 11, 17], 20) == 0)

print("### Question 9")


def moyennes(Notes):
    """ renvoie la liste des moyennes après chaque devoir"""
    total = 0
    moyennes = []
    for i in range(len(Notes)):
        total += Notes[i]
        moyennes += [total/(i+1)]
    return moyennes


assert (moyennes([18, 0, 18]) == [18, 9, 12])
assert (moyennes([14, 14, 14, 14]) == [14, 14, 14, 14])
assert (moyennes([14, 10, 15, 15]) == [14, 12, 13, 13.5])
assert (moyennes([]) == [])


print("### Question 10")


def renverse(Notes):
    """ renvoie une nouvelle liste, dans l'ordre inverse """
    renversee = []
    for i in range(len(Notes)):
        renversee += [Notes[len(Notes)-1-i]]
    return renversee


assert (renverse([15, 16, 11, 13]) == [13, 11, 16, 15])
assert (renverse([15, 16, 11]) == [11, 16, 15])
assert (renverse([]) == [])


print("### Question 11")


def noteMaxi(Notes):
    """ renvoie la note maximale """
    assert (len(Notes) > 0)
    maxi = 0
    for note in Notes:
        if note > maxi:
            maxi = note
    return maxi


assert (noteMaxi([16, 12, 15]) == 16)
assert (noteMaxi([16]) == 16)
assert (noteMaxi([10, 11, 16, 15, 17, 11]) == 17)

print("### Question 12")


def noteMaxi2(Notes):
    """ renvoie les deux notes maximales """
    assert (len(Notes) > 1)
    maxi_1 = 0
    maxi_2 = 0
    for note in Notes:
        if note >= maxi_1:
            maxi_2 = maxi_1
            maxi_1 = note
        elif note >= maxi_2:
            maxi_2 = note
    return maxi_1, maxi_2


assert (noteMaxi2([16, 12, 15]) == (16, 15))
assert (noteMaxi2([15, 16]) == (16, 15))
assert (noteMaxi2([10, 11, 16, 15, 17, 11]) == (17, 16))
assert (noteMaxi2([10, 10, 10, 15, 17, 15, 17]) == (17, 17))


print("### Question 13")


def notesCroissantes(Notes):
    """ détermine si les notes sont croissantes """
    precedente = -1
    for note in Notes:
        if note < precedente:
            return False
        precedente = note
    return True


assert (notesCroissantes([16, 12, 15]) == False)
assert (notesCroissantes([15, 16]) == True)
assert (notesCroissantes([15]) == True)
assert (notesCroissantes([]) == True)
assert (notesCroissantes([15, 16, 17, 16]) == False)


#################
# Sauts de puces
#################

print("### Question 14")


def contact(L1, L2):
    """ les puces ont-elles été au meme endroit au meme moment ? """
    assert (len(L1) == len(L2))
    for idx in range(len(L1)):
        if L1[idx] == L2[idx]:
            return True
    return False


assert (contact([-5, 12, 17, 11, 24], [6, 4, 11, 18, 0]) == False)
assert (contact([-5, 12, 17, 11, 24], [6, 4, 18, 11, 0]) == True)
assert (contact([0], [0]) == True)
assert (contact([], []) == False)


print("### Question 15")


def caseCommune(L1, L2):
    """ les puces ont-elles été au meme endroit ? """
    assert (len(L1) == len(L2))
    for pos1 in L1:
        for pos2 in L2:
            if pos1 == pos2:
                return True
    return False


assert (caseCommune([-5, 12, 17, 11, 24], [6, 4, 11, 18, 0]) == True)
assert (caseCommune([-5, 12, 17, 11, 24], [6, 4, 18, 10, 11]) == True)
assert (caseCommune([0], [0]) == True)
assert (caseCommune([], []) == False)
assert (caseCommune([-5, 12, 17, 11, 24], [6, 4, 13, 18, 0]) == False)

print("### Question 16")


def retourCase(L):
    """ la puce passe-t-elle deux fois par la même case ? """
    for idx1 in range(len(L)):
        for idx2 in range(idx1+1, len(L)):
            if L[idx1] == L[idx2]:
                return True
    return False


assert (retourCase([-5, 12, 17, 11, 24]) == False)
assert (retourCase([-5, 12, 17, 11, 11]) == True)
assert (retourCase([0, 0]) == True)
assert (retourCase([0]) == False)
assert (retourCase([]) == False)
assert (retourCase([0, 1, 0, 1, 0, 1]) == True)

print("### Question 17")


def distanceMin(L1, L2):
    """ renvoie l'instant auquel les puces ont été les plus proches """
    assert (len(L1) == len(L2))
    assert (len(L1) > 0)
    mini = abs(L1[0] - L2[0])
    for idx in range(1, len(L1)):
        if abs(L1[idx] - L2[idx]) < mini:
            mini = abs(L1[idx] - L2[idx])
    return mini


assert (distanceMin([11], [11]) == 0)
assert (distanceMin([11], [-11]) == 22)
assert (distanceMin([4, 11, 8], [11, 11, 3]) == 0)
assert (distanceMin([-5, 12, 17, 11, 24], [6, 4, 11, 18, 0]) == 6)
assert (distanceMin([-5, 12, 17, 11, 24], [6, 4, 18, 11, 10]) == 0)
assert (distanceMin([-5, 12, 17, 11, 24], [6, 4, 13, 18, 0]) == 4)


print("### Question 18")

# deux parcours
P1 = [(1800, 0), (2050, 55), (1900, 82), (2200, 142), (1750, 221), (1800, 240)]
P2 = [(1500, 0), (1950, 92), (1900, 107), (2553, 146), (1670, 212), (1800, 282)]


def maxmin(Parcours):
    """ renvoie les altitudes maximale et minimale """
    assert (len(Parcours) > 0)
    maxi = mini = Parcours[0][0]
    for elt in Parcours[1:]:
        altitude = elt[0]
        if altitude < mini:
            mini = altitude
        elif altitude > maxi:
            maxi = altitude
    return maxi, mini


assert (maxmin(P1) == (2200, 1750))
assert (maxmin(P2) == (2553, 1500))

print("### Question 19")


def deniv(Parcours):
    """ renvoie les dénivelés positifs et négatifs """
    montee = descente = 0
    precedent = Parcours[0][0]
    for elt in Parcours[1:]:
        altitude = elt[0]
        if altitude < precedent:
            descente += altitude - precedent
        elif altitude > precedent:
            montee += altitude - precedent
        precedent = altitude
    return montee, descente


assert (deniv(P1) == (600, -600))
assert (deniv(P2) == (1233, -933))


print("### Question 20")


def parcoursInverse(Parcours):
    """ renvoie le parcours en sens inverse """
    parcours_inv = [0]*len(Parcours)
    tmps_total = 0  # stocke le temps total depuis le début du parcours inverse
    tmps_precedent = Parcours[len(Parcours)-1][1]  # stocke le temps depuis le dernier element de la liste
    parcours_inv[0] = Parcours[len(Parcours)-1][0], 0
    for i in range(len(Parcours)-1):
        elt = Parcours[len(Parcours)-2-i]  # prends la liste à l'envers, sauf le dernier element
        parcours_inv[i+1] = elt[0], tmps_precedent - elt[1] + tmps_total
        tmps_total += tmps_precedent - elt[1]
        tmps_precedent = elt[1]
    return parcours_inv


assert (parcoursInverse(P1) == [(1800, 0), (1750, 19), (2200, 98), (1900, 158), (2050, 185), (1800, 240)])
assert (parcoursInverse(P2) == [(1800, 0), (1670, 70), (2553, 136), (1900, 175), (1950, 190), (1500, 282)])
