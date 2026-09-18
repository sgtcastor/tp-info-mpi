# --- Correction - TP05 - Récursivité - Exercice 1

# - Factorielle

def factIt(n):
    produit = 1
    for i in range(2, n+1):
        produit *= i
    return produit


assert (factIt(6) == 720)
assert (factIt(0) == 1)
assert (factIt(1) == 1)
assert (factIt(2) == 2)


def factRec(n):
    if n == 0:
        return 1
    else:
        return n * factRec(n-1)


assert (factRec(6) == 720)
assert (factRec(0) == 1)
assert (factRec(1) == 1)
assert (factRec(2) == 2)


# - palindrome

def palindromeIt(ch):
    for i in range(len(ch)//2):
        if ch[i] != ch[len(ch)-i-1]:
            return False
    return True


assert (palindromeIt("kayak") == True)
assert (palindromeIt("kayyak") == True)
assert (palindromeIt("abca") == False)
assert (palindromeIt("dabcad") == False)
assert (palindromeIt("a") == True)
assert (palindromeIt("") == True)


def palindromeRec(ch):
    if len(ch) < 2:
        return True
    elif ch[0] == ch[len(ch)-1]:
        return palindromeRec(ch[1:len(ch)-1])
    else:
        return False

assert (palindromeRec("kayak") == True)
assert (palindromeRec("kayyak") == True)
assert (palindromeRec("abca") == False)
assert (palindromeRec("dabcad") == False)
assert (palindromeRec("a") == True)
assert (palindromeRec("") == True)


# - maximum

def maximumIt(L):
    assert (len(L) > 0)
    maxi = L[0]
    for i in range(len(L)):
        if L[i] > maxi:
            maxi = L[i]
    return maxi


assert (maximumIt([1, 2, 5, 1, 3]) == 5)
assert (maximumIt([1, 2, 1, 1, 3]) == 3)
assert (maximumIt([11, 2, 1, 1, 3]) == 11)
assert (maximumIt([2]) == 2)
assert (maximumIt([2, 3, 3, 3, 2]) == 3)


def maximumRec(L):
    assert (len(L) > 0)
    if len(L) == 1:
        return L[0]
    else:
        maxi1 = maximumRec(L[0:len(L)//2])
        maxi2 = maximumRec(L[len(L)//2:len(L)])
        if maxi1 < maxi2:
            return maxi2
        return maxi1


assert (maximumRec([1, 2, 5, 1, 3]) == 5)
assert (maximumRec([1, 2, 1, 1, 3]) == 3)
assert (maximumRec([11, 2, 1, 1, 3]) == 11)
assert (maximumRec([2]) == 2)
assert (maximumRec([2, 3, 3, 3, 2]) == 3)


# - hanoi

def hanoi(n, depart, arrivee, inter):
    if n == 0:
        print('Aucun déplacement à faire')
    elif n == 1:
        print(depart, '->', arrivee)
    else:
        hanoi(n-1, depart, inter, arrivee)
        hanoi(1, depart, arrivee, inter)
        hanoi(n-1, inter, arrivee, depart)


# hanoi(4, 1, 3, 2)


# - Q6.

def palindromeRec(ch):
    def palindromeRec_aux(ch, debut, fin):
        if fin - debut < 2:
            return True
        elif ch[debut] == ch[fin-1]:
            return palindromeRec_aux(ch, debut+1, fin-1)
        else:
            return False
    return palindromeRec_aux(ch, 0, len(ch))


assert (palindromeRec("kayak") == True)
assert (palindromeRec("kayyak") == True)
assert (palindromeRec("abca") == False)
assert (palindromeRec("dabcad") == False)
assert (palindromeRec("a") == True)
assert (palindromeRec("") == True)


def maximumRec(L):
    def maximumRec_aux(L, debut, fin):
        if fin - debut == 1:
            return L[debut]
        else:
            maxi1 = maximumRec_aux(L, debut, (debut + fin)//2)
            maxi2 = maximumRec_aux(L, (debut + fin)//2, fin)
            if maxi1 < maxi2:
                return maxi2
            return maxi1
    assert (len(L) > 0)
    return maximumRec_aux(L, 0, len(L))


assert (maximumRec([1, 2, 5, 1, 3]) == 5)
assert (maximumRec([1, 2, 1, 1, 3]) == 3)
assert (maximumRec([11, 2, 1, 1, 3]) == 11)
assert (maximumRec([2]) == 2)
assert (maximumRec([2, 3, 3, 3, 2]) == 3)
