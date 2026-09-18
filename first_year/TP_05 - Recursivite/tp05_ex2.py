# --- TP05 - Récursivité - Exercice 2

import time


# - Fibo naif

def fibo(n):
    global count
    count += 1
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibo(n-2) + fibo(n-1)


"""
count = 0
t1 = time.time()
z = fibo(40)
assert (z == 102334155)
t2 = time.time()
print("temps de calcul pour fibo(40)", t2-t1)
print("Nombre d'appel à la fonction fibo avec fibo(36)", count)
"""


# - Fibo avec memoisation

def fibo_memo(memo, n):
    if memo[n] != -1:
        return memo[n]
    else:
        memo[n] = fibo_memo(memo, n-2) + fibo_memo(memo, n-1)
        return memo[n]


def fibo2(n):
    memo = [-1]*(n+1)
    memo[0], memo[1] = 0, 1
    return fibo_memo(memo, n)


t1 = time.time()
z = fibo2(40)
assert (z == 102334155)
t2 = time.time()
print("temps de calcul pour fibo2(40)", t2-t1)


# - Fibo en programmation dynamique

def fibo3(n):
    if n < 2:
        return n
    u = [0]*(n+1)
    u[0], u[1] = 0, 1
    for i in range(2, n+1):
        u[i] = u[i-2] + u[i-1]
    return u[n]


t1 = time.time()
z = fibo3(40)
assert z == 102334155
t2 = time.time()
print("temps de calcul pour fibo3(40)", t2-t1)


# - Fibo en programmation dynamique 2

def fibo4(n):
    if n < 2:
        return n
    u0, u1 = 0, 1
    for i in range(2, n+1):
        u0, u1 = u1, u0 + u1
    return u1


t1 = time.time()
z = fibo4(40)
assert z == 102334155
t2 = time.time()
print("temps de calcul pour fibo4(40)", t2-t1)
