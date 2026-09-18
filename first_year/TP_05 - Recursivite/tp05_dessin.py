# -- TP05 - Dessins r�cursifs

import matplotlib.pyplot as plt
import numpy as np
import math as m


def segment(xa, ya, r, theta):
    """ trace le segment partant du point (xa, ya)
    de longueur r, dans la direction theta"""
    xb = xa + r*m.cos(theta)
    yb = ya + r*m.sin(theta)
    plt.plot([xa, xb], [ya, yb], "b")


def cercle(x, y, r):
    """ trace le cercle de centre (x, y) et de rayon r"""
    t = np.linspace(0, 2*np.pi, 100)
    X = x + r*np.cos(t)
    Y = y + r*np.sin(t)
    plt.plot(X, Y, "b")


def carreNoir(xa, ya, c):
    """ trace le carr� noir de c�t� c et dont le coin
    inf�rieur gauche est en (xa, ya)"""
    plt.fill([xa, xa+c, xa+c, xa], [ya, ya, ya+c, ya+c], "k")


def carreBlanc(xa, ya, c):
    """ trace le carr� blanc de c�t� c et dont le coin
    inf�rieur gauche est en (xa, ya)"""
    plt.fill([xa, xa+c, xa+c, xa], [ya, ya, ya+c, ya+c], "w")


# - Exemple d'utilisation

"""
cercle(0, 0, 1)
carreNoir(-1, -1, 1)
carreBlanc(-0.7, -0.7, 0.5)

plt.axis("equal")
plt.show()
"""


# - Q2

def traceCercles1(n, x, y, r):
    cercle(x, y, r)
    if n > 1:
        traceCercles1(n-1, x + 1.5*r, y, r/2)


"""
traceCercles1(4, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


# - Q3

def traceCercles2(n, x, y, r):
    cercle(x, y, r)
    if n > 1:
        traceCercles2(n-1, x + 1.5*r, y, r/2)
        traceCercles2(n-1, x, y - 1.5*r, r/2)


"""
traceCercles2(4, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


# - Q4

def traceCercles4(n, x, y, r):
    def traceCercles3(n, x, y, r, vide):
        cercle(x, y, r)
        if n > 1:
            if vide != 'nord':
                traceCercles3(n-1, x, y + 1.5*r, r/2, 'sud')
            if vide != 'sud':
                traceCercles3(n-1, x, y - 1.5*r, r/2, 'nord')
            if vide != 'est':
                traceCercles3(n-1, x + 1.5*r, y, r/2, 'ouest')
            if vide != 'ouest':
                traceCercles3(n-1, x - 1.5*r, y, r/2, 'est')
    cercle(x, y, r)
    traceCercles3(n-1, x, y + 1.5*r, r/2, 'sud')
    traceCercles3(n-1, x, y - 1.5*r, r/2, 'nord')
    traceCercles3(n-1, x + 1.5*r, y, r/2, 'ouest')
    traceCercles3(n-1, x - 1.5*r, y, r/2, 'est')


"""
traceCercles4(4, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


# - Q5


def tapis(n, xa, ya, c):
    if n == 0:
        carreNoir(xa, ya, c)
    else:
        carreBlanc(xa + c/3, ya + c/3, c/3)
        tapis(n-1, xa + 2*c/3, ya + 2*c/3, c/3)
        tapis(n-1, xa + 1*c/3, ya + 2*c/3, c/3)
        tapis(n-1, xa + 0*c/3, ya + 2*c/3, c/3)
        tapis(n-1, xa + 2*c/3, ya + 1*c/3, c/3)
        #
        tapis(n-1, xa + 0*c/3, ya + 1*c/3, c/3)
        tapis(n-1, xa + 2*c/3, ya + 0*c/3, c/3)
        tapis(n-1, xa + 1*c/3, ya + 0*c/3, c/3)
        tapis(n-1, xa + 0*c/3, ya + 0*c/3, c/3)


"""
tapis(3, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


# - Q6

def flocon(n, xa, ya, r):
    def flocon_aux(n, xa, ya, r, theta):
        if n == 0:
            segment(xa, ya, r, theta)
        else:
            flocon_aux(n-1, xa, ya, r/3, theta)
            flocon_aux(n-1,
                       xa + m.cos(theta)*r/3,
                       ya + m.sin(theta)*r/3,
                       r/3, theta - m.pi/3)
            flocon_aux(n-1,
                       xa + m.cos(theta)*r/3 + m.cos(theta - m.pi/3)*r/3,
                       ya + m.sin(theta)*r/3 + m.sin(theta - m.pi/3)*r/3,
                       r/3, theta + m.pi/3)
            flocon_aux(n-1,
                       xa + 2/3*(m.cos(theta)*r),
                       ya + 2/3*(m.sin(theta)*r),
                       r/3, theta)
    flocon_aux(n, xa, ya, r, 0)
    flocon_aux(n, xa + r, ya, r, 2*m.pi/3)
    flocon_aux(n,
               xa + m.cos(m.pi/3)*r,
               ya + m.sin(m.pi/3)*r,
               r, -2*m.pi/3)


"""
flocon(3, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


# - Q7

def fonction_init(n, xa, ya, c):
    def fonction_pos(n, xa, ya, c, color, special):
        if n > 0:
            if color == 1:
                carreBlanc(xa + padding, ya + padding, c - padding)
            else:
                carreNoir(xa + padding, ya + padding, c - padding)
            fonction_pos(n-1, xa + c, ya, c/2, color, False)
            fonction_pos(n-1, xa, ya + c, c/2, color, False)
            fonction_neg(n-1, xa + c/2, ya + c/2, c/2,
                         (1, 0)[color], True)
            if special:
                fonction_neg(n-1, xa + 3*c/2, ya + 3*c/2, c/2, color, False)

    def fonction_neg(n, xa, ya, c, color, special):
        if n > 0:
            if color == 1:
                carreBlanc(xa, ya, c - padding)
            else:
                carreNoir(xa, ya, c - padding)
            fonction_neg(n-1, xa - c/2, ya + c/2, c/2, color, False)
            fonction_neg(n-1, xa + c/2, ya - c/2, c/2, color, False)
            fonction_pos(n-1, xa, ya, c/2, (1, 0)[color], True)
            if special:
                fonction_pos(n-1, xa - c, ya - c, c/2, color, False)

    carreNoir(xa, ya, c)
    padding = c/198
    fonction_pos(n, xa, ya, c/2, 1, True)


"""
fonction_init(6, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


def sierpinski(n, xa, ya, c):
    if n == 0:
        segment(xa, ya, c, 0)
        segment(xa + c, ya, c, 2*m.pi/3)
        segment(xa + m.cos(m.pi/3)*c,
                ya + m.sin(m.pi/3)*c,
                c, -2*m.pi/3)
    else:
        sierpinski(n-1, xa, ya, c/2)
        sierpinski(n-1, xa + c/2, ya, c/2)
        sierpinski(n-1, xa + m.cos(m.pi/3)*c/2, ya + m.sin(m.pi/3)*c/2, c/2)


"""
sierpinski(6, 0, 0, 1)
plt.axis("equal")
plt.show()
"""


def menger(n, xa, ya, c):
    def triangle(n, xa, ya, c, theta, symmetrie):
        if n > 0:
            segment(xa + m.cos(theta)*c/3, ya + m.sin(theta)*c/3, c/3, theta)
            segment(xa + 2*m.cos(theta)*c/3, ya + 2*m.sin(theta)*c/3,
                    c/3, theta + 2*m.pi/3)
            segment(xa + m.cos(theta)*c/3, ya + m.sin(theta)*c/3,
                    c/3, theta + m.pi/3)

            if symmetrie:
                triangle(n-1,
                         xa + m.cos(theta)*c/3 + m.cos(theta + m.pi/3)*c/3,
                         ya + m.sin(theta)*c/3 + m.sin(theta + m.pi/3)*c/3,
                         c/3, theta - 2*m.pi/3, symmetrie)  # 1, 0
            else:
                triangle(n-1, xa + 2*m.cos(theta)*c/3, ya + 2*m.sin(theta)*c/3,
                         c/3, theta + 2*m.pi/3, symmetrie)  # 1, 0

            triangle(n-1, xa, ya, c/3, theta, symmetrie)  # 0, 0
            triangle(n-1, xa + m.cos(theta + m.pi/3)*c/3,
                     ya + m.sin(theta + m.pi/3)*c/3,
                     c/3, theta, symmetrie)  # 0, 1
            triangle(n-1, xa + 2*m.cos(theta + m.pi/3)*c/3,
                     ya + 2*m.sin(theta + m.pi/3)*c/3,
                     c/3, theta, symmetrie)  # 0, 2
            triangle(n-1, xa + 2*m.cos(theta)*c/3,
                     ya + 2*m.sin(theta)*c/3, c/3, theta, symmetrie)  # 2, 0
            triangle(n-1, xa + m.cos(theta)*c/3 + m.cos(theta + m.pi/3)*c/3,
                     ya + m.sin(theta)*c/3 + m.sin(theta + m.pi/3)*c/3,
                     c/3, theta, symmetrie)  # 1, 1

            # inverses
            triangle(n-1, xa + m.cos(theta)*c/3 + m.cos(theta + m.pi/3)*c/3,
                     ya + m.sin(theta)*c/3 + m.sin(theta + m.pi/3)*c/3,
                     c/3, theta + m.pi, not symmetrie)  # 0, 0
            triangle(n-1, xa + 2*m.cos(theta)*c/3 + m.cos(theta + m.pi/3)*c/3,
                     ya + 2*m.sin(theta)*c/3 + m.sin(theta + m.pi/3)*c/3,
                     c/3, theta + m.pi, not symmetrie)  # 1, 0
            triangle(n-1, xa + m.cos(theta)*c/3 + 2*m.cos(theta + m.pi/3)*c/3,
                     ya + m.sin(theta)*c/3 + 2*m.sin(theta + m.pi/3)*c/3,
                     c/3, theta + m.pi, not symmetrie)  # 0, 1

    for i in range(3):
        theta = i * 2*m.pi/3 + m.pi/6
        segment(xa, ya, c, theta)
        segment(xa + m.cos(theta)*c, ya + m.sin(theta)*c, c, theta + 2*m.pi/3)
        segment(xa + m.cos(theta)*c, ya + m.sin(theta)*c, c, theta - 2*m.pi/3)

    triangle(n, xa - m.cos(m.pi/6)*c, ya - m.sin(m.pi/6)*c,
             c, m.pi/6, False)  # milieu gauche
    triangle(n, xa, ya + c, c, -m.pi/2, False)  # haut droite
    triangle(n, xa - m.cos(5*m.pi/6), ya - m.sin(5*m.pi/6),
             c, 5*m.pi/6, False)  # bas droite

    triangle(n, xa, ya, c, -m.pi/6, True)  # milieu droite
    triangle(n, xa, ya, c, m.pi/2, True)  # haut gauche
    triangle(n, xa, ya, c, -5*m.pi/6, True)  # bas gauche


# """
menger(3, 0, 0, 1)
plt.axis("equal")
plt.show()
# """
