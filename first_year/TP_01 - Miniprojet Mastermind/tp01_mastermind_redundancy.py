# # -- Mini-projet Mastermind -- # #

# -- import modules -- #

from random import randint

# -- Les variables globales -- #

colors = '🔵🟢🟡🔴🟤⚫'
labels = 'BVJRMN'

l_lines = 4  # Longeur du code
nb_lines = 10  # Nombre d'essais
redundancy = False  # Autorisation de répéter les couleurs

# -- Les fonctions auxiliaires et tests -- #


def convert(string):
    """
    Convertit la chaine de caractères passée en arguments en cercles de
    couleurs correspondants, et la renvoie
    """
    assert str(string) == string
    return_value = ''
    for char in string:
        idx = 0
        for label in labels:
            if char == label:
                return_value = return_value + colors[idx]
            idx += 1
    return return_value


assert convert(labels) == colors
assert convert(labels[1]+labels[0]) == colors[1]+colors[0]


def show():
    """
    Affiche l'état actuel du jeu
    """
    assert len(guess) == len(answers)
    print('\n'*10)
    for i in range(nb_lines - len(guess)):
        print('|' + '⚪'*l_lines +
              '|' + '·'*l_lines +
              '|')
    for i in range(len(guess)):
        print('|' + convert(guess[len(guess) - i - 1]) +
              '|' + answers[len(guess) - i - 1] +
              '|')
    print()


# -- Programme principal -- #

guess = []
answers = []

solution = ''
if redundancy == True:
    for i in range(l_lines):
        solution = solution + labels[randint(0, len(labels)-1)]
else:
    ZtoN = []
    for n in range(len(labels)):
        ZtoN += [n]
    for i in range(l_lines):
        rndm = randint(0, len(ZtoN)-1)
        solution = solution + labels[ZtoN[rndm]]
        ZtoN = ZtoN[:rndm] + ZtoN[rndm+1:]

ipt = None
while len(guess) < nb_lines and ipt != solution:
    valid_ipt = False
    while not valid_ipt:
        show()

        ipt = input('Essai suivant (' + labels + '): ')
        if len(ipt) == l_lines:
            valid_ipt = True
            for char_idx in range(len(ipt)):
                valid_char = False
                for label in labels:
                    valid_char = valid_char or ipt[char_idx] == label
                for char_idx2 in range(len(ipt)):
                    valid_char = valid_char and not (
                        redundancy == False and
                        ipt[char_idx] == ipt[char_idx2] and
                        char_idx != char_idx2)
                valid_ipt = valid_ipt and valid_char
    assert len(ipt) == l_lines
    guess += [ipt]

    match = 0
    mismatch = 0
    nomatch = 0
    checkedguess = [False]*l_lines
    usedsolution = [False]*l_lines
    for idx in range(l_lines):  # Vérifie les matchs parfaits
        if ipt[idx] == solution[idx]:
            match += 1
            checkedguess[idx] = True
            usedsolution[idx] = True

    for ipt_idx in range(l_lines):  # Vérifie les potentiels matchs
        if checkedguess[ipt_idx] == False:
            found = False  # Teste chaque élément de la solution restant
            for sol_idx in range(l_lines):
                if ipt[ipt_idx] == solution[sol_idx] and usedsolution[sol_idx] == False and found == False:
                    found = True
                    mismatch += 1
                    usedsolution[sol_idx] = True

            if not found:
                nomatch += 1
            checkedguess[ipt_idx] = True

    assert match + mismatch + nomatch == l_lines
    answers += ['•'*match + '°'*mismatch + '·'*nomatch]

show()
if ipt == solution:
    print('MASTERMIND !')
else:
    print('La solution était: ' + convert(solution))
