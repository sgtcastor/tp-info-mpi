######################## Mini-projet Mastermind ########################

###### import modules #######

from random import randint

##### Les variables globales #######

colors = '🔵🟢🟡🔴🟤⚫'
labels = 'BVJRMN'
l_lines = 4
nb_lines = 10

##### Les fonctions auxiliaires et tests #######


def convert(string):
    assert str(string) == string
    return_value = ''
    for char in string:
        idx = 0
        for label in labels:
            if char == label:
                return_value = return_value + colors[idx]
            idx += 1
    return return_value


def show():
    assert len(guess) == len(answers)
    for i in range(nb_lines - len(guess)):
        print('|' + '⚪'*l_lines + '|' + '·'*l_lines + '|')
    for i in range(len(guess)):
        print('|' + convert(guess[len(guess) -i-1]) + '|' + answers[len(guess) -i-1] + '|')
    print()

##### programme principal #######

guess = []
answers = []

solution = ''
for i in range(l_lines):
    solution = solution + labels[randint(0, len(labels)-1)]

ipt = None
while len(guess) < nb_lines and ipt != solution:
    valid_ipt = False
    while not valid_ipt:
        show()

        ipt = input('Next guess (' + labels + '): ')
        if len(ipt) == l_lines:
            valid_ipt = True
            for char in ipt:
                valid_char = False
                for label in labels:
                    valid_char = valid_char or char == label
                valid_ipt = valid_ipt and valid_char
    assert len(ipt) == l_lines
    guess += [ipt]

    match = 0
    mismatch = 0
    nomatch = 0
    for idx in range(l_lines):
        if ipt[idx] == solution[idx]:
            match += 1
        else:
            found = False
            for char in solution:
                found = found or ipt[idx] == char
            if found:
                mismatch += 1
            else:
                nomatch += 1
    assert match + mismatch + nomatch == l_lines
    answers += ['•'*match + '°'*mismatch + '·'*nomatch]

show()
if ipt == solution:
    print('MASTERMIND !')
else:
    print('La solution était: ' + solution)
