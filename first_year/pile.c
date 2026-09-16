// pile.c

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>


/* Implémentation d'une pile par maillons chainés */

struct s_maillon {
    int val;    // valeur du maillon
    struct s_maillon* suivant; // maillon suivant
};
typedef struct s_maillon Maillon; // renommage

typedef struct {
    Maillon* sommet; // sommet de pile. NULL pour la pile vide
} Pile;


// Opérations sur les piles

Pile* creerPile(){
   Pile* pile = malloc(sizeof(Pile));
   pile->sommet = NULL;
   return pile;
}


void detruireChaine(Maillon* m) {
    if (m != NULL){
        detruireChaine(m->suivant);
        free(m);
    }
}


void detruirePile(Pile* pile){
    assert(pile != NULL);
    detruireChaine(pile->sommet);
    free(pile);
}


void detruirePileIt(Pile* pile){
    Maillon* suivant;
    for (Maillon* ptr = pile->sommet; ptr != NULL; ptr = suivant){
        suivant = ptr->suivant;
        free(ptr);
    }
}


void afficherPile(Pile* pile){ // affiche les éléments de la pile (sans dépiler)
    printf("[SOMMET:");
    for (Maillon* ptr = pile->sommet; ptr != NULL; ptr = ptr->suivant){
        printf("%d,", ptr->val);
    }
    printf("]\n");
}


void empiler(Pile* pile, int val){
    Maillon* new = malloc(sizeof(Maillon));
    new->suivant = pile->sommet;
    new->val = val;
    pile->sommet = new;
}


bool estVide(Pile* pile){
    return pile->sommet == NULL;
}


int depiler(Pile* pile) {
    Maillon* new = pile->sommet->suivant;
    int val = pile->sommet->val;
    free(pile->sommet);
    pile->sommet = new;
    return val;
}


void testPile() {
    printf("testPile ...\n");
    Pile* p = creerPile();
    assert(estVide(p));
    for (int i = 5; i < 10; i+=1){
        empiler(p,i);
    }
    afficherPile(p);
    assert(depiler(p) == 9);
    assert(depiler(p) == 8);
    assert(depiler(p) == 7);
    assert(depiler(p) == 6);
    assert(depiler(p) == 5);
    assert(estVide(p));
    detruirePileIt(p);
    printf("testPile OK\n");
}


/* Exercice */

bool parenthesesOK(char text[]){
    Pile* p = creerPile();
    for (int i = 0; text[i] != '\0'; i++){
        if (text[i] == '('){
            empiler(p, i);
        } else if (text[i] == ')' && text[depiler(p)] != '('){
            bonus(text, i);
            return false;
        } else if (text[i] == '['){
            empiler(p, i);
        } else if (text[i] == ']' && text[depiler(p)] != '['){
            bonus(text, i);
            return false;
        } else if (text[i] == '{'){
            empiler(p, i);
        } else if (text[i] == '}' && text[depiler(p)] != '{'){
            bonus(text, i);
            return false;
        }
    }
    if (estVide(p)){
        return true;
    }
    printf("fin du texte atteinte lors du parsing: %s\n", text);
    return false;
}


void bonus(char* text, int i){
    printf("erreur à l'index %d:\n%s\n", i, text);
    for (int j = 0; text[j] != '\0'; j++){
        if (j == i){
            printf("^");
        } else {
            printf(" ");
        }
    }
    printf("\n");
}


void testparenthesesOK(){
    printf("testparenthesesOK ...\n");
    assert(parenthesesOK("") == true);
    assert(parenthesesOK("a") == true);
    assert(parenthesesOK("a(aa)") == true);
    assert(parenthesesOK("{a(a)}[a]a") == true);
    assert(parenthesesOK("aaa(aa") == false);
    assert(parenthesesOK("aaa[aa)a") == false);
    assert(parenthesesOK("a[aa(]aa)") == false);
    printf("testparenthesesOK OK\n");
}



/* Programme principal */

int main(){
    printf("Piles en C\n");
    Pile* p = creerPile();
    detruirePileIt(p);
    testPile();

    testparenthesesOK();
    return 0;
}
