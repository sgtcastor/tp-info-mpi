/******  Backtracking dominos ******/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Domino */
struct domino_s {
    int x; // valeur de gauche
    int y; // valeur de droite
};

typedef struct domino_s Domino;

/* Maillon */
struct element_s {
    Domino d;
    struct element_s* suiv;
};

typedef struct element_s element;

/* Chaine */
typedef element* chaine;


/**** Partie 1 ****/

/* Affiche les dominos d'une liste */
void afficheDominos(element* L){
    for (element* l = L; l != NULL; l = l->suiv) {
        printf("[%d|%d] ", l->d.x, l->d.y);
    }
}

/* Alloue un maillon dans le tas, et l'ajoute en tete de liste */
element* ajouteTete(element* L, Domino d){
    element* elt = (element*) malloc(sizeof(element));
    elt->d = d; elt->suiv = L;
    return elt;
}


/* Q1 */
Domino tete(element* L){
    assert (L != NULL);
    return L->d;
}

/* Q2 */
element* retireTete(element* L){
    assert (L != NULL);
    element* LL = L->suiv;
    free(L);
    return LL;
}

/* Q3 */
void detruireListe(element* L){
    element* suiv;
    for (element* l = L; l != NULL; l = suiv){
        suiv = l->suiv;
        free(l);
    }
}


/**** Partie 2 ****/

/* Q4 */
bool possible(Domino di, Domino dj){return di.y == dj.x;}


/* Q5 */
bool possibleAvecRotation(Domino di, Domino dj){return di.x == dj.x;}


/* Q6 */
Domino rotation(Domino d){return (Domino) {.x = d.y, .y = d.x};}


/* Q7 */
bool estChaine(element* L){
    if (L == NULL) return true;
    for (element* l = L; l->suiv; l = l->suiv){
        if (l->d.y != l->suiv->d.x) return false;
    }
    return true;
}

/**** Partie 3 ****/

/* Q11 */
bool trouveChaine_aux(Domino jeu[], bool libre[], int n, chaine L);
void trouveChaine(Domino jeu[], bool libre[], int n){
    trouveChaine_aux(jeu, libre, n, (chaine) NULL);
}
bool trouveChaine_aux(Domino jeu[], bool libre[], int n, chaine L){
    assert (estChaine(L));
    bool vide = true;
    Domino domino;
    for (int i = 0; i < n; i++){
        if (!libre[i]) continue;
        vide = false;

        // Backtracking
        domino = jeu[i];
        if (L == NULL || possible(domino, tete(L))){  // sans rotation
            L = ajouteTete(L, domino);
            libre[i] = false;
            if (trouveChaine_aux(jeu, libre, n, L)){
                L = retireTete(L);
                libre[i] = true;
                return true;
            }
            L = retireTete(L);
            libre[i] = true;
        }
        if (L == NULL || possibleAvecRotation(domino, tete(L))){  // avec rotation
            L = ajouteTete(L, rotation(domino));
            libre[i] = false;
            if (trouveChaine_aux(jeu, libre, n, L)){
                L = retireTete(L);
                libre[i] = true;
                return true;
            }
            L = retireTete(L);
            libre[i] = true;
        }
    }
    if (vide) afficheDominos(L);
    return vide;
}

/* Q12 */
int nbChainesCompletes_aux(Domino jeu[], bool libre[], int n, chaine L);
int nbChainesCompletes(Domino jeu[], bool libre[], int n){
    nbChainesCompletes_aux(jeu, libre, n, (chaine) NULL);
}
int nbChainesCompletes_aux(Domino jeu[], bool libre[], int n, chaine L){
    assert (estChaine(L));
    bool vide = true;
    int total = 0;
    Domino domino;
    for (int i = 0; i < n; i++){
        if (!libre[i]) continue;
        vide = false;

        // Backtracking
        domino = jeu[i];
        if (L == NULL || possible(domino, tete(L))){  // sans rotation
            L = ajouteTete(L, domino);
            libre[i] = false;
            total += nbChainesCompletes_aux(jeu, libre, n, L);
            L = retireTete(L);
            libre[i] = true;
        }
        if (domino.x == domino.y) continue;  // on ne compte pas deux fois les dominos symmetriques
        if (L == NULL || possibleAvecRotation(domino, tete(L))){  // avec rotation
            L = ajouteTete(L, rotation(domino));
            libre[i] = false;
            total += nbChainesCompletes_aux(jeu, libre, n, L);
            L = retireTete(L);
            libre[i] = true;
        }
    }
    if (vide) return 1;
    return total;
}

/* Q15 */
bool valide(int qtte[]){
    int impairs = 0;
    for (int i = 0; i < 6; i++){
        impairs += qtte[i] % 2;
    }
    return (impairs == 0 || impairs == 2);
}


bool trouveChaineOpti_aux(Domino jeu[], bool libre[], int n, chaine L, int qtte[]);
void trouveChaineOpti(Domino jeu[], bool libre[], int n){
    int qtte[6] = {0, 0, 0, 0, 0, 0};
    Domino domino;
    for (int i = 0; i < n; i++){
        domino = jeu[i];
        qtte[domino.x] += 1;
        qtte[domino.y] += 1;
    }
    trouveChaineOpti_aux(jeu, libre, n, (chaine) NULL, qtte);
}
bool trouveChaineOpti_aux(Domino jeu[], bool libre[], int n, chaine L, int qtte[]){
    if (!valide(qtte)) return false;
    assert (estChaine(L));
    bool vide = true;
    Domino domino;
    for (int i = 0; i < n; i++){
        if (!libre[i]) continue;
        vide = false;

        // Backtracking
        domino = jeu[i];
        if (L == NULL || possible(domino, tete(L))){  // sans rotation
            qtte[domino.x] -= 1;
            qtte[domino.y] -= 1;
            libre[i] = false;
            L = ajouteTete(L, domino);
            if (trouveChaineOpti_aux(jeu, libre, n, L, qtte)){
                L = retireTete(L);
                libre[i] = true;
                qtte[domino.x] += 1;
                qtte[domino.y] += 1;
                return true;
            }
            L = retireTete(L);
            libre[i] = true;
            qtte[domino.x] += 1;
            qtte[domino.y] += 1;
        }
        if (L == NULL || possibleAvecRotation(domino, tete(L))){  // avec rotation
            qtte[domino.x] -= 1;
            qtte[domino.y] -= 1;
            libre[i] = false;
            L = ajouteTete(L, rotation(domino));
            if (trouveChaineOpti_aux(jeu, libre, n, L, qtte)){
                L = retireTete(L);
                libre[i] = true;
                qtte[domino.x] += 1;
                qtte[domino.y] += 1;
                return true;
            }
            L = retireTete(L);
            libre[i] = true;
            qtte[domino.x] += 1;
            qtte[domino.y] += 1;
        }
    }
    if (vide) afficheDominos(L);
    return vide;
}


int nbChainesCompletesOpti_aux(Domino jeu[], bool libre[], int n, chaine L, int qtte[]);
int nbChainesCompletesOpti(Domino jeu[], bool libre[], int n){
    int qtte[6] = {0, 0, 0, 0, 0, 0};
    Domino domino;
    for (int i = 0; i < n; i++){
        domino = jeu[i];
        qtte[domino.x] += 1;
        qtte[domino.y] += 1;
    }
    return nbChainesCompletesOpti_aux(jeu, libre, n, (chaine) NULL, qtte);
}
int nbChainesCompletesOpti_aux(Domino jeu[], bool libre[], int n, chaine L, int qtte[]){
    if (!valide(qtte)) return 0;
    assert (estChaine(L));
    bool vide = true;
    int total = 0;
    Domino domino;
    for (int i = 0; i < n; i++){
        if (!libre[i]) continue;
        vide = false;

        // Backtracking
        domino = jeu[i];
        if (L == NULL || possible(domino, tete(L))){  // sans rotation
            qtte[domino.x] -= 1;
            qtte[domino.y] -= 1;
            libre[i] = false;
            L = ajouteTete(L, domino);
            total += nbChainesCompletesOpti_aux(jeu, libre, n, L, qtte);
            L = retireTete(L);
            libre[i] = true;
            qtte[domino.x] += 1;
            qtte[domino.y] += 1;
        }
        if (domino.x == domino.y) continue;  // on ne compte pas deux fois les dominos symmetriques
        if (L == NULL || possibleAvecRotation(domino, tete(L))){  // avec rotation
            qtte[domino.x] -= 1;
            qtte[domino.y] -= 1;
            libre[i] = false;
            L = ajouteTete(L, rotation(domino));
            total += nbChainesCompletesOpti_aux(jeu, libre, n, L, qtte);
            L = retireTete(L);
            libre[i] = true;
            qtte[domino.x] += 1;
            qtte[domino.y] += 1;
        }
    }
    if (vide) return 1;
    return total;
}


int main(){
    printf("Backtracking - Dominos\n");

    /*NB : initialiseur de structure + initialiseur de tableau */
    Domino jeu0[3] = {{.x = 1, .y = 0}, {.x = 1, .y = 1}, {.x = 0, .y = 0}};  // faisable
    bool libre0[3] = {true, true, true};

    Domino jeu1[2] = {{.x = 2, .y = 1}, {.x = 0, .y = 1}};  // faisable avec rotation
    bool libre1[2] = {true, true};

    Domino jeu2[5] = {{.x = 5, .y = 4}, {.x = 4, .y = 0}, 
    {.x = 0, .y = 1}, {.x = 3, .y = 2}, {.x = 3, .y = 5}};  
    bool libre2[5] = {true, true, true, true, true}; // faisable avec rotation

    Domino jeu3[5] = {{.x = 0, .y = 0}, {.x = 0, .y = 1}, 
    {.x = 1, .y = 4}, {.x = 1, .y = 2}, {.x = 2, .y = 2}}; // infaisable
    bool libre3[5] = {true, true, true, true, true};

    Domino jeu4[6] = {{.x = 1, .y = 2}, {.x = 1, .y = 3},
    {.x = 1, .y = 1}, {.x = 2, .y = 3}, {.x = 2, .y = 4}, {.x = 3, .y = 4}};
    bool libre4[6] = {true, true, true, true, true, true};

    Domino jeu5[10] = {
    {.x = 0, .y = 1}, {.x = 0, .y = 2}, {.x = 0, .y = 3}, {.x = 0, .y = 4}, 
    {.x = 1, .y = 2}, {.x = 1, .y = 3}, {.x = 1, .y = 4},
    {.x = 2, .y = 3}, {.x = 2, .y = 4},
    {.x = 3, .y = 4}};
    bool libre5[10] = {true, true, true, true, true, true, true, true, true, true};

    Domino jeu6[15] = {
    {.x = 0, .y = 0}, {.x = 0, .y = 1}, {.x = 0, .y = 2}, {.x = 0, .y = 3}, {.x = 0, .y = 4}, 
    {.x = 1, .y = 1}, {.x = 1, .y = 2}, {.x = 1, .y = 3}, {.x = 1, .y = 4},
    {.x = 2, .y = 2}, {.x = 2, .y = 3}, {.x = 2, .y = 4},
    {.x = 3, .y = 3}, {.x = 3, .y = 4},
    {.x = 4, .y = 4}};
    bool libre6[15] = {true, true, true, true, true, true, true, true, true, true, 
                       true, true, true, true, true};

    /**** Tests ****/
    
    printf("\nJeu 0 : ");
    trouveChaine(jeu0, libre0, 3);
    printf("\nJeu 1 : ");
    trouveChaine(jeu1, libre1, 2);
    printf("\nJeu 2 : ");
    trouveChaine(jeu2, libre2, 5);
    printf("\nJeu 3 : ");
    trouveChaine(jeu3, libre3, 5);
    printf("\nJeu 4 : ");
    trouveChaine(jeu4, libre4, 6);
    printf("\nJeu 5 : ");
    trouveChaine(jeu5, libre5, 10);
    printf("\nJeu 6 : ");
    trouveChaine(jeu6, libre6, 15);
    printf("\n");

    printf("\nNb ch. completes:\n");
    printf("Jeu 0 : %d\n", nbChainesCompletes(jeu0, libre0, 3));
    printf("Jeu 1 : %d\n", nbChainesCompletes(jeu1, libre1, 2));
    printf("Jeu 2 : %d\n", nbChainesCompletes(jeu2, libre2, 5));
    printf("Jeu 3 : %d\n", nbChainesCompletes(jeu3, libre3, 5));
    printf("Jeu 4 : %d\n", nbChainesCompletes(jeu4, libre4, 6));
    printf("Jeu 5 : %d\n", nbChainesCompletes(jeu5, libre5, 10));
    printf("Jeu 6 : %d\n", nbChainesCompletes(jeu6, libre6, 15));

    printf("\nOptimisations:");
    printf("\nJeu 0 : ");
    trouveChaineOpti(jeu0, libre0, 3);
    printf("\nJeu 1 : ");
    trouveChaineOpti(jeu1, libre1, 2);
    printf("\nJeu 2 : ");
    trouveChaineOpti(jeu2, libre2, 5);
    printf("\nJeu 3 : ");
    trouveChaineOpti(jeu3, libre3, 5);
    printf("\nJeu 4 : ");
    trouveChaineOpti(jeu4, libre4, 6);
    printf("\nJeu 5 : ");
    trouveChaineOpti(jeu5, libre5, 10);
    printf("\nJeu 6 : ");
    trouveChaineOpti(jeu6, libre6, 15);
    printf("\n---\n");
    printf("Jeu 0 : %d\n", nbChainesCompletesOpti(jeu0, libre0, 3));
    printf("Jeu 1 : %d\n", nbChainesCompletesOpti(jeu1, libre1, 2));
    printf("Jeu 2 : %d\n", nbChainesCompletesOpti(jeu2, libre2, 5));
    printf("Jeu 3 : %d\n", nbChainesCompletesOpti(jeu3, libre3, 5));
    printf("Jeu 4 : %d\n", nbChainesCompletesOpti(jeu4, libre4, 6));
    printf("Jeu 5 : %d\n", nbChainesCompletesOpti(jeu5, libre5, 10));
    printf("Jeu 6 : %d\n", nbChainesCompletesOpti(jeu6, libre6, 15));

    return 0;
}
