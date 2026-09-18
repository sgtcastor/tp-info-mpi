/*
Commande pour compiler (et executer):
gcc TP29_voy.c -o TP29_voy -lm && ./TP29_voy
*/

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#define N 10

const char* VILLES[N] = {"Limoges", "Tulle", "Brive", "Chateauroux", "Bergerac", 
                         "Montluçon", "Chartres", "Angoulème", "Villeneuve-sur-Lot", "Mont-de-Marsan"};
const int DIST[N][N] = 
   {{  0,  89,  93, 122, 148, 145, 340, 104, 258, 297}, // distances depuis Limoges
    { 89,   0,  28, 215, 143, 189, 435, 174, 168, 288}, // distances depuis Tulle
    { 93,  28,   0, 219, 109, 207, 425, 153, 134, 254}, // distances depuis Brive / Terrasson
    {122, 215, 219,   0, 269,  98, 203, 200, 325, 422}, // distances depuis Chateauroux / Issoudun
    {148, 143, 109, 269,   0, 279, 462, 107,  59, 153}, // distances depuis Bergerac
    {145, 189, 207,  98, 279,   0, 283, 240, 335, 432}, // distances depuis Montluçon
    {340, 435, 425, 203, 462, 283,   0, 376, 520, 601}, // distances depuis Chartres
    {104, 174, 153, 200, 107, 240, 376,   0, 166, 229}, // distances depuis Angoulème
    {258, 168, 134, 325,  59, 335, 520, 166,   0, 124}, // distances depuis Villeneuve-sur-Lot
    {297, 288, 254, 422, 153, 432, 601, 229, 124,   0}}; // distances depuis Mont-de-Marsan

void testSymetrie(){
    for (int i = 0; i < N; i += 1) {
        assert(DIST[i][i] == 0);
        for (int j = i + 1; j < N; j += 1) {
            assert(DIST[i][j] == DIST[j][i]);
        }
    }
}

/* Q2 */
bool parcoursValide(int villes[], int taille){
    int present = 0;
    for (int i = 0; i < taille; i++) present |= 1 << (villes[i] - 1);
    return (present == (1 << taille) - 1);
}

void testValide(){
    int p1[N-1] = {1,2,4,5,7,3,8,9,6};
    int p2[N-1] = {4,2,4,5,7,3,8,9,6};
    int p3[N-1] = {1,2,4,0,7,3,8,5,6};
    int p4[N-1] = {1,2,4,11,7,3,8,9,6};
    assert(parcoursValide(p1, N-1));
    assert(!parcoursValide(p2, N-1));
    assert(!parcoursValide(p3, N-1));
    assert(!parcoursValide(p4, N-1));
}

/* Q3 */
int longueurParcours(int villes[], int taille){
    int sum = DIST[0][villes[0]];
    for (int i = 0; i < taille - 1; i++) sum += DIST[villes[i]][villes[i+1]];
    return sum + DIST[villes[taille-1]][0];
}

/* Q4 */
void afficheParcours(int parcours[], int taille) {
    if (taille > 0) printf("%s", VILLES[parcours[0]]);
    for (int i = 1; i < taille; i++) printf(" - %s", VILLES[parcours[i]]);
    printf("\n");
}

/* Q6  */
bool mystere(int parcours[], int taille){
    int i = taille - 1;
    while (i >= 0 && parcours[i] == N-1) {
        parcours[i] = 1;
        i -= 1;
    }
    if (i >= 0) {
        parcours[i] += 1;
        return true; 
    } else {
        return false;
    }
}

/* Q7 */
void assigne(int cible[], int val[], int taille);
void plusCourtParcours() {
    int parcours[N-1]; // parcours courant
    int meilleur[N-1]; // meilleur parcours rencontré
    for (int i = 0; i < N-1; i++){
        parcours[i] = i + 1;
    }
    assigne(meilleur, parcours, N-1);
    while (mystere(parcours, N-1)){
        if (parcoursValide(parcours, N-1) && longueurParcours(parcours, N-1) < longueurParcours(meilleur, N-1)){
            assigne(meilleur, parcours, N-1);
        }
    }
    afficheParcours(meilleur, N-1);
    printf("Longeur: %d km\n", longueurParcours(meilleur, N-1));
}
void assigne(int cible[], int val[], int taille){
    for (int i = 0; i < taille; i++) cible[i] = val[i];
}

/* Q11 */
bool inArray(int val, int liste[], int taille);
void parcoursGlouton(){
    int parcours[N-1];
    int courant = 0;
    int meilleurLocal;
    for (int i = 0; i < N-1; i++){
        meilleurLocal = i+1;
        while (inArray(meilleurLocal, parcours, i)) meilleurLocal++;
        for (int j = 1; j < N-1; j++){
            if (!inArray(j, parcours, i) && DIST[courant][j] < DIST[courant][meilleurLocal]){
                meilleurLocal = j;
            }
        }
        parcours[i] = meilleurLocal;
        courant = meilleurLocal;
    }
    afficheParcours(parcours, N-1);
    printf("Longeur: %d km\n", longueurParcours(parcours, N-1));
}
bool inArray(int val, int liste[], int taille){
    for (int i = 0; i < taille; i++){
        if (val == liste[i]) return true;
    }
    return false;
}


/* Q12 */
void echange(int liste[], int a, int b);
void parcoursRecuitSimule(int ne){
    int parcours[N-1];
    for (int i = 0; i < N-1; i++){
        parcours[i] = i + 1;
    }
    float T;  // temperature
    int ra; int rb;  // deux index aleatoires
    int l1; int l2;  // longeurs parcours
    for (int i = 0; i < ne; i++){
        T = 100;
        while (T > 1){
            ra = rand() % (N-1); rb = rand() % (N-2);
            if (rb >= ra) rb += 1;  // ra doit etre != rb

            l1 = longueurParcours(parcours, N-1);
            echange(parcours, ra, rb);
            l2 = longueurParcours(parcours, N-1);

            if (l2 > l1 && rand() > RAND_MAX * exp((float) (l1 - l2) / T)){
                echange(parcours, ra, rb);
            }

            T *= .99;
        }
    }
    afficheParcours(parcours, N-1);
    printf("Longeur: %d km\n", longueurParcours(parcours, N-1));
}
void echange(int liste[], int a, int b){
    int temp = liste[b];
    liste[b] = liste[a];
    liste[a] = temp;
}


int main() {
    srand(time(NULL));

    printf("Les MP2I partent en tournée\n");
    testSymetrie();
    testValide();

    int p0[9] = {1,2,3,4,5,6,7,8,9};
    afficheParcours(p0, 9);
    printf("Longeur: %d km\n", longueurParcours(p0, 9));

    printf("\n - Approche exhaustive - \n");
    // plusCourtParcours();
    
    printf("\n - Parcours glouton - \n");
    parcoursGlouton();
    
    printf("\n - Recuit simulé - \n");
    parcoursRecuitSimule(1);

    return 0;
}