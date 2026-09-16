#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>


/* Exercice 3 */

int t[9] = {7, 4, -1, 5, 6, 18, 11, 8, 77};

void afficherTableau(int tab[], int n){
    printf("{");
    if (0 < n) printf("%d", tab[0]);
    for (int i = 1; i < n; i++){
        printf(", %d", tab[i]);
    }
    printf("}");
}

void remonter_dans_le_tas(int tab[], int i){
    if (i == 0) return;
    int parent = (i - 1) / 2;
    if (tab[parent] < tab[i]){
        int temp = tab[parent];
        tab[parent] = tab[i];
        tab[i] = temp;
        remonter_dans_le_tas(tab, parent);
    }
}

void descendre_dans_le_tas(int tab[], int i, int n){  // ameliorable
    int enf1 = 2 * i + 1;
    if (enf1 >= n) return;

    int enf2 = 2 * i + 2;
    if (enf2 >= n){
        if (tab[i] < tab[enf1]){
            int temp = tab[enf1];
            tab[enf1] = tab[i];
            tab[i] = temp;
        }
        return;
    }

    if (tab[enf1] < tab[enf2]){
        int temp = tab[enf2];
        tab[enf2] = tab[i];
        tab[i] = temp;
        descendre_dans_le_tas(tab, enf2, n);
    } else {
        int temp = tab[enf1];
        tab[enf1] = tab[i];
        tab[i] = temp;
        descendre_dans_le_tas(tab, enf1, n);
    }
}

void tri_par_tas(int tab[], int n){  // Heap sort
    for (int i = 1; i < n; i++){
        remonter_dans_le_tas(tab, i);
    }
    printf("Tableau en Tas  : "); afficherTableau(tab, n); printf("\n");
    int temp;
    for (int i = n - 1; i > 0; i--){
        temp = tab[i]; tab[i] = tab[0]; tab[0] = temp;
        descendre_dans_le_tas(tab, 0, i);
    }
}

/* Fonction principale */

int main(){
    printf("\n** EXERCICE 3 **\n\n");
    printf("Tableau initial : "); afficherTableau(t, 9); printf("\n");
    tri_par_tas(t, 9);
    printf("Tableau final   : "); afficherTableau(t, 9); printf("\n");
    return 0;
}