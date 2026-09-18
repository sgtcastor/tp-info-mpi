/* TP 01 - Exercice 1 - Quicksort en C */

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>


void tri_rapide(int* tab, int n) {
    //DEBUG// printf("IN: "); for (int i = 0; i < n; i++) printf("%d ", tab[i]); printf("\n");

    if (n <= 1) return;
    int pivot = 0;
    int temp;
    for (int i = 0; i < n - 1; i++){
        if (tab[i] < tab[n - 1]){
            temp = tab[pivot];
            tab[pivot] = tab[i];
            tab[i] = temp;
            pivot++;
        }
    }
    temp = tab[pivot];
    tab[pivot] = tab[n - 1];
    tab[n - 1] = temp;
    tri_rapide(tab, pivot);
    tri_rapide(tab + pivot + 1, n - pivot - 1);

    //DEBUG// printf("OUT: "); for (int i = 0; i < n; i++) printf("%d ", tab[i]); printf("\n");
}

// NB tests : la mise en place de tests 
// ne doit pas modifier le code des fonctions testées
int main(){
    printf("Début tests\n");

    // test
    int t[10] = {7,11,-1,3,8,4,6,0,4,3}; // tableau initial
    int tt[10] = {-1,0,3,3,4,4,6,7,8,11}; // résultat attendu
    tri_rapide(t, 10);
    for (int i = 0; i < 10; i++) assert(t[i] == tt[i]);

    int t0[0] = {};
    tri_rapide(t0, 0);

    int t1[1] = {42};
    tri_rapide(t1, 1);

    int t2[2] = {4, 2};
    tri_rapide(t2, 2);

    printf("\nFin tests\n");
    return 0;
}
