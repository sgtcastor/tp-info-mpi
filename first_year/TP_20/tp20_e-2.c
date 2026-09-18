#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/** Le dictionnaire big_dico (ne pas modifier) **/

#include "big_dico7.h"
#define N 31060  /* nb de mots */
#define L 8  /* nb de lettres + 1 */
extern char big_dico[N][L];  // variable globale


int big_dico_bin[N];
char big_dico_alpha[N][L];


int convmot(char mot[]){  // fonction de convertion sur laquelle repose le programme
    int b = 0;
    for (int i = 0; i < L - 1; i++){
        b |= 1 << (mot[i] - 65);
    }
    return b;
}


void convdico(){
    for (int i = 0; i < N; i++){
        big_dico_bin[i] = convmot(big_dico[i]);
    }
}


void swap_alpha(char* mot, int i, int j){
    char temp = mot[i];
    mot[i] = mot[j];
    mot[j] = temp;
}


int split_alpha(char* mot, int a, int b){
    int pivot = mot[b];
    int i = (a - 1);

    for (int j = a; j <= b - 1; j++){
        if (mot[j] < pivot){
            i += 1;
            swap_alpha(mot, i, j);
        }
    }
    swap_alpha(mot, i + 1, b);
    return (i + 1);
}


void alpha(char* mot, int a, int b){
    if (a < b) {
        int pivot = split_alpha(mot, a, b);
        alpha(mot, a, pivot - 1);
        alpha(mot, pivot + 1, b);
    }
}


void alpha_dico(){
    for (int i = 0; i < N; i++){
        alpha(big_dico_alpha[i], 0, L - 2);
    }
}


void swap(int i, int j){
    int temp1 = big_dico_bin[i];
    big_dico_bin[i] = big_dico_bin[j];
    big_dico_bin[j] = temp1;
    char temp2[L];
    strcpy(temp2, big_dico[i]);
    strcpy(big_dico[i], big_dico[j]);
    strcpy(big_dico[j], temp2);

    strcpy(temp2, big_dico_alpha[i]);
    strcpy(big_dico_alpha[i], big_dico_alpha[j]);
    strcpy(big_dico_alpha[j], temp2);
}


int split(int a, int b){
    int pivot = big_dico_bin[b];
    int i = (a - 1);

    for (int j = a; j <= b - 1; j++){
        if (big_dico_bin[j] < pivot){
            i += 1;
            swap(i, j);
        }
    }
    swap(i + 1, b);
    return (i + 1);
}


void quickSort(int a, int b){
    if (a < b) {
        int pivot = split(a, b);
        quickSort(a, pivot - 1);
        quickSort(pivot + 1, b);
    }
}


int dicho_bin(int mot_bin){
    int a = 0;
    int b = N;
    int c;
    while (a < b){
        c = (a + b) / 2;
        if (big_dico_bin[c] < mot_bin){
            a = c + 1;
        } else if (big_dico_bin[c] >= mot_bin){
            b = c;
        }
    }
    if (big_dico_bin[a] == mot_bin){
        return a;
    }
    return -1;
}


bool egal(char entree[], char mot[]){
    for (int i = 0; i < L; i++){
        if (entree[i] != mot[i]){
            return false;
        }
    }
    return true;
}


void trouver(char mot[], bool afficher){
    int mot_bin = convmot(mot);
    int a = dicho_bin(mot_bin);
    if (a != -1){
        char* mot_alpha = malloc(L*sizeof(char));
        strcpy(mot_alpha, mot);
        alpha(mot_alpha, 0, L - 2);
        for (int i = a; big_dico_bin[i] == mot_bin; i++){
            if (afficher && egal(big_dico_alpha[i], mot_alpha)){
                printf("%s\n", big_dico[i]);
            }
        }
        free(mot_alpha);
    }
}


void init(){
    convdico();
    for (int i = 0; i < N; i++){
        strcpy(big_dico_alpha[i], big_dico[i]);
    }
    alpha_dico();
    quickSort(0, N);
}


void testefficacite(int n){
    double temps_total = 0.0;
    int r;
    for (int i = 0; i < n; i++){
        r = rand() % N;
        clock_t debut = clock();
        trouver(big_dico[r], false);
        temps_total += (double) (clock() - debut) / CLOCKS_PER_SEC;
    }
    printf("Temps total sur %d: %f\n", n, temps_total);
    printf("Temps moyen sur %d: %f\n", n, (double) temps_total / n);
}


int main(){
    srand(time(NULL));  // initialisation du générateur de nombres pseudo-aléatoires

    clock_t debut = clock();
    init();
    printf("Temps d'initialisation: %f\n", (float) (clock() - debut) / CLOCKS_PER_SEC);

    char mot[8];  // 7 + 1 lettres (pour le '\0')
    printf("Donnez vos 7 lettres (la casse n'importe pas)\n");
    bool correct = false;
    while (! correct){
        if (scanf("%s", mot) == 1) {
            if (strlen(mot) != 7) {
                printf("Le mot ne fait pas 7 lettres\n");
                while (getchar() != '\n');
            } else {
                for (int i = 0; i < L - 1; i++){
                    if (0x60 < mot[i] && mot[i] < 0x7b){
                        mot[i] = mot[i] - 0x20;
                    }
                }
                printf("mot saisi : %s\n", mot);
                correct = true;
            }
        }
    }

    debut = clock();
    trouver(mot, true);
    printf("Temps d'execution: %lf\n", (float) (clock() - debut) / CLOCKS_PER_SEC);

    testefficacite(1000);

    return 0;
}
