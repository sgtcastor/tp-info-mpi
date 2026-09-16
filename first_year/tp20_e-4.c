#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>


/** Le dictionnaire big_dico (ne pas modifier) **/

#include "big_dico7.h"
#define N 31060  /* nb de mots */
#define L 8  /* nb de lettres + 1 */
extern char big_dico[N][L];  // variable globale


char big_dico_alpha[N][L];  // dico aux lettres des mots triees par ordre alpha.
int big_dico_int[N];  // dico_alpha converti en liste d'int
char input[L];


/* Declaration des fonctions internes */
uint64_t convmot(char mot[]);
void selectionsort_alpha(char mot[]);
void alpha_dico();
void quicksort_int(int a, int b);
    int split(int a, int b);
    void swap(int i, int j);
int dicho(int mot);


void init(){
    for (int i = 0; i < N; i++){  // conversion
        strcpy(big_dico_alpha[i], big_dico[i]);
        selectionsort_alpha(big_dico_alpha[i]);
        big_dico_int[i] = convmot(big_dico_alpha[i]);
    }
    quicksort_int(0, N);  // tri
}


void getinput(){
    printf("Donnez vos %d lettres (la casse n'importe pas)\n", L - 1);
    bool correct = false;
    while (!correct){
        if (scanf("%s", input) == 1){
            if (strlen(input) != L - 1){
                printf("Le mot ne fait pas %d lettres\n", L - 1);
                while (getchar() != '\n');
            } else {
                /* "Majusculisation" */
                for (int i = 0; i < L - 1; i++){
                    if ('a' <= input[i] && input[i] <= 'z'){
                        input[i] = input[i] - 0x20;
                    }
                }
                printf("mot saisi : %s\n", input);
                correct = true;
            }
        }
    }
}


void trouver(char mot[], bool afficher){
    char mot_alpha[L];
    strcpy(mot_alpha, mot);
    selectionsort_alpha(mot_alpha);
    int mot_int = convmot(mot_alpha);
    int a = dicho(mot_int);
    if (a != -1 && afficher){
        for (int i = a; big_dico_int[i] == mot_int; i++){
            printf("%s\n", big_dico[i]);
        }
    }
}


void testrapidite(int n){
    double temps_total = 0.0;
    int rndm;
    for (int i = 0; i < n; i++){
        rndm = rand() % N;
        clock_t debut = clock();
        trouver(big_dico[rndm], false);
        temps_total += (double) (clock() - debut) * 1000 / CLOCKS_PER_SEC;
    }
    printf("Temps total sur %d: %f ms\n", n, temps_total);
    printf("Temps moyen sur %d: %f µs\n", n, (double) temps_total * 1000 / n);
}


int main(){
    srand(time(NULL));  // initialisation du générateur de nombres pseudo-aléatoires

    /* Initialisation des dictionnaires */
    clock_t debut = clock();
    init();
    printf("Temps d'initialisation: %f ms\n",
           (float) (clock() - debut) * 1000 / CLOCKS_PER_SEC);

    getinput();

    /* Recherche */
    debut = clock();
    trouver(input, true);
    printf("Temps d'execution: %lf ms\n",
           (float) (clock() - debut) * 1000 / CLOCKS_PER_SEC);

    testrapidite(1000);

    return 0;
}


uint64_t convmot(char mot[]){  // fonction de convertion mot -> int (unique)
    uint64_t b = 0;  // uint64_t limite a des mots de 14 lettres maxi
    for (int i = 0; i < L - 1; i++){
        b |= (uint64_t) (mot[i] - 'A') << (5 * i);
    }  // BUG du << avec 64 bits
    return b;
}


void selectionsort_alpha(char mot[]){  // tri alpha. des lettres d'un mot
    int mini;
    char temp;
    for (int i = 0; i < L - 1; i++){
        mini = i;
        for (int j = i + 1; j < L - 1; j++){
            if (mot[j] < mot[mini]){
                mini = j;
            }
        }
        temp = mot[mini];
        mot[mini] = mot[i];
        mot[i] = temp;
    }
}


void alpha_dico(){  // tri alpha. du dico
    for (int i = 0; i < N; i++){
        selectionsort_alpha(big_dico_alpha[i]);
    }
}


void quicksort_int(int a, int b){  // tri du dico en fonction de sa version int
    if (a < b) {
        int pivot = split(a, b);
        quicksort_int(a, pivot - 1);
        quicksort_int(pivot + 1, b);
    }
}
int split(int a, int b){  // partition du quicksort
    int pivot = big_dico_int[b];
    int i = (a - 1);

    for (int j = a; j <= b - 1; j++){
        if (big_dico_int[j] < pivot){
            i += 1;
            swap(i, j);
        }
    }
    swap(i + 1, b);
    return (i + 1);
}
void swap(int i, int j){  // echange deux entrees de dico
    int temp_int = big_dico_int[i];  // dico int
    big_dico_int[i] = big_dico_int[j];
    big_dico_int[j] = temp_int;

    char temp_str[L];  // dico originel
    strcpy(temp_str, big_dico[i]);
    strcpy(big_dico[i], big_dico[j]);
    strcpy(big_dico[j], temp_str);
}  // pas besoin de modifier dico_alpha (inutile apres init. de dico_int)


int dicho(int mot){  // recherche dichotomique dans dico_int
    int a = 0;
    int b = N;
    int c;
    while (a < b){
        c = (a + b) / 2;
        if (big_dico_int[c] < mot){
            a = c + 1;
        } else if (big_dico_int[c] >= mot){
            b = c;
        }
    }
    if (big_dico_int[a] == mot){
        return a;
    }
    return -1;
}
