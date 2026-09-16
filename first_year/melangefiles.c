#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#define TAILLE_MAX 5

/* Implémentation par tableau de taille fixe, avec circularité */

struct s_File {
    int tab[TAILLE_MAX]; // stockage des éléments de la file
    int tete; // position de la tete
    int fin;  // position du premier emplacement libre
    int longueur; // nb d'éléments
};

typedef struct s_File File;

File* creerFile(){
    File* file = (File*) malloc(1*sizeof(File));
    file->tete = 0;
    file->fin = 0;
    file->longueur = 0;
    return file;
}

void mettreEnFile(File* file, int valeur){
    assert(file->longueur < TAILLE_MAX);
    file->tab[file->fin] = valeur;
    file->fin = (file->fin + 1) % TAILLE_MAX;
    file->longueur ++;
}

int sortirDeFile(File* file){
    assert(file->longueur > 0);
    int val = file->tab[file->tete];
    file->tete = (file->tete + 1) % TAILLE_MAX;
    file->longueur --;
    return val;
}

void afficherFile(File* file){
    printf("[");
    for (int i = 0; i < file->longueur; i += 1) {
        printf("%d", file->tab[(i + file->tete) % TAILLE_MAX]);
        if (i < file->longueur - 1){
            printf(", ");
        }
    }
    printf("]");
}

bool estVide(File* file){
    return file->longueur == 0;
}

bool estPleine(File* file){
    return file->longueur == TAILLE_MAX;
}

void detruireFile(File* file){
    free(file);
}

void testFile(){
    File* file = creerFile();
    assert(estVide(file));
    for (int i = 0; i<4; i+=1){
        mettreEnFile(file, i);
    }
    afficherFile(file);
    for (int i = 0; i<2; i+=1){
        sortirDeFile(file);
    }
    afficherFile(file);
    for (int i = 4; i<7; i+=1){
        mettreEnFile(file, i);
    }
    afficherFile(file);
    assert(estPleine(file));
    detruireFile(file);
}


void simulation(int temps){
    File* F1 = creerFile();
    File* F2 = creerFile();
    File* F3 = creerFile();
    afficherFile(F1);
    printf("\n");
    afficherFile(F2);
    printf("\n");
    afficherFile(F3);
    printf("\n");
    for (int t = 0; t < temps; t++){
        if (t % 6 == 0){
            for (int i = 0; i < 3; i++){
                if (! estVide(F3)){
                    sortirDeFile(F3);
                    printf("3->\n");
                }
                if (! estVide(F1)){
                    mettreEnFile(F3, sortirDeFile(F1));
                    printf("1->3\n");
                } else if (! estVide(F2)){
                    mettreEnFile(F3, sortirDeFile(F2));
                    printf("2->3\n");
                }
            }
        }
        if (rand() % 2 < 1){
            if (estPleine(F1)){
                printf("Tuuut1\n");
            } else {
                mettreEnFile(F1, 1);
                printf("->1\n");
            }
        }
        if (rand() % 4 < 3){
            if (estPleine(F2)){
                printf("Tuuut2\n");
            } else {
                mettreEnFile(F2, 2);
                printf("->2\n");
            }
        }
        afficherFile(F1);
        printf("\n");
        afficherFile(F2);
        printf("\n");
        afficherFile(F3);
        printf("\n");
        printf("\n");
    }
    detruireFile(F1);
    detruireFile(F2);
    detruireFile(F3);
}


int main(){
    printf("Mélange de files\n");
    testFile();

    printf("Simulation d'un carrefour\n");
    simulation(100);

    return 0;
}
