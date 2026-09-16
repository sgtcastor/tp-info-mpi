#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <time.h>

#define TAILLE_MAX 52


typedef struct {
    int tab[TAILLE_MAX];
    int tete;
    int fin;
    int longueur;
} File;

File* creerFile(){
    File* file = (File*) malloc(1*sizeof(File));
    file->tete = 0;
    file->fin = 0;
    file->longueur = 0;
    return file;
}

void enfiler(File* file, int valeur){
    assert(file->longueur < TAILLE_MAX);
    file->tab[file->fin] = valeur;
    file->fin = (file->fin + 1) % TAILLE_MAX;
    file->longueur ++;
}

int defiler(File* file){
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

int* creerPaquet(){
    int* paquet = malloc(TAILLE_MAX*sizeof(int));
    for (int i = 0; i < TAILLE_MAX; i++){
        paquet[i] = (i % 3) + 1;
    }
    return paquet;
}

void melanger(int* paquet){
    srand(time(NULL));
    for (int i = 1; i < TAILLE_MAX; i++){
        int temp = paquet[i];
        int r = rand() % i;
        paquet[i] = paquet[r];
        paquet[r] = temp;
    }
}

void distribuer(int* paquet, File* m1, File* m2){
    for (int i = 0; i < TAILLE_MAX; i++){
        if (i % 2 == 0){
            enfiler(m1, paquet[i]);
        } else {
            enfiler(m2, paquet[i]);
        }
    }
}

void bataille(File* m1, File* m2, File* d){
    printf(" Bataille !\n");

    if (estVide(m1) || estVide(m2)){
        return;
    }

    enfiler(d, defiler(m1));
    enfiler(d, defiler(m2));
    printf("[%d] ? ? [%d]\n", m1->longueur, m2->longueur);

    if (estVide(m1) || estVide(m2)){
        return;
    }

    int carte1 = defiler(m1);
    int carte2 = defiler(m2);
    enfiler(d, carte1);
    enfiler(d, carte2);
    printf("[%d] %d %d [%d]", m1->longueur, carte1, carte2, m2->longueur);

    if (carte1 > carte2){
        printf(" Joueur 1 remporte la bataille\n");
        while (! estVide(d)){
            enfiler(m1, defiler(d));
        }
    } else if (carte1 < carte2){
        printf(" Joueur 2 remporte la bataille\n");
        while (! estVide(d)){
            enfiler(m2, defiler(d));
        }
    } else {
        bataille(m1, m2, d);
    }
}

int partie(){
    int* Paquet = creerPaquet();
    melanger(Paquet);

    File* Main1 = creerFile();
    File* Main2 = creerFile();
    distribuer(Paquet, Main1, Main2);
    free(Paquet);

    File* Defausse = creerFile();

    while (! estVide(Main1) && ! estVide(Main2)){
        int carte1 = defiler(Main1);
        int carte2 = defiler(Main2);
        enfiler(Defausse, carte1);
        enfiler(Defausse, carte2);
        printf("[%d] %d %d [%d]", Main1->longueur, carte1, carte2, Main2->longueur);

        if (carte1 == carte2){
            bataille(Main1, Main2, Defausse);
        } else {
            printf("\n");
        }
    }

    int ret;
    if (estVide(Main2)){
        printf("Joueur 1 remporte le jeu\n");
        ret = 1;
    } else {
        printf("Joueur 2 remporte le jeu\n");
        ret = 2;
    }

    detruireFile(Main1);
    detruireFile(Main2);
    detruireFile(Defausse);

    return ret;
}

int main(){
    int w1 = 1;
    int w2 = 1;
    for (int i = 0; i < 10; i++){
        if (partie() == 1){
            w1 ++;
        } else {
            w2 ++;
        }
    }
    printf("Ratio 1/2: %d/%d = %f\n", w1, w2, (float) w1/w2);
    return 0;
}
