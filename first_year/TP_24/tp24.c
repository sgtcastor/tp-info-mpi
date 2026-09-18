#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#define TAILLE_MAX 100

struct s_Tas {
   int tab[TAILLE_MAX];
   int taille; // nb d'éléments dans le tas
};

typedef struct s_Tas Tas;

/* Exercice 1 */

Tas* creerTas() {
    Tas* tas = malloc(sizeof(Tas));
    tas->taille = 0;
    return tas;
}

void detruireTas(Tas* tas) {
    free(tas);
}

void afficherTableau(int tab[], int n){
    printf("{");
    if (0 < n) printf("%d", tab[0]);
    for (int i = 1; i < n; i++){
        printf(", %d", tab[i]);
    }
    printf("}");
}

void afficherTas(Tas* tas) {
    afficherTableau(tas->tab, tas->taille);
}

bool est_vide(Tas* tas){
    return tas->taille == 0;
}

void monter(Tas* tas, int i);

void ajouter(Tas* tas, int x){
    assert(tas->taille <= TAILLE_MAX);
    tas->tab[tas->taille] = x;
    monter(tas, tas->taille);
    tas->taille ++;
}

void monter(Tas* tas, int i){
    if (i == 0) return;
    int parent = (i - 1) / 2;
    if (tas->tab[parent] < tas->tab[i]){
        int temp = tas->tab[parent];
        tas->tab[parent] = tas->tab[i];
        tas->tab[i] = temp;
        monter(tas, parent);
    }
}

void descendre(Tas* tas, int i);

int retirer_max(Tas* tas){
    assert(! est_vide(tas));
    int maxi = tas->tab[0];
    tas->taille --;
    tas->tab[0] = tas->tab[tas->taille];
    descendre(tas, 0);
    return maxi;
}

void descendre(Tas* tas, int i){  // ameliorable
    int enf1 = 2 * i + 1;
    if (enf1 >= tas->taille) return;

    int enf2 = 2 * i + 2;
    if (enf2 >= tas->taille){
        if (tas->tab[i] < tas->tab[enf1]){
            int temp = tas->tab[enf1];
            tas->tab[enf1] = tas->tab[i];
            tas->tab[i] = temp;
        }
        return;
    }

    if (tas->tab[enf1] < tas->tab[enf2]){
        int temp = tas->tab[enf2];
        tas->tab[enf2] = tas->tab[i];
        tas->tab[i] = temp;
        descendre(tas, enf2);
    } else {
        int temp = tas->tab[enf1];
        tas->tab[enf1] = tas->tab[i];
        tas->tab[i] = temp;
        descendre(tas, enf1);
    }
}

void testTas(){
    Tas* tas = creerTas();
    for (int i = 20; i < 30; i++) ajouter(tas, i);
    while (! est_vide(tas)) printf("%d ", retirer_max(tas)); printf("\n");
    detruireTas(tas);
}

/* Exercice 2 */

void une_journee_en_prepa(){
    Tas* travaux = creerTas();
    int de;
    int priorite;

    for (int i = 0; i < 16; i++){
        // Heure de la journee
        printf("-- Heure %d --\n", i + 1);

        // Nouveaux travaux
        de = rand() % 6 + 1;
        if (de == 1){
            printf("Pas de travail donne\n");
        } else if (2 <= de && de <= 3){
            printf("Travail donne :\n");
            priorite = rand() % 10 + 1;
            ajouter(travaux, priorite);
            printf("+ travail de priorite %d\n", priorite);
        } else if (4 <= de && de <= 6){
            printf("Travail donne :\n");
            priorite = rand() % 10 + 1;
            ajouter(travaux, priorite);
            printf("+ travail de priorite %d\n", priorite);
            priorite = rand() % 10 + 1;
            ajouter(travaux, priorite);
            printf("+ travail de priorite %d\n", priorite);
        }

        // Travail realise
        if (est_vide(travaux)){
            printf("Je m'ennuie ...\n");
        } else {
            printf("Travail realise : travail de priorite %d\n", retirer_max(travaux));
        }
    }
    printf("-- Fin de la journee --\n");
    if (est_vide(travaux)) printf("Tous les travaux ont ete realise\n");
    else {
        printf("Travail non realise :\n");
        while (! est_vide(travaux)) printf("+ travail de priorite %d\n", retirer_max(travaux));
        detruireTas(travaux);
    }
}


/* Fonction principale */

int main(){
    printf("\n** EXERCICE 1 **\n\n");
    Tas* tas = creerTas();
    afficherTas(tas); printf("\n");
    detruireTas(tas);
    testTas();
    printf("\n** EXERCICE 2 **\n\n");
    srand(time(NULL));
    une_journee_en_prepa();
    return 0;
}
