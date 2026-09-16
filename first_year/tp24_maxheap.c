#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define TAILLE_MAX 1024


typedef struct {
    int tab[TAILLE_MAX];
    int longeur;
} Tas;


Tas* creerHeap(){
    Tas* tas = malloc(sizeof(Tas));
    tas->longeur = 0;
    return tas;
}

void detruireHeap(Tas* tas){
    free(tas);
}

void afficherHeap(Tas* tas){
    printf("[");
    if (0 < tas->longeur) printf("%d", tas->tab[0]);
    for (int i = 1; i < tas->longeur; i++){
        printf("; %d", tas->tab[i]);
    }
    printf("]");
}

int premierEnfant(int i){
    return 2 * i + 1;
}

int parent(int i){
    return (i - 1) / 2;
}

void echanger(int* tab, int i, int j){
    int temp = tab[i];
    tab[i] = tab[j];
    tab[j] = temp;
}

void mettreEnTas(Tas* tas, int x){
    int noeud = tas->longeur;
    tas->tab[noeud] = x;
    tas->longeur ++;
    int par = parent(noeud);
    while (noeud > 0 && tas->tab[noeud] > tas->tab[par]){
        echanger(tas->tab, noeud, par);
        noeud = par;
        par = parent(noeud);
    }
}

int sortirDeTas(Tas* tas){
    int max = tas->tab[0];
    int noeud = 0;
    tas->tab[noeud] = tas->tab[tas->longeur - 1];
    tas->longeur --;
    int enf1 = premierEnfant(noeud);
    while (enf1 < tas->longeur){
        if (tas->tab[noeud] < tas->tab[enf1] && (enf1 + 1 >= tas->longeur || tas->tab[enf1] > tas->tab[enf1 + 1])){
            echanger(tas->tab, noeud, enf1);
            noeud = enf1;
        } else if (tas->tab[noeud] < tas->tab[enf1 + 1]){
            echanger(tas->tab, noeud, enf1 + 1);
            noeud = enf1 + 1;
        } else break;
        enf1 = premierEnfant(noeud);
    }
    return max;
}

void triTas(int* tab, int n){
    Tas* tas = creerHeap();
    for (int i = 0; i < n; i++) mettreEnTas(tas, tab[i]);
    for (int i = 0; i < n; i++) tab[n - i - 1] = sortirDeTas(tas);
}

int main(){
    Tas* tas = creerHeap();
    for (int i = 0; i < 10; i++) mettreEnTas(tas, i);
    afficherHeap(tas); printf("\n");
    for (int i = 0; i < 5; i++) printf("%d ", sortirDeTas(tas));
    printf("\n"); afficherHeap(tas); printf("\n");
    detruireHeap(tas);

    int tab[] = {5, 9, 2, 1, 7, 3};
    triTas(tab, 6);
    for (int i = 0; i < 6; i++) printf("%d ", tab[i]);

    return 0;
}
