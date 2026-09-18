/* TP n°1 - Exercice 2 - Listes doublement chaînées */

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

struct s_maillon {
    struct s_maillon * prec;
    struct s_maillon * suiv;
    int val;
};
typedef struct s_maillon Maillon;

typedef struct {
    Maillon* prem;
    Maillon* dern;
} ListeDC;


ListeDC* creeListe(){
    ListeDC* liste = (ListeDC*) malloc(1*sizeof(ListeDC));
    liste->prem = NULL;
    liste->dern = NULL;
    return liste;
}


void afficherListe(ListeDC* liste, bool tete){
    if (!liste->prem){
        printf("{}");
        return;
    }
    printf("{");
    if (tete){  // du debut
        for (Maillon* maillon = liste->prem; maillon && maillon != liste->dern; maillon = maillon->suiv){
            printf("%d, ", maillon->val);
        }
        printf("%d}", liste->dern->val);
    } else {  // de la fin
        for (Maillon* maillon = liste->dern; maillon && maillon != liste->prem; maillon = maillon->prec){
            printf("%d, ", maillon->val);
        }
        printf("%d}", liste->prem->val);
    }
}


void ajouterListe(ListeDC* liste, int x, bool tete){
    Maillon* maillon = (Maillon*) malloc(1*sizeof(Maillon));
    maillon->val = x;
    if (tete){  // ajout en tete
        maillon->suiv = liste->prem;
        maillon->prec = NULL;
        if (liste->prem) liste->prem->prec = maillon;  // au moins 1 maillon
        else liste->dern = maillon;  // aucun maillon
        liste->prem = maillon;
    } else {  // ajout en queue
        maillon->suiv = NULL;
        maillon->prec = liste->dern;
        if (liste->dern) liste->dern->suiv = maillon;  // au moins 1 maillon
        else liste->prem = maillon;  // aucun maillon
        liste->dern = maillon;
    }
}


bool supprimerListe(ListeDC* liste, int x, bool tete){
    Maillon* maillon;
    if (tete){
        maillon = liste->prem;
        while (maillon && maillon->val != x) maillon = maillon->suiv;
    } else {
        maillon = liste->dern;
        while (maillon && maillon->val != x) maillon = maillon->prec;
    }
    if (!maillon) return false;

    if (maillon->prec) maillon->prec->suiv = maillon->suiv;
    else liste->prem = maillon->suiv;
    if (maillon->suiv) maillon->suiv->prec = maillon->prec;
    else liste->dern = maillon->prec;

    free(maillon);
    return true;
}


void detruireListe(ListeDC* liste){
    Maillon* suiv;
    for (Maillon* maillon = liste->prem; maillon; maillon = suiv){
        suiv = maillon->suiv;
        free(maillon);
    }
    free(liste);
}


int main() {
    ListeDC* liste = creeListe();
    afficherListe(liste, true); printf("\n");

    // tests
    printf("ajouter 4 en tete et 5 en queue\n");
    ajouterListe(liste, 4, true);
    ajouterListe(liste, 5, false);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    printf("ajouter 3 en tete et 6 en queue\n");
    ajouterListe(liste, 3, true);
    ajouterListe(liste, 6, false);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    printf("ajouter 4 en tete et 5 en queue\n");
    ajouterListe(liste, 4, true);
    ajouterListe(liste, 5, false);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    printf("supprimer 5 a partir de la tete\n");
    supprimerListe(liste, 5, true);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    printf("supprimer 4 a partir de la queue\n");
    supprimerListe(liste, 4, false);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    // cas particuliers (supprimer le premier et le dernier elt)
    printf("supprimer 4 a partir de la tete\n");
    supprimerListe(liste, 4, true);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    printf("supprimer 5 a partir de la tete\n");
    supprimerListe(liste, 5, true);
    afficherListe(liste, true); afficherListe(liste, false); printf("\n");

    detruireListe(liste);
    return 0;
}
