/* TP - Attracteurs du tic-tac-toe */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

/* codage des pions : vide : 0, X : 1, O : 2 */

/* Adam joue les X */ 

/* stockage des informations
   -3 : le sommet est invalide
   -2 : partie terminée nulle
   -1 : partie non terminée qui n'est pas dans l'attracteur 
   0, 1, 2, 3 ... : plus petit attracteur Ai contenant le sommet */

#define N 19683 // 3^9

int attractA[N];
int attractE[N];

void affiche(int ttt[3][3]){
    char symb[3] = {' ', 'X', 'O'};
    printf("-------\n");
    for (int i = 0; i < 3; i+=1) {
        for (int j = 0; j < 3; j += 1) {
            printf("|%c", symb[ttt[i][j]]);
        }
        printf("|\n");
    }
    printf("-------\n");
}


/* PARTIE 1 - codage / décodage */

int codage(int ttt[3][3]){
    int c = 0;
    for (int i = 0; i < 9; i++) c = c*3 + ttt[i / 3][i % 3];
    return c;
} 

void decodage(int c, int ttt[3][3]){
    for (int i = 8; i >= 0; i--){ttt[i/3][i%3] = c%3; c/=3;}
}

void testCodage(){
    int ttt[3][3] = {{0,1,1}, {2,0,2}, {0,1,2}};
    affiche(ttt);
    assert(codage(ttt) == 3461);
    decodage(3461, ttt);
    affiche(ttt);
}

/* PARTIE 2 - information sur les plateaux */

bool etatGagnant(int ttt[3][3], int J) {
    // teste les lignes
    for (int i = 0; i < 3; i += 1) {
        if ((ttt[i][0] == J) && (ttt[i][1] == J) && (ttt[i][2] == J)) {
            return true;
        }
    }
    // teste les colonnes
    for (int j = 0; j < 3; j += 1) {
        if ((ttt[0][j] == J) && (ttt[1][j] == J) && (ttt[2][j] == J)) {
            return true;
        }
    }
    // teste les diagonales
    if ((ttt[0][0] == J) && (ttt[1][1] == J) && (ttt[2][2] == J)) {
        return true;
    } else if ((ttt[0][2] == J) && (ttt[1][1] == J) && (ttt[2][0] == J)) {
        return true;
    } else {
        return false;
    }
}

// compte le nombre de 0/1/2 dans la grille
void comptage(int ttt[3][3], int *n0, int *n1, int *n2){
    *n0 = *n1 = *n2 = 0;
    int t;
    for (int i = 0; i < 9; i++){
        t = ttt[i/3][i%3];
        if (t == 0) (*n0)++;
        if (t == 1) (*n1)++;
        if (t == 2) (*n2)++;
    }
}

// initialisation des tableaux attractA et attractE
void init(){
    int ttt[3][3];
    int n0, n1, n2;
    bool GA, GE;
    for (int c = 0; c < N; c++){
        decodage(c, ttt);
        comptage(ttt, &n0, &n1, &n2);
        GA = etatGagnant(ttt, 1); GE = etatGagnant(ttt, 2);
        if (n1 - n2 > 1 || n1 < n2 || GA && GE || GA && n1 == n2 || GE && n1 - n2 == 1){
            attractA[c] = attractE[c] = -3;
        } else
        if (GA){attractA[c] =  0; attractE[c] = -1;} else
        if (GE){attractA[c] = -1; attractE[c] =  0;} else
        if (n0 == 0) attractA[c] = attractE[c] = -2; else
        attractA[c] = attractE[c] = -1;
    }
}


/* PARTIE 3 - calcul des attracteurs */

// calcule l'attracteur Ak : renvoie true si de nouveaux états attracteurs ont été trouvés
bool attracK(int attract[N], int J, int K) {
    bool retval = false;
    int ttt[3][3]; int *t;
    int n0, n1, n2;
    bool controle;
    int a;
    for (int c = 0; c < N; c++){
        if (attract[c] != -1) continue;
        decodage(c, ttt); comptage(ttt, &n0, &n1, &n2);
        controle = J == 1 && n1 == n2 || J == 2 && n1 > n2;
        attract[c] = controle ? -1 : K;
        for (int i = 0; i < 9; i++){
            t = &ttt[i/3][i%3];
            if (*t != 0) continue;
            *t = controle ? J : 3 - J;
            a = attract[codage(ttt)];
            if (controle ? a == K-1 : a <= 0){attract[c] = controle ? K : -1; break;}
            *t = 0;
        }
        retval |= attract[c] == K;
    }
    return retval;
}

void attracteurs(){
    int K = 1;
    while (attracK(attractA, 1, K)) K++;
    K = 1;
    while (attracK(attractE, 2, K)) K++;
}

/* PARTIE 4 - Jeu humain vs IA */

void joueIA(int ttt[3][3], int J) {
    // si on est dans l'attracteur J, on cherche à descendre strictement (position gagnante)
    // si on est dans l'attracteur adverse, normalement, impossible d'en sortir 
    // si on n'est pas dans un attracteur, normalement, on ne peut pas entrer dans le sien, et il ne faut pas aller dans celui de l'adversaire
    int* attract = J == 1 ? attractA : attractE;
    int* attractAdv = J == 1 ? attractE : attractA;
    int c = codage(ttt);

    int *t;
    int a;
    if (attract[c] >= 0){
        printf("IA : je vais vous battre en moins de %d coups !\n", attract[c]);
        int ma = 9, mi;
        for (int i = 0; i < 9; i++){
            t = &ttt[i/3][i%3];
            if (*t != 0) continue;
            *t = J;
            a = attract[codage(ttt)];
            if (a >= 0 && a < ma) mi = i;
            *t = 0;
        }
        ttt[mi/3][mi%3] = J;
    } else if (attractAdv[c] >= 0){
        printf("IA : hmm, vous êtes bien parti ... \n");
        int ma = 0, mi;
        for (int i = 0; i < 9; i++){
            t = &ttt[i/3][i%3];
            if (*t != 0) continue;
            *t = J;
            if (attractAdv[codage(ttt)] > ma) mi = i;
            *t = 0;
        }
        ttt[mi/3][mi%3] = J;
    } else {
        printf("IA : c'est serré ... \n");
        for (int i = 0; i < 9; i++){
            t = &ttt[i/3][i%3];
            if (*t != 0) continue;
            *t = J;
            if (attractAdv[codage(ttt)] < 0) return;
            *t = 0;
        }
    }
}


/* joue à l'emplacement lig / col */
bool joue(int ttt[3][3], int lig, int col, int J) {
    if ((lig >= 0) && (lig < 3) && (col >= 0) && (col < 3) && ttt[lig][col] == 0){
        ttt[lig][col] = J;
        return true;
    } else {
        return false;
    }
}

void joueHumain(int ttt[3][3], int J){
    bool ok = false;
    int lig, col;
    while (!ok){
        printf("A vous : ligne, colonne\n");
        if (scanf("%d,%d", &lig, &col) == 2){
            ok = joue(ttt, lig, col, J);
        } else while (getchar() != EOF);
        if (!ok) printf("Erreur de saisie\n");
    }
}

void tictactoe() {
    int ttt[3][3] = {{0,0,0},{0,0,0},{0,0,0}};
    int IA = 1; // identité de l'IA
    printf("Voulez-vous commencer ? (o/n)\n");
    char rep;
    if (scanf("%c", &rep) == 1) {
        if (rep == 'o') IA = 2; // l'IA joue Eve
    }
    int J = 1;  // Adam commence
    bool G;
    affiche(ttt);
    for (int n = 0; n < 9; n++, J=3-J, affiche(ttt)){
        (J == IA ? joueIA : joueHumain)(ttt, J);
        if (G = etatGagnant(ttt, J)) break;
    }
    printf(!G ? "Match nul\n" : J == IA ? "L'IA gagne\n" : "L'Humain gagne\n");
}

// fonction principale 

int main(){
    printf("TicTacToe\n");
    testCodage();

    // Initialisation de attractA et attractE
    init();
    
    // Statistiques sur les états finaux
    int nGA = 0; // gagnant pour A
    int nGE = 0; // gagnant pour E
    int nN = 0;  // partie nulle
    int nT = 0;  // partie valide

    {
        int A, E;
        for (int c = 0; c < N; c++){
            A = attractA[c]; E = attractE[c];
            if (A == 0) nGA++;
            if (E == 0) nGE++;
            if (A == -2) nN++;
            if (E != -3) nT++;
        }
    }
    printf("N:%d GA:%d GE:%d TOT:%d\n", nN, nGA, nGE, nT);

    printf("Calcul attracteurs\n");
    attracteurs();
    
    // Statistiques sur les attracteurs
    int cA = 0; // dans l'attracteur de A
    int cE = 0; // dans l'attracteur de E
    int cN = 0; // partie valide d'issue incertaine
    {
        int A, E;
        for (int c = 0; c < N; c++){
            A = attractA[c]; E = attractE[c];
            if (A >= 0) cA++;
            if (E >= 0) cE++;
            if (A == -1 && E == -1 || A == -2) cN++;
        }
    }
    printf("N:%d A:%d E:%d TOT:%d\n", cN, cA, cE, cN + cA + cE);
    printf("attractA[0]:%d attractE[0]:%d\n", *attractA, *attractE);

    // jeu humain / IA

    tictactoe();

    return 0;
}
