#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#define N 13

const char* PAYS[N] = {"Argentine", "Bresil", "Bolivie", "Chili", "Colombie", 
                       "Equateur", "Guyana", "Guyane", "Paraguay", "Perou", 
                       "Suriname", "Uruguay", "Venezuela"};

int FRONTIERES[24][2] = 
   {{5,4},{5,9},{4,12},{12,6},{6,10},{10,7},{7,1},{9,2},{9,3},
   {2,3},{2,0},{2,8},{8,0},{3,0},{0,11},{11,1},{8,1},{2,1},{9,1},
   {4,1},{12,1},{6,1},{10,1},{0,1}};

int VOIS[N][N] = 
   {{0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de l'Argentine
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins du Brésil
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de la Bolivie
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins du Chili
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de la Colombie
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de l'Equateur
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de la Guyana
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de la Guyane
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins du Paraguay
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins du Perou
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins du Suriname
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}, // voisins de l'Uruguay
    {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}};// voisins du Venezuela

void afficheVoisins(){
   for (int i = 0; i < N; i++){
      printf("%s a %d voisin", PAYS[i], VOIS[i][0]);
      if (VOIS[i][0] != 1) printf("s");  // s au pluriel (et zero)
      if (VOIS[i][0] > 0) printf("\t:");  // tab et ':' si liste non vide
      for (int j = 0; j < VOIS[i][0]; j++){
         printf(" %s ", PAYS[VOIS[i][j+1]]);
      }
      printf("\n");
   }
}

void initialiseGraphe(){
   int pays0;
   int pays1;
   for (int i = 0; i < 24; i++){
      pays0 = FRONTIERES[i][0];
      pays1 = FRONTIERES[i][1];
      VOIS[pays0][++VOIS[pays0][0]] = pays1;  // incremente la taille de la liste des voisins
      VOIS[pays1][++VOIS[pays1][0]] = pays0;  // et ajoute le pays a la fin de la liste
   }
}

void afficheColoriage(int coul[N]){
   for (int i = 0; i < N; i += 1){
      printf("(%d-%s)\t: %d\n", i, PAYS[i], coul[i]);
   }
}

bool coloriageValide(int coul[N]){
   for (int i = 0; i < N; i++){
      for (int j = 0; j < VOIS[i][0]; j++){
         if (coul[i] == coul[VOIS[i][j+1]]) return false;
      }
   }
   return true;
}

void colorationGloutonne(int coul[N]){
   for (int i = 0; i < N; i++) coul[i] = -1;  // (re)initialisation
   int val;
   for (int i = 0; i < N; i++){
      val = 0;
      for (int j = 0; j < VOIS[i][0]; j++){
         if (coul[VOIS[i][j+1]] >= val) val = coul[VOIS[i][j+1]] + 1;
      }
      coul[i] = val;
   }
   assert(coloriageValide(coul));  // test de l'algo
}

void colorationGloutonneAvecRecyclage(int coul[N]){
   for (int i = 0; i < N; i++) coul[i] = -1;  // (re)initialisation
   int val;
   uint16_t utilisees;  // registre des couleurs utilisees par les voisins
   for (int i = 0; i < N; i++){
      utilisees = 0;
      for (int j = 0; j < VOIS[i][0]; j++){
         utilisees |= 1 << coul[VOIS[i][j+1]];
      }
      val = 0;
      while (utilisees & 1){  // s'arrete au plus petit bit egal a zero
         val++;
         utilisees >>= 1;
      }
      coul[i] = val;
   }
   assert(coloriageValide(coul));  // test de l'algo
}

int main(){
   printf("\nColoration de carte\n");

   printf("\n--- Avant initialisation ---\n");
   afficheVoisins();
   initialiseGraphe();
   printf("\n--- Apres initialisation ---\n");
   afficheVoisins();

   /* Test de coloriageValide */
   int coloriage[N] = {0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
   assert (!coloriageValide(coloriage));
   for (int i = 0; i < N; i++) coloriage[i] = i;
   assert ( coloriageValide(coloriage));
   /* --- */

   printf("\n--- Coloration gloutonne ---\n");
   colorationGloutonne(coloriage);
   afficheColoriage(coloriage);
   printf("\n--- Coloration gloutonne avec recyclage ---\n");
   colorationGloutonneAvecRecyclage(coloriage);
   afficheColoriage(coloriage);

   printf("\n");
   return 0;
}