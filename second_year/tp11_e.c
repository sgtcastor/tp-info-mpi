#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

/* noeud : identifiant entre 0 et N-1 */
typedef int nodeID;

/* représente une ville */
struct node_s {
    char nom[100]; // nom
    float x; // longitude
    float y; // latitude
    int hab; // nombre d'habitants
};

typedef struct node_s node;

/* représente une liaison possible */
struct edge_s {
    nodeID i;
    nodeID j;
    float  d;
    bool selected; // pour l'algorithme de Kruskal (ou Prim)
};

typedef struct edge_s edge;

double sqrt(double x){
    float r = 1;
    for (int i = 0; i < 8; i++) r = r + (x-r*r)/(2*r);
    return r;
}

/* import : lit le fichier filename contenant des descriptions
de villes et alloue dans le tas un tableau de n villes 
NB : n est renvoyé via le pointeur n */
node* lireFichier(const char* filename, int* n) {
    FILE* f = fopen(filename, "r");
    if (f == NULL){
        printf("Erreur a l'ouverture 1\n");
        return NULL;
    }
    int nv;
    if (fscanf(f, "%d\n", &nv) != 1){
        printf("Erreur de lecture 1\n");
        return NULL;
    }
    printf("il y a %d villes\n", nv);
    node* villes = malloc(nv*sizeof(node));
    for (int i = 0; i < nv; i++){
        if (fscanf(f, "%s %d %f %f\n", villes[i].nom, &villes[i].hab, &villes[i].x, &villes[i].y) != 4){
            printf("Erreur de lecture 1\n");
            return NULL;
        }
    }
    *n = nv;
    return villes;
}


/* crée l'ensemble des arêtes susceptibles de relier les villes 
les arêtes sont allouées dans le tas. Leur nombre na est 
renvoyé via pointeur */
edge* creerAretes(node* villes, int nv, int* na){
    *na = nv*(nv-1)/2;
    edge* aretes = malloc(*na*sizeof(aretes));
    int index = 0;
    for (int i = 0; i < nv; i++){
        for (int j = 0; j < nv; j++){
            aretes[index].i = i;
            aretes[index].j = j;
            float dx = villes[i].x - villes[j].x;
            float dy = villes[i].y - villes[j].y;
            aretes[index].d = 107 * sqrt(dx*dx + dy*dy);
            aretes[index].selected = false;
            index++;
        }
    }
    return aretes;
}


/* Code trouvé sur internet ... */

void quicksort(edge* tab, int taille) {
    // cas induit
    if (taille <= 1) return;
    edge pivot = tab[0];
    // pivot a la fin
    tab[0] = tab[taille-1];
    tab[taille-1] = pivot;
    // tri
    int isep = 0; // index pivot final
    edge elt;
    for (int i = 0; i < taille - 1; i++){
        elt = tab[i];
        if (elt.d < pivot.d){
            tab[i] = tab[isep];
            tab[isep] = elt;
            isep++;
        }
    }
    // pivot a sa place
    edge tmp = tab[isep];
    tab[isep] = pivot;
    tab[taille-1] = tmp;
    // recursivite
    quicksort(tab, isep);
    quicksort(&tab[isep+1], taille-isep-1);
}



/* export : écrit dant le fichier filename les informations nécessaires 
à l'affichage des arêtes sélectionnées au format geojson */
void exporter(const char* filename, edge* aretes, int na, node* villes, int nv){
    FILE* f = fopen(filename, "w");
    fprintf(f,"{\"type\": \"FeatureCollection\",\"features\": [\n");
    // A COMPLETER
    // ...........pour chaque arête e sélectionnée, on doit écrire dans f :
    // fprintf(f, "{\"type\": \"Feature\", \"geometry\": {\"type\": \"LineString\",");
    // fprintf(f,  "\"coordinates\": [[%f, %f], [%f, %f]]}},\n", 
    //                villes[e.i].x, villes[e.i].y, villes[e.j].x, villes[e.j].y);
    // ...... et entre chaque  écriture, on doit mettre une virgule ","
    fprintf(f,"]}");
    fclose(f);
}


int main(){
    printf("Yaourtophone project !\n");

    const char* fichier_in = "villes20.txt";
    const char* fichier_out = "villes20.geojson";

    /* lecture du fichier */
    int nv;
    node* villes = lireFichier(fichier_in, &nv);

    /* affichage des villes */
    for (int i = 0; i < nv; i++) printf("%s ", villes[i].nom); printf("\n");

    /* création du tableau d'arêtes */
    int na;
    edge* aretes = creerAretes(villes, nv, &na);

    /* algorithme de Kruskal */

    /* tri du tableau d'arêtes */
    quicksort(aretes, na);
    // for (int i = 0; i < na; i++) printf("%f ", aretes[i].d); printf("\n");

    // composantes connexes de chaque sommet
    int c[nv];
    for (int i = 0; i < nv; i++) c[i] = i;

    // algo glouton
    edge reseau[nv-1];
    int j = 0;
    for (int i = 0; i < nv - 1; i ++){
        while (c[aretes[j].i] == c[aretes[j].j]) j++;
        reseau[i] = aretes[j];
        c[aretes[j].j] = c[aretes[j].i];
    }
    float tt = 0;
    for (int i = 0; i < nv-1; i++) tt += reseau[i].d;
    printf("Longeur totale : %f\n", tt);

    /* export geojson */
    // A FAIRE
    return 0;
}