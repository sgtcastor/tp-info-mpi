#include <assert.h>
#include <stdlib.h>
#include "image.h"

// COMPILATION + RUN:
// gcc image.c tp17_e.c -o tp17; ./tp17


int dist(pixel p1, pixel p2){
    int r = p1.r - p2.r, v = p1.v - p2.v, b = p1.b - p2.b;
    return r*r + v*v + b*b;
}

pixel mean(pixel p1, pixel p2){
    return (pixel) {.r = (p1.r + p2.r)/2, .v = (p1.v + p2.v)/2, .b = (p1.b + p2.b)/2};
}

int find(int *tree, int i){return tree[i] == i ? i : (tree[i] = find(tree, tree[i]));}


void kmeans(pixel *pixels, int n, int k){
    /* Il est T17:40 et je me rends compte que j'implémente
    le mauvais algorithme */
    int *tree = malloc(n*sizeof(int));
    for (int i = 0; i < n; i++) tree[i] = i;
    
    int min_d = dist(pixels[0], pixels[1]);
    int min_i = 0, min_j = 1;
    for (int i = 0; i < n; i++){
        for (int j = i+1; j < n; j++){
            if (find(tree, i) == find(tree, j)) continue;
            int d = dist(pixels[i], pixels[j]);
            if (d < min_d){min_d = d; min_i = i; min_j = j;}
        }
    }
    pixel m = mean(pixels[min_i], pixels[min_j]);
    pixels[min_i] = m;
    pixels[min_j] = m;
    tree[min_j] = tree[min_i];
}

void kmeans2(pixel *pixels, int n, int k){
    pixel *centroids = malloc(k*sizeof(pixel));
    for (int i = 0; i < k; i++) centroids[i] = (pixel) {.r = rand()&255, .v = rand()&255, .b = rand()&255};
    for (int i = 0; i < n; i++){
        int j_min = 0, d_min = dist(pixels[i], centroids[0]);
        for (int j = 1; i < k; j++){
            int d = dist(pixels[i], centroids[j]);
            if (d < d_min){j_min = j; d_min = d;}
        }
    }
}

int main(int argc, char* argv[]){
    assert(argc == 4);
    int k = 4;  // argv[3];

    Image* source = importerImage(argv[1]);
    int h = source->haut, l = source->larg;
    kmeans2(source->pixels, h*l, k);
    detruireImage(source);

    Image* dest = creerImage(h, l);
    exporterImage(dest, argv[2]);
    detruireImage(dest);

    return 0;
}
