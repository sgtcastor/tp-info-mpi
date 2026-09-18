#include "image.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

uint8_t getPix(Image* img, int i, int j){
    assert(img != NULL && i >= 0 && i < img->haut && j >=0 && j < img->larg);
    return img->pixels[i*img->larg + j];
}

void setPix(Image* img, int i, int j, uint8_t v){
    assert(img != NULL && i >= 0 && i < img->haut && j >=0 && j < img->larg);
    img->pixels[i*img->larg + j] = v;
}

Image* creerImage(int haut, int larg) {
    uint8_t* pixels = malloc(haut*larg*sizeof(int));
    Image* img = malloc(sizeof(Image));
    img->haut = haut;
    img->larg = larg;
    img->pixels = pixels;
    return img;
}

void detruireImage(Image* img) {
    free(img->pixels);
    free(img);
}

Image* redim(Image* img, int haut, int larg){
    Image* img2 = creerImage(haut, larg);
    float coeff;  // coefficient d'agrandissement
    if (img->haut / haut < img->larg / larg) coeff = (float) img->haut / haut;
    else coeff = (float) img->larg / larg;
    for (int i = 0; i < haut; i++){
        for (int j = 0; j < larg; j++){
            if (0 <= i*coeff && i*coeff < img->haut && 0 <= j*coeff && j*coeff < img->larg){
                setPix(img2, i, j, getPix(img, i*coeff, j*coeff));
            } else setPix(img2, i, j, 127);
        }
    }
    return img2;
}

Image* importerImage(char* filename){
    FILE* f = fopen(filename, "r");
    if (f == NULL) return NULL;

    char format[3];
    fscanf(f, "%s", format);
    if (format[0] != 'P' || format[1] != '2') return NULL;

    int larg; int haut;
    fscanf(f, "%d %d", &larg, &haut);
    Image* img = creerImage(haut, larg);

    int maxval;
    fscanf(f, "%d", &maxval);
    if (maxval != 255) return NULL;

    int pixel;
    int k = 0;
    while (fscanf(f, "%d", &pixel) && k < haut*larg){
        if (maxval < pixel) return NULL;
        setPix(img, k / larg, k % larg, (uint8_t) pixel);
        k++;
    }

    fclose(f);
    return img;
}

bool exporterImage(Image* img, char* filename){
    FILE* f = fopen(filename, "w");
    if (f == NULL) return false;
    fprintf(f, "P2\n");
    fprintf(f, "%d %d\n", img->larg, img->haut);
    fprintf(f, "255\n");
    for (int i = 0; i < img->haut; i++){
        for (int j = 0; j < img->larg; j++){
            fprintf(f, "%d ", getPix(img, i, j));
        }
        fprintf(f, "\n");
    }
    fclose(f);
    return true;
}
