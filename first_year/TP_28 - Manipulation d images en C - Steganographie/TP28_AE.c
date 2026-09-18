#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct s_image {
    int haut;
    int larg;
    uint8_t* pixels;
};

typedef struct s_image Image;

/**
 * Manipulation d'images
 **/

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

void printImage(Image* img){
    int n = img->haut;
    int p = img->larg;
    int ind = 0;
    printf("-------IMAGE-------\n");
    for (int i = 0; i < n; i+=1){
        for (int j = 0; j < p; j += 1){
            printf("%d ", img->pixels[ind]);
            ind += 1;
        }
        printf("\n");
    }
    printf("-------FIN-------\n");
}

/**
 * Image test
 **/
Image* imageTest(int n, int p){
    Image* img = creerImage(n, p);
    for (int i = 0; i < img->haut; i++){
        for (int j = 0; j < img->larg; j++){
            if ((i + j) % 2 == 0) setPix(img, i, j, 255);
            else setPix(img, i, j, 0);
        }
    }
    return img;
}

/**
 *   Import / Export
 **/
// renvoie NULL en cas d'erreur
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

// renvoie false en cas d'erreur
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


/**
 * Traitement d'image 
 **/

void miroir(Image* img){
    uint8_t temp;
    for (int i = 0; i < img->haut; i++){
        for (int j = 0; j < img->larg / 2; j++){
            temp = getPix(img, i, j);
            setPix(img, i, j, getPix(img, i, img->larg-1 - j));
            setPix(img, i, img->larg-1 - j, temp);
        }
    }
}

Image* redim(Image* img, int haut, int larg){
    Image* img2 = creerImage(haut, larg);
    float coeff;
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

/**
 * Steganographie
 **/

void remplacer(Image* steg, Image* support, Image* secret, int shift, int i, int j){
    setPix(steg, i, j, (getPix(support, i, j) & 0b11111100) | (getPix(secret, i, j) >> shift & 3));
}


Image* cacher(Image* support, Image* secret){
    Image* steg = creerImage(support->haut, support->larg);
    Image* sec2 = redim(secret, support->haut / 2, support->larg / 2);
    for (int i = 0; i < support->haut / 2; i++){
        for (int j = 0; j < support->larg / 2; j++){
            remplacer(steg, support, sec2, 6, 2*i  , 2*j  );
            remplacer(steg, support, sec2, 4, 2*i  , 2*j+1);
            remplacer(steg, support, sec2, 2, 2*i+1, 2*j  );
            remplacer(steg, support, sec2, 0, 2*i+1, 2*j+1);
        }
    }
    return steg;
}


int main(){
    printf("TP Images\n");

    Image* test = imageTest(5, 10);
    printImage(test);
    exporterImage(test, "test.pgm");

    Image* mp2i = importerImage("mp2i.pgm");
    printImage(mp2i);

    miroir(mp2i);
    exporterImage(mp2i, "mp2i_miroir.pgm");
    miroir(mp2i);
    exporterImage(redim(mp2i, 60, 40), "mp2i_60_40_E.pgm");

    exporterImage(cacher(importerImage("vangogh.pgm"), importerImage("hippo.pgm")), "vangogh_steg.pgm");

    return 0;
}
