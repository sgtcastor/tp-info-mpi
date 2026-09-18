#include "image.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

Image* reveler(Image* img){
    Image* secret = creerImage(img->haut / 2, img->larg / 2);
    for (int i = 0; i < img->haut / 2; i++){
        for (int j = 0; j < img->larg / 2; j++){
            setPix(secret, i, j,
                (getPix(img, 2*i  , 2*j  ) & 0b11) << 6 |
                (getPix(img, 2*i  , 2*j+1) & 0b11) << 4 |
                (getPix(img, 2*i+1, 2*j  ) & 0b11) << 2 |
                (getPix(img, 2*i+1, 2*j+1) & 0b11)
            );
        }
    }
    return secret;
}

int main(int argc, char* argv[]){
    if (argc < 3) {
        fprintf(stderr, "trop peu de parametres\n");
        printf("./revele image secret\n");
        exit(0);
    }
    if (argc > 3) {
        fprintf(stderr, "trop de parametres\n");
        printf("./revele image secret\n");
        exit(0);
    }
    Image* img = importerImage(argv[1]);
    Image* secret = reveler(img);
    detruireImage(img);
    exporterImage(secret, argv[2]);
    detruireImage(secret);
}
