#include "image.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


void remplacer(Image* steg, Image* support, Image* secret, int shift, int i, int j){
    setPix(steg, i, j,
        (getPix(support, i, j) & 0b11111100) |
        (getPix(secret, i/2, j/2) >> shift & 0b11)
    );
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
    detruireImage(sec2);
    return steg;
}

int main(int argc, char* argv[]){
    if (argc < 4) {
        fprintf(stderr, "trop peu de parametres\n");
        printf("./cache support secret image\n");
        exit(0);
    }
    if (argc > 4) {
        fprintf(stderr, "trop de parametres\n");
        printf("./cache support secret image\n");
        exit(0);
    }
    Image* support = importerImage(argv[1]);
    Image* secret = importerImage(argv[2]);
    Image* img = cacher(support, secret);
    detruireImage(support);
    detruireImage(secret);
    exporterImage(img, argv[3]);
    detruireImage(img);
}
