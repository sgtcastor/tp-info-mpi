#ifndef _IMAGE_H_
#define _IMAGE_H_
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int haut;
    int larg;
    uint8_t* pixels;
} Image;

uint8_t getPix(Image* img, int i, int j);
void setPix(Image* img, int i, int j, uint8_t v);
Image* creerImage(int haut, int larg);
void detruireImage(Image* img);
Image* redim(Image* img, int haut, int larg);
Image* importerImage(char* filename);
bool exporterImage(Image* img, char* filename);
#endif
