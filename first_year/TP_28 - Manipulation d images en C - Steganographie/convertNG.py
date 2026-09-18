#!/usr/bin/python3
 
### Convertir un ficher .PNG en .PGM 256
 
import numpy as np
import matplotlib.pyplot as plt
 
import sys
 
 
# Importer une image
def chargeImage(nom_de_fichier):
    img_f=plt.imread(nom_de_fichier)
    img_uint8 = (255*img_f).astype(dtype=np.uint8)
    return img_uint8
 
def niveauxGris(T):
    (n,p,r) = T.shape
    TG = np.zeros((n,p),dtype=np.uint8)
    for i in range(n):
        for j in range(p):
            TG[i,j]= np.uint8(0.2162*T[i,j,0] + 0.7152*T[i,j,1] + 0.0722*T[i,j,2])
    return TG
 
def exporterImage(img, filename):
    with open(filename, "w") as f:
        (n,p) = img.shape
        f.write("P2\n")
        f.write("{} {}\n".format(p,n))
        f.write("255\n")
        for i in range(n):
            for j in range(p):
                f.write("{}\n".format(img[i,j]))
 
# script
try :
    filename = sys.argv[1]
    filename2 = sys.argv[2]
    assert filename[-4:] == ".png"
    assert filename2[-4:] == ".pgm"
    img = chargeImage(filename)
    img_gray = niveauxGris(img)
    exporterImage(img_gray, filename2)
except:
    print("Usage : ./convertNG filename.png filename.pgm")
    raise
