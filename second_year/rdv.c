// gcc -Wall rdv.c -lpthread -o rdv && ./rdv

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define N 10

/* variables partagées */
int finis = 0;
pthread_mutex_t mx;
sem_t suivant;


void attendre(){
    pthread_mutex_lock(&mx);
    if (++finis == N){
        for (int i = 0; i < N; i++) sem_post(&suivant);
        finis = 0;
        printf("\n");
    }
    pthread_mutex_unlock(&mx);
    sem_wait(&suivant);
    pthread_mutex_lock(&mx);
    pthread_mutex_unlock(&mx);
}

void* rdv(void* args){
    int k = 2;  // k = 2
    for (int i = 0; i < k;){
        sleep(rand() % 100 / 100);
        printf("\e[0;%dm%d\e[0;0m ", 31 + i%6, i); fflush(stdout);
        if (++i < k) attendre();
    }
    return NULL;
}

int main() {
    srand(time(NULL));
    pthread_t t[N];
    pthread_mutex_init(&mx, NULL);
    sem_init(&suivant, 0, 0);

    for (int i = 0; i < N; i++) pthread_create(&t[i], NULL, rdv, NULL);
    for (int i = 0; i < N; i++) pthread_join(t[i], NULL);

    printf("\n");
    return 0;
}
