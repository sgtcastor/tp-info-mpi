// gcc -Wall prodcons.c -lpthread -o prodcons && ./prodcons

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* variables partagées */
int stack[5], size = 0;
pthread_mutex_t mx;
sem_t libre, occupe;


int push(int x){
    pthread_mutex_lock(&mx);
    stack[size++] = x;
    pthread_mutex_unlock(&mx);
    return x;
}

int pop (     ){
    pthread_mutex_lock(&mx);
    int x = stack[--size];
    pthread_mutex_unlock(&mx);
    return x;
}

void* prod(void* args){
    for (int i = 0; i < 15; i++){
        sem_wait(&libre);
        sleep(rand() % 100 / 100);
        printf("\e[0;32m+%d\e[0;0m ", push(i)); fflush(stdout);
        sem_post(&occupe);
    }
    return NULL;
}

void* cons(void* args){
    for (int i = 0; i < 10; i++){
        sem_wait(&occupe);
        sleep(rand() % 100 / 100);
        printf("\e[0;31m-%d\e[0;0m ", pop()); fflush(stdout);
        sem_post(&libre);
    }
    return NULL;
}

int main() {
    srand(time(NULL));
    pthread_mutex_init(&mx, NULL);
    sem_init(&libre, 0, 5);
    sem_init(&occupe, 0, 0);
    pthread_t p0, p1, p2;
    pthread_t c0, c1;

    pthread_create(&p0, NULL, prod, NULL);
    pthread_create(&p1, NULL, prod, NULL);
    pthread_create(&p2, NULL, cons, NULL);
    pthread_create(&c0, NULL, cons, NULL);
    pthread_create(&c1, NULL, cons, NULL);

    pthread_join(p0, NULL);
    pthread_join(p1, NULL);
    pthread_join(p2, NULL);
    pthread_join(c0, NULL);
    pthread_join(c1, NULL);

    printf("\n");
    return 0;
}
