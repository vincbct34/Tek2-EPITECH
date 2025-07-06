/*
** EPITECH PROJECT, 2025
** array_1d_to_2d.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <stdlib.h>

void array_1d_to_2d(const int *array, size_t height, size_t width, int ***res)
{
    int **array_2d = malloc(sizeof(int *) * height);

    for (size_t i = 0; i < height; i++) {
        array_2d[i] = malloc(sizeof(int) * width);
        for (size_t j = 0; j < width; j++) {
            array_2d[i][j] = array[i * width + j];
        }
    }
    *res = array_2d;
}

void array_2d_free(int **array, size_t height, size_t width)
{
    (void)width;
    for (size_t i = 0; i < height; i++) {
        free(array[i]);
    }
    free(array);
}
