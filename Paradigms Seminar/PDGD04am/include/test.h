/*
** EPITECH PROJECT, 2025
** test.h
** File description:
** Paradigms Seminar - First day of C pool
*/

#ifndef TEST_H_
    #define TEST_H_

    #include "../ex02/concat.h"
    #include "../ex04/print.h"
    #include <stdlib.h>

void mul_div_long(int a, int b, int *mul, int *div);
void mul_div_short(int *a, int *b);
void concat_strings(const char *str1, const char *str2, char **res);
void concat_struct(concat_t *str);
void array_1d_to_2d(const int *array, size_t height, size_t width, int ***res);
void array_2d_free(int **array, size_t height, size_t width);
void print_normal(const char *str);
void print_reverse(const char *str);
void print_upper(const char *str);
void print_42(const char *str);
void do_action(action_t action, const char *str);

#endif
