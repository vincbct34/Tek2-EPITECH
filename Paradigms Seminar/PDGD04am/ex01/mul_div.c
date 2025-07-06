/*
** EPITECH PROJECT, 2025
** mul_div.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <stdio.h>

void mul_div_long(int a, int b, int *mul, int *div)
{
    *mul = a * b;
    *div = (b != 0) ? (a / b) : 42;
}

void mul_div_short(int *a, int *b)
{
    int tmp = *a;

    *a = *a * *b;
    *b = (*b != 0) ? (tmp / *b) : 42;
}
