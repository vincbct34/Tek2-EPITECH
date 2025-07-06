/*
** EPITECH PROJECT, 2025
** print.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "print.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void print_normal(const char *str)
{
    printf("%s\n", str);
}

void print_reverse(const char *str)
{
    int len = strlen(str);

    for (int i = len - 1; i >= 0; i--)
        printf("%c", str[i]);
    printf("\n");
}

void print_upper(const char *str)
{
    int len = strlen(str);

    for (int i = 0; i < len; i++)
        printf("%c", toupper(str[i]));
    printf("\n");
}

void print_42(const char *str)
{
    (void)str;
    printf("42\n");
}

void do_action(action_t action, const char *str)
{
    void (*print[PRINT_COUNT])(const char *str) = {print_normal, print_reverse,
        print_upper, print_42};

    print[action](str);
}
