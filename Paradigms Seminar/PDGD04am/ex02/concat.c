/*
** EPITECH PROJECT, 2025
** concat.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "concat.h"
#include <string.h>
#include <stdlib.h>

void concat_strings(const char *str1, const char *str2, char **res)
{
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    char *str3 = malloc(sizeof(char) * (len1 + len2 + 1));

    strcpy(str3, str1);
    strcat(str3, str2);
    *res = str3;
}

void concat_struct(concat_t *str)
{
    int len1 = strlen(str->str1);
    int len2 = strlen(str->str2);
    char *str3 = malloc(sizeof(char) * (len1 + len2 + 1));

    strcpy(str3, str->str1);
    strcat(str3, str->str2);
    str->res = str3;
}
