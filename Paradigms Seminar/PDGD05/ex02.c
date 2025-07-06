/*
** EPITECH PROJECT, 2025
** ex02.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

void append_c(string_t *this, const char *ap)
{
    char *tmp = malloc(sizeof(char) * (strlen(this->str) + strlen(ap) + 1));

    if (!tmp)
        return;
    tmp = strcpy(tmp, this->str);
    tmp = strcat(tmp, ap);
    free(this->str);
    this->str = tmp;
}

void append_s(string_t *this, const string_t *ap)
{
    append_c(this, ap->str);
}
