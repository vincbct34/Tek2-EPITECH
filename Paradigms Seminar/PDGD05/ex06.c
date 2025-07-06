/*
** EPITECH PROJECT, 2025
** ex06.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

int compare_s(const string_t *this, const string_t *str)
{
    if (!this->str || !str->str)
        return (-1);
    return (strcmp(this->str, str->str));
}

int compare_c(const string_t *this, const char *str)
{
    if (!this->str || !str)
        return (-1);
    return (strcmp(this->str, str));
}
