/*
** EPITECH PROJECT, 2025
** ex01.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

void assign_c(string_t *this, const char *s)
{
    if (this->str)
        string_destroy(this);
    string_init(this, s);
}

void assign_s(string_t *this, const string_t *str)
{
    assign_c(this, str->str);
}
