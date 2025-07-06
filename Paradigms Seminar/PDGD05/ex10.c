/*
** EPITECH PROJECT, 2025
** ex10.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

int find_s(const string_t *this, const string_t *str, size_t pos)
{
    if (!this->str || !str->str || pos > strlen(this->str))
        return (-1);
    return (strstr(this->str + pos, str->str) - this->str);
}

int find_c(const string_t *this, const char *str, size_t pos)
{
    if (!this->str || !str || pos > strlen(this->str))
        return (-1);
    return (strstr(this->str + pos, str) - this->str);
}
