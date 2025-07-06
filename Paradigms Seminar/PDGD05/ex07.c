/*
** EPITECH PROJECT, 2025
** ex07.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

size_t copy(const string_t *this, char *s, size_t n, size_t pos)
{
    size_t len = 0;

    if (this == NULL || this->str == NULL || s == NULL)
        return 0;
    if (pos >= strlen(this->str))
        return 0;
    len = strlen(this->str);
    strncpy(s, this->str + pos, n);
    if (n > len - pos + 1)
        return len - pos + 1;
    return n;
}
