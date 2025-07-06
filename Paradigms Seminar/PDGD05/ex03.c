/*
** EPITECH PROJECT, 2025
** ex03.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

char at_pos(const string_t *this, size_t pos)
{
    if (pos >= strlen(this->str))
        return (-1);
    return (this->str[pos]);
}
