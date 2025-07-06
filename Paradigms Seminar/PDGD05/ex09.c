/*
** EPITECH PROJECT, 2025
** ex09.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

int empty(const string_t *this)
{
    if (!this->str || this->str[0] == '\0')
        return (1);
    return (0);
}
