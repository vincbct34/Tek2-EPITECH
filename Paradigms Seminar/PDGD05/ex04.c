/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

void clear(string_t *this)
{
    this->str[0] = '\0';
    this->str = realloc(this->str, sizeof(char));
}
