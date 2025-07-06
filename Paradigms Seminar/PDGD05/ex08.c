/*
** EPITECH PROJECT, 2025
** ex08.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

const char *c_str(const string_t *this)
{
    if (!this->str)
        return (NULL);
    return (this->str);
}
