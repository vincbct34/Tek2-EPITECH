/*
** EPITECH PROJECT, 2025
** ex05.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

int length(const string_t *this)
{
    if (!this->str)
        return (-1);
    return (strlen(this->str));
}
