/*
** EPITECH PROJECT, 2025
** test_ex00.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(string_init, test0)
{
    string_t s;

    string_init(&s, NULL);
}

Test(string_init, test1)
{
    string_t s;

    string_destroy(NULL);
}
