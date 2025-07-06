/*
** EPITECH PROJECT, 2025
** test_ex07.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(copy, test_copy)
{
    string_t s16;
    string_init(&s16, "String 16");
    char buffer[10];
    size_t copied = copy(&s16, buffer, 6, 0);
    cr_assert_str_eq(buffer, "String");
    string_destroy(&s16);
}
