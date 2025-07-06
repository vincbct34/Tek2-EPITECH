/*
** EPITECH PROJECT, 2025
** test_ex05.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(length, test_length)
{
    string_t s12;
    string_init(&s12, "String 12");
    cr_assert_eq(length(&s12), 9);
    string_destroy(&s12);
}

Test(length, test_length_null)
{
    string_t s13;
    string_init(&s13, "Hello");
    string_destroy(&s13);
    cr_assert_eq(length(&s13), -1);
}
