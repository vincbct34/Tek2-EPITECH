/*
** EPITECH PROJECT, 2025
** test_ex09.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(empty, test_empty)
{
    string_t s19;
    string_init(&s19, "String 19");
    cr_assert_eq(empty(&s19), 0);
    string_destroy(&s19);
}

Test(empty, test_empty_null)
{
    string_t s20;
    string_init(&s20, "Hello");
    string_destroy(&s20);
    cr_assert_eq(empty(&s20), 1);
}
