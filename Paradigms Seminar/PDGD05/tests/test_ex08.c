/*
** EPITECH PROJECT, 2025
** test_ex08.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(c_str, test_c_str)
{
    string_t s17;
    string_init(&s17, "String 17");
    cr_assert_str_eq(c_str(&s17), "String 17");
    string_destroy(&s17);
}

Test(c_str, test_c_str_null)
{
    string_t s18;
    string_init(&s18, "Hello");
    string_destroy(&s18);
    cr_assert_null(c_str(&s18));
}
