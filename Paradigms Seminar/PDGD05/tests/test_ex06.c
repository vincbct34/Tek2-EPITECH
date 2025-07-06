/*
** EPITECH PROJECT, 2025
** test_ex06.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(compare_s, test_compare_s)
{
    string_t s13, s14;
    string_init(&s13, "String 13");
    string_init(&s14, "String 13");
    cr_assert_eq(compare_s(&s13, &s14), 0);
    string_destroy(&s13);
    string_destroy(&s14);
}

Test(compare_s, test_compare_s_null)
{
    string_t s14;
    string_init(&s14, "String 14");
    string_destroy(&s14);
    cr_assert_eq(compare_s(&s14, &s14), -1);
}

Test(compare_c, test_compare_c)
{
    string_t s15;
    string_init(&s15, "String 15");
    cr_assert_eq(compare_c(&s15, "String 15"), 0);
    string_destroy(&s15);
}

Test(compare_c, test_compare_c_null)
{
    string_t s16;
    string_init(&s16, "String 16");
    string_destroy(&s16);
    cr_assert_eq(compare_c(&s16, "String 16"), -1);
}
