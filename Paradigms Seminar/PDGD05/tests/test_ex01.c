/*
** EPITECH PROJECT, 2025
** test_ex01.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(assign_c, test_str)
{
    string_t s1;
    string_init(&s1, "String 1");
    assign_c(&s1, "String 2");
    cr_assert_str_eq(s1.str, "String 2");
    string_destroy(&s1);
}

Test(assign_s, test_str)
{
    string_t s5, s6;
    string_init(&s5, "String 5");
    string_init(&s6, "String 6");
    assign_s(&s5, &s6);
    cr_assert_str_eq(s5.str, "String 6");
    string_destroy(&s5);
    string_destroy(&s6);
}
