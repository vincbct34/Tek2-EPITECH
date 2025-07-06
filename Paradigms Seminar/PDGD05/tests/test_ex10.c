/*
** EPITECH PROJECT, 2025
** test_ex10.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(find_s, test_find_s)
{
    string_t s20, s21;
    string_init(&s20, "Find this string");
    string_init(&s21, "this");
    cr_assert_eq(find_s(&s20, &s21, 0), 5);
    string_destroy(&s20);
}

Test(find_s, test_find_s_null)
{
    string_t s20, s21;
    string_init(&s20, "Find this string");
    string_init(&s21, "all");
    string_destroy(&s20);
    string_destroy(&s21);
    cr_assert_eq(find_s(&s20, &s21, 0), -1);
}

Test(find_c, test_find_c)
{
    string_t s22;
    string_init(&s22, "Find this string");
    cr_assert_eq(find_c(&s22, "this", 0), 5);
    string_destroy(&s22);
}

Test(find_c, test_find_c_null)
{
    string_t s22;
    string_init(&s22, "Find this string");
    string_destroy(&s22);
    cr_assert_eq(find_c(&s22, "this", 0), -1);
}
