/*
** EPITECH PROJECT, 2025
** test_ex02.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(append_c, test_append_c)
{
    string_t s7;
    string_init(&s7, "Initial String");
    append_c(&s7, "Appended String");
    cr_assert_str_eq(s7.str, "Initial StringAppended String");
    string_destroy(&s7);
}

Test(append_s, test_append_s)
{
    string_t s8, s9;
    string_init(&s8, "String 8");
    string_init(&s9, "String 9");
    append_s(&s8, &s9);
    cr_assert_str_eq(s8.str, "String 8String 9");
    string_destroy(&s8);
    string_destroy(&s9);
}
