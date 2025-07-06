/*
** EPITECH PROJECT, 2025
** test_ex04.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(clear, test_clear)
{
    string_t s11;
    string_init(&s11, "String 11");
    clear(&s11);
    cr_assert_str_empty(s11.str);
    string_destroy(&s11);
}
