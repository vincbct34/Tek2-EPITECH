/*
** EPITECH PROJECT, 2025
** test_ex03.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include <criterion/criterion.h>
#include "../string.h"
#include <string.h>

Test(at_pos, test_at_pos)
{
    string_t s10;
    string_init(&s10, "String 10");
    cr_assert_eq(at_pos(&s10, 0), 'S');
    cr_assert_eq(at_pos(&s10, 1), 't');
    cr_assert_eq(at_pos(&s10, 2), 'r');
    cr_assert_eq(at_pos(&s10, 3), 'i');
    cr_assert_eq(at_pos(&s10, 4), 'n');
    cr_assert_eq(at_pos(&s10, 5), 'g');
    cr_assert_eq(at_pos(&s10, 6), ' ');
    cr_assert_eq(at_pos(&s10, 7), '1');
    cr_assert_eq(at_pos(&s10, 8), '0');
    cr_assert_eq(at_pos(&s10, 15), -1);
    string_destroy(&s10);
}
