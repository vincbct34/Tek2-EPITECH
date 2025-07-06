/*
** EPITECH PROJECT, 2025
** ex01.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "int_list.h"

Test(int_list, int_list_add_elem_at_front)
{
    int_list_t *list = NULL;
    cr_assert(int_list_add_elem_at_front(&list, 1));
    cr_assert_eq(int_list_get_size(list), 1);
    int_list_clear(&list);
}

Test(int_list, int_list_add_elem_at_position)
{
    int_list_t *list = NULL;
    cr_assert(int_list_add_elem_at_position(&list, 1, 0));
    cr_assert_eq(int_list_get_size(list), 1);
    cr_assert(int_list_add_elem_at_position(&list, 2, 1));
    cr_assert_eq(int_list_get_size(list), 2);
    int_list_clear(&list);
}