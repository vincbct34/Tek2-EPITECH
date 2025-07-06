/*
** EPITECH PROJECT, 2025
** ex02.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "int_list.h"

Test(int_list, int_list_get_elem_at_front)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_front(&list, 1);
    cr_assert_eq(int_list_get_elem_at_front(list), 1);
    int_list_clear(&list);
}

Test(int_list, int_list_get_elem_at_back)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    cr_assert_eq(int_list_get_elem_at_back(list), 1);
    int_list_clear(&list);
}

Test(int_list, int_list_get_elem_at_position)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    int_list_add_elem_at_back(&list, 2);
    cr_assert_eq(int_list_get_elem_at_position(list, 1), 2);
    int_list_clear(&list);
}