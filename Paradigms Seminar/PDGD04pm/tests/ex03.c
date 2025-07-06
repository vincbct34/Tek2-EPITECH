/*
** EPITECH PROJECT, 2025
** ex03.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "int_list.h"

Test(int_list, int_list_get_size)
{
    int_list_t *list = NULL;
    cr_assert_eq(int_list_get_size(list), 0);
    int_list_add_elem_at_front(&list, 1);
    cr_assert_eq(int_list_get_size(list), 1);
    int_list_add_elem_at_back(&list, 2);
    cr_assert_eq(int_list_get_size(list), 2);
    int_list_clear(&list);
}

Test(int_list, int_list_is_empty)
{
    int_list_t *list = NULL;
    cr_assert(int_list_is_empty(list));
    int_list_add_elem_at_front(&list, 1);
    cr_assert_not(int_list_is_empty(list));
    int_list_clear(&list);
}

Test(int_list, int_list_add_elem_at_back)
{
    int_list_t *list = NULL;
    cr_assert(int_list_add_elem_at_back(&list, 1));
    cr_assert_eq(int_list_get_size(list), 1);
    int_list_clear(&list);
}

Test(int_list, int_list_del_elem_at_front)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_front(&list, 1);
    cr_assert(int_list_del_elem_at_front(&list));
    cr_assert_eq(int_list_get_size(list), 0);
    cr_assert_not(int_list_del_elem_at_front(&list));
    int_list_clear(&list);
}

Test(int_list, int_list_del_elem_at_back)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    cr_assert(int_list_del_elem_at_back(&list));
    cr_assert_eq(int_list_get_size(list), 0);
    cr_assert_not(int_list_del_elem_at_back(&list));
    int_list_clear(&list);
}

Test(int_list, int_list_del_elem_at_position)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    int_list_add_elem_at_back(&list, 2);
    cr_assert(int_list_del_elem_at_position(&list, 1));
    cr_assert_eq(int_list_get_size(list), 1);
    cr_assert(int_list_del_elem_at_position(&list, 0));
    cr_assert_eq(int_list_get_size(list), 0);
    cr_assert_not(int_list_del_elem_at_position(&list, 0));
    int_list_clear(&list);
}

Test(int_list, int_list_clear)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    int_list_add_elem_at_back(&list, 2);
    int_list_clear(&list);
    cr_assert_eq(int_list_get_size(list), 0);
}

Test(int_list, int_list_dump)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 1);
    int_list_add_elem_at_back(&list, 2);
    int_list_dump(list);
    int_list_clear(&list);
}