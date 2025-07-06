/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "list.h"
#include <stdio.h>

Test(list, list_get_size)
{
    list_t *list = NULL;
    cr_assert_eq(list_get_size(list), 0);
    list_add_elem_at_front(&list, (void *)1);
    cr_assert_eq(list_get_size(list), 1);
    list_add_elem_at_back(&list, (void *)2);
    cr_assert_eq(list_get_size(list), 2);
    list_clear(&list);
}

Test(list, list_is_empty)
{
    list_t *list = NULL;
    cr_assert(list_is_empty(list));
    list_add_elem_at_front(&list, (void *)1);
    cr_assert_not(list_is_empty(list));
    list_clear(&list);
}

Test(list, list_add_elem_at_front)
{
    list_t *list = NULL;
    cr_assert(list_add_elem_at_front(&list, (void *)1));
    cr_assert_eq(list_get_size(list), 1);
    list_clear(&list);
}

Test(list, list_add_elem_at_back)
{
    list_t *list = NULL;
    cr_assert(list_add_elem_at_back(&list, (void *)1));
    cr_assert_eq(list_get_size(list), 1);
    list_clear(&list);
}

Test(list, list_add_elem_at_position)
{
    list_t *list = NULL;
    cr_assert(list_add_elem_at_position(&list, (void *)1, 0));
    cr_assert_eq(list_get_size(list), 1);
    cr_assert(list_add_elem_at_position(&list, (void *)2, 1));
    cr_assert_eq(list_get_size(list), 2);
    list_clear(&list);
}

Test(list, list_del_elem_at_front)
{
    list_t *list = NULL;
    list_add_elem_at_front(&list, (void *)1);
    cr_assert(list_del_elem_at_front(&list));
    cr_assert_eq(list_get_size(list), 0);
    cr_assert_not(list_del_elem_at_front(&list));
    list_clear(&list);
}

Test(list, list_del_elem_at_back)
{
    list_t *list = NULL;
    list_add_elem_at_back(&list, (void *)1);
    cr_assert(list_del_elem_at_back(&list));
    cr_assert_eq(list_get_size(list), 0);
    cr_assert_not(list_del_elem_at_back(&list));
    list_clear(&list);
}

Test(list, list_del_elem_at_position)
{
    list_t *list = NULL;
    list_add_elem_at_back(&list, (void *)1);
    list_add_elem_at_back(&list, (void *)2);
    cr_assert(list_del_elem_at_position(&list, 1));
    cr_assert_eq(list_get_size(list), 1);
    cr_assert(list_del_elem_at_position(&list, 0));
    cr_assert_eq(list_get_size(list), 0);
    cr_assert_not(list_del_elem_at_position(&list, 0));
    list_clear(&list);
}

Test(list, list_clear)
{
    list_t *list = NULL;
    list_add_elem_at_back(&list, (void *)1);
    list_add_elem_at_back(&list, (void *)2);
    list_clear(&list);
    cr_assert_eq(list_get_size(list), 0);
}

Test(list, list_get_elem_at_front)
{
    list_t *list = NULL;
    list_add_elem_at_front(&list, (void *)1);
    cr_assert_eq(list_get_elem_at_front(list), (void *)1);
    list_clear(&list);
}

Test(list, list_get_elem_at_back)
{
    list_t *list = NULL;
    list_add_elem_at_back(&list, (void *)1);
    cr_assert_eq(list_get_elem_at_back(list), (void *)1);
    list_clear(&list);
}

Test(list, list_get_elem_at_position)
{
    list_t *list = NULL;
    list_add_elem_at_back(&list, (void *)1);
    list_add_elem_at_back(&list, (void *)2);
    cr_assert_eq(list_get_elem_at_position(list, 1), (void *)2);
    list_clear(&list);
}
