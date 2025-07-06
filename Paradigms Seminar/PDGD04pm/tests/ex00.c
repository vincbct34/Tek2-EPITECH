/*
** EPITECH PROJECT, 2025
** ex00.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "int_list.h"

Test(int_list_add_elem_at_back, add_single_element)
{
    int_list_t *list = NULL;
    cr_assert(int_list_add_elem_at_back(&list, 42));
    cr_assert_eq(list->value, 42);
    cr_assert_null(list->next);
    int_list_clear(&list);
}

Test(int_list_add_elem_at_back, add_multiple_elements)
{
    int_list_t *list = NULL;
    cr_assert(int_list_add_elem_at_back(&list, 42));
    cr_assert(int_list_add_elem_at_back(&list, 84));
    cr_assert_eq(list->value, 42);
    cr_assert_eq(list->next->value, 84);
    cr_assert_null(list->next->next);
    int_list_clear(&list);
}

Test(int_list_get_size, empty_list)
{
    int_list_t *list = NULL;
    cr_assert_eq(int_list_get_size(list), 0);
}

Test(int_list_get_size, non_empty_list)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 42);
    int_list_add_elem_at_back(&list, 84);
    cr_assert_eq(int_list_get_size(list), 2);
    int_list_clear(&list);
}

Test(int_list_is_empty, empty_list)
{
    int_list_t *list = NULL;
    cr_assert(int_list_is_empty(list));
}

Test(int_list_is_empty, non_empty_list)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 42);
    cr_assert_not(int_list_is_empty(list));
    int_list_clear(&list);
}

Test(int_list_clear, clear_list)
{
    int_list_t *list = NULL;
    int_list_add_elem_at_back(&list, 42);
    int_list_add_elem_at_back(&list, 84);
    int_list_clear(&list);
    cr_assert_null(list);
}
