/*
** EPITECH PROJECT, 2025
** ex03.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "test.h"

Test(array_1d_to_2d, test_array_1d_to_2d)
{
    int array[] = {1, 2, 3, 4, 5, 6};
    int **res = NULL;
    array_1d_to_2d(array, 2, 3, &res);
    cr_assert_eq(res[0][0], 1);
    cr_assert_eq(res[0][1], 2);
    cr_assert_eq(res[0][2], 3);
    cr_assert_eq(res[1][0], 4);
    cr_assert_eq(res[1][1], 5);
    cr_assert_eq(res[1][2], 6);
    array_2d_free(res, 2, 3);
}
