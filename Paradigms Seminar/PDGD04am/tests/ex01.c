/*
** EPITECH PROJECT, 2025
** ex01.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "test.h"

Test(mul_div_long, simple_test)
{
    int mul = 0;
    int div = 0;

    mul_div_long(4, 2, &mul, &div);
    cr_assert_eq(mul, 8);
    cr_assert_eq(div, 2);
}

Test(mul_div_short, simple_test)
{
    int a = 4;
    int b = 2;

    mul_div_short(&a, &b);
    cr_assert_eq(a, 8);
    cr_assert_eq(b, 2);
}

Test(concat_strings, simple_test)
{
    char *res = NULL;
    concat_strings("Hello, ", "World!", &res);
    cr_assert_str_eq(res, "Hello, World!");
    free(res);
}

Test(concat_struct, simple_test)
{
    concat_t str = {"Hello, ", "World!", NULL};
    concat_struct(&str);
    cr_assert_str_eq(str.res, "Hello, World!");
    free(str.res);
}
