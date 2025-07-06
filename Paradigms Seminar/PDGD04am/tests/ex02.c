/*
** EPITECH PROJECT, 2025
** ex02.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "test.h"
#include "../ex02/concat.h"

Test(concat_strings, test_concat_strings)
{
    char *res = NULL;
    concat_strings("Hello", "World", &res);
    cr_assert_str_eq(res, "HelloWorld");
}

Test(concat_struct, test_concat_struct)
{
    concat_t str = {NULL, NULL, NULL};
    str.str1 = "Hello";
    str.str2 = "World";
    concat_struct(&str);
    cr_assert_str_eq(str.res, "HelloWorld");
}
