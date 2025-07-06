/*
** EPITECH PROJECT, 2025
** Test
** File description:
** base
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "../new.h"
#include "../player.h"
#include "../point.h"
#include "../vertex.h"
#include "../char.h"
#include "../int.h"
#include "../float.h"
#include "../array.h"
#include "../raise.h"
#include "../iterator.h"

typedef struct
{
    Class   base;
    int nb;
}   IntClass;

Test(IntClass, creation)
{
    Object *i = new(Int, 42);

    cr_assert_not_null(i);
    delete(i);
}

Test(IntClass, addition)
{
    Object *i1 = new(Int, 42);
    Object *i2 = new(Int, 58);
    Object *result = addition(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, 100);
    delete(i1);
    delete(i2);
    delete(result);
}

Test(IntClass, subtraction)
{
    Object *i1 = new(Int, 100);
    Object *i2 = new(Int, 58);
    Object *result = subtraction(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, 42);
    delete(i1);
    delete(i2);
    delete(result);
}

Test(IntClass, multiplication)
{
    Object *i1 = new(Int, 6);
    Object *i2 = new(Int, 7);
    Object *result = multiplication(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, 42);
    delete(i1);
    delete(i2);
    delete(result);
}

Test(IntClass, division)
{
    Object *i1 = new(Int, 84);
    Object *i2 = new(Int, 2);
    Object *result = division(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, 42);
    delete(i1);
    delete(i2);
    delete(result);
}

Test(IntClass, equality)
{
    Object *i1 = new(Int, 42);
    Object *i2 = new(Int, 42);
    Object *i3 = new(Int, 43);

    cr_assert(eq(i1, i2));
    cr_assert_not(eq(i1, i3));
    delete(i1);
    delete(i2);
    delete(i3);
}

Test(IntClass, greater_than)
{
    Object *i1 = new(Int, 43);
    Object *i2 = new(Int, 42);

    cr_assert(gt(i1, i2));
    delete(i1);
    delete(i2);
}

Test(IntClass, less_than)
{
    Object *i1 = new(Int, 41);
    Object *i2 = new(Int, 42);

    cr_assert(lt(i1, i2));
    delete(i1);
    delete(i2);
}

Test(IntClass, to_string)
{
    Object *i = new(Int, 42);
    char *str = str(i);

    cr_assert_not_null(str);
    cr_assert_str_eq(str, "<Int (42)>");
    free(str);
    delete(i);
}

Test(IntClass, addition_negative)
{
    Object *i1 = new(Int, -42);
    Object *i2 = new(Int, 58);
    Object *result = addition(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, 16);
    delete(i1);
    delete(i2);
    delete(result);
}

Test(IntClass, subtraction_negative)
{
    Object *i1 = new(Int, 42);
    Object *i2 = new(Int, 58);
    Object *result = subtraction(i1, i2);

    cr_assert_not_null(result);
    cr_assert_eq(((IntClass *)result)->nb, -16);
    delete(i1);
    delete(i2);
    delete(result);
}

