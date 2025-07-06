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
    int     x, y;
}   PointClass;

Test(Point, creation)
{
    Object *point = new(Point, 1, 2);

    cr_assert_not_null(point);
    cr_assert_str_eq(str(point), "<Point (1, 2)>");
    delete(point);
}

Test(Point, addition)
{
    Object *point1 = new(Point, 1, 2);
    Object *point2 = new(Point, 3, 4);
    Object *result = addition(point1, point2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Point (4, 6)>");
    delete(point1);
    delete(point2);
    delete(result);
}

Test(Point, subtraction)
{
    Object *point1 = new(Point, 5, 7);
    Object *point2 = new(Point, 2, 3);
    Object *result = subtraction(point1, point2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Point (3, 4)>");
    delete(point1);
    delete(point2);
    delete(result);
}

Test(Point, string_representation)
{
    Object *point = new(Point, 8, 9);

    cr_assert_not_null(point);
    cr_assert_str_eq(str(point), "<Point (8, 9)>");
    delete(point);
}
