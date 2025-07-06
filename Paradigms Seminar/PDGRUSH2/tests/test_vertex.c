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
    int     x, y, z;
}   VertexClass;

Test(Vertex, creation)
{
    Object  *v = new(Vertex, 1, 2, 3);

    cr_assert_not_null(v);
    delete(v);
}

Test(Vertex, string_representation)
{
    Object  *v = new(Vertex, 1, 2, 3);
    char    *str = str(v);

    cr_assert_str_eq(str, "<Vertex (1, 2, 3)>");
    free(str);
    delete(v);
}

Test(Vertex, addition)
{
    Object  *v1 = new(Vertex, 1, 2, 3);
    Object  *v2 = new(Vertex, 4, 5, 6);
    Object  *result = addition(v1, v2);
    char    *str = str(result);

    cr_assert_str_eq(str, "<Vertex (5, 7, 9)>");
    free(str);
    delete(v1);
    delete(v2);
    delete(result);
}

Test(Vertex, subtraction)
{
    Object  *v1 = new(Vertex, 4, 5, 6);
    Object  *v2 = new(Vertex, 1, 2, 3);
    Object  *result = subtraction(v1, v2);
    char    *str = str(result);

    cr_assert_str_eq(str, "<Vertex (3, 3, 3)>");
    free(str);
    delete(v1);
    delete(v2);
    delete(result);
}
