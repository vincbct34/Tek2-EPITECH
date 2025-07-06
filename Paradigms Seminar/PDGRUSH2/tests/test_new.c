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

Test(new, test_new)
{
    Object *player = new(Player, "player");
    Object *point = new(Point, 0, 1);
    Object *vertex = new(Vertex, 0, 1, 2);
    Object *character = new(Char, 'a');
    Object *integer = new(Int, 42);
    Object *floating = new(Float, 42.42);

    cr_assert_not_null(player);
    cr_assert_not_null(point);
    cr_assert_not_null(vertex);
    cr_assert_not_null(character);
    cr_assert_not_null(integer);
    cr_assert_not_null(floating);

    delete(player);
    delete(point);
    delete(vertex);
    delete(character);
    delete(integer);
    delete(floating);
}

Test(delete, test_delete_null)
{
    delete(NULL);
    cr_assert(1);
}

Test(delete, test_delete_valid)
{
    Object *player = new(Player, "player");
    Object *point = new(Point, 0, 1);
    Object *vertex = new(Vertex, 0, 1, 2);
    Object *character = new(Char, 'a');
    Object *integer = new(Int, 42);
    Object *floating = new(Float, 42.42);

    cr_assert_not_null(player);
    cr_assert_not_null(point);
    cr_assert_not_null(vertex);
    cr_assert_not_null(character);
    cr_assert_not_null(integer);
    cr_assert_not_null(floating);

    delete(player);
    delete(point);
    delete(vertex);
    delete(character);
    delete(integer);
    delete(floating);
    cr_assert(1);
}
