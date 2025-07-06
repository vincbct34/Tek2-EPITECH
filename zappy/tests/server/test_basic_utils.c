/*
** EPITECH PROJECT, 2025
** Zappy Tests
** File description:
** Simple unit tests for basic utility functions
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "game_types.h"
#include "tile_utils.h"
#include "resource_utils.h"

Test(basic_utils, wrap_coordinate_x_normal)
{
    int result = wrap_coordinate_x(5, 10);

    cr_assert_eq(result, 5);
}

Test(basic_utils, wrap_coordinate_x_negative)
{
    int result = wrap_coordinate_x(-1, 10);

    cr_assert_eq(result, 9);
}

Test(basic_utils, wrap_coordinate_x_overflow)
{
    int result = wrap_coordinate_x(10, 10);

    cr_assert_eq(result, 0);
}

Test(basic_utils, wrap_coordinate_y_normal)
{
    int result = wrap_coordinate_y(3, 8);

    cr_assert_eq(result, 3);
}

Test(basic_utils, wrap_coordinate_y_negative)
{
    int result = wrap_coordinate_y(-1, 8);

    cr_assert_eq(result, 7);
}

Test(basic_utils, wrap_coordinate_y_overflow)
{
    int result = wrap_coordinate_y(8, 8);

    cr_assert_eq(result, 0);
}

Test(basic_utils, orientation_values)
{
    cr_assert_eq(ORIENTATION_NORTH, 1);
    cr_assert_eq(ORIENTATION_EAST, 2);
    cr_assert_eq(ORIENTATION_SOUTH, 3);
    cr_assert_eq(ORIENTATION_WEST, 4);
}

Test(basic_utils, resource_values)
{
    cr_assert_eq(RESOURCE_FOOD, 0);
    cr_assert_eq(RESOURCE_LINEMATE, 1);
    cr_assert_eq(RESOURCE_DERAUMERE, 2);
    cr_assert_eq(RESOURCE_SIBUR, 3);
    cr_assert_eq(RESOURCE_MENDIANE, 4);
    cr_assert_eq(RESOURCE_PHIRAS, 5);
    cr_assert_eq(RESOURCE_THYSTAME, 6);
}
