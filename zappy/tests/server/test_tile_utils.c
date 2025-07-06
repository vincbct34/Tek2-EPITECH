/*
** EPITECH PROJECT, 2025
** Zappy Tests
** File description:
** Unit tests for tile utilities
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "tile_utils.h"
#include "game_types.h"

Test(tile_utils, wrap_coordinate_x_normal)
{
    int result = wrap_coordinate_x(5, 10);

    cr_assert_eq(result, 5);
}

Test(tile_utils, wrap_coordinate_x_negative)
{
    int result = wrap_coordinate_x(-1, 10);

    cr_assert_eq(result, 9);
}

Test(tile_utils, wrap_coordinate_x_overflow)
{
    int result = wrap_coordinate_x(10, 10);

    cr_assert_eq(result, 0);
}

Test(tile_utils, wrap_coordinate_x_large_negative)
{
    int result = wrap_coordinate_x(-15, 10);

    cr_assert_eq(result, 5);
}

Test(tile_utils, wrap_coordinate_x_large_positive)
{
    int result = wrap_coordinate_x(25, 10);

    cr_assert_eq(result, 5);
}

Test(tile_utils, wrap_coordinate_y_normal)
{
    int result = wrap_coordinate_y(3, 8);

    cr_assert_eq(result, 3);
}

Test(tile_utils, wrap_coordinate_y_negative)
{
    int result = wrap_coordinate_y(-1, 8);

    cr_assert_eq(result, 7);
}

Test(tile_utils, wrap_coordinate_y_overflow)
{
    int result = wrap_coordinate_y(8, 8);

    cr_assert_eq(result, 0);
}

Test(tile_utils, wrap_coordinate_y_large_negative)
{
    int result = wrap_coordinate_y(-10, 8);

    cr_assert_eq(result, 6);
}

Test(tile_utils, wrap_coordinate_y_large_positive)
{
    int result = wrap_coordinate_y(20, 8);

    cr_assert_eq(result, 4);
}

Test(tile_utils, get_tile_char_empty_tile)
{
    tile_t tile;
    memset(&tile, 0, sizeof(tile_t));
    
    char result = get_tile_char(&tile);
    
    cr_assert_eq(result, '.');
}

Test(tile_utils, get_tile_char_with_food)
{
    tile_t tile;
    memset(&tile, 0, sizeof(tile_t));
    tile.resources[RESOURCE_FOOD] = 1;
    
    char result = get_tile_char(&tile);
    
    cr_assert_eq(result, '#'); // Food is represented by '#'
}

Test(tile_utils, get_tile_char_with_linemate)
{
    tile_t tile;
    memset(&tile, 0, sizeof(tile_t));
    tile.resources[RESOURCE_LINEMATE] = 1;
    
    char result = get_tile_char(&tile);
    
    cr_assert_eq(result, 'L');
}

Test(tile_utils, get_tile_color_empty_tile)
{
    tile_t tile;
    memset(&tile, 0, sizeof(tile_t));
    
    const char *result = get_tile_color(&tile);
    
    cr_assert_str_eq(result, GRAY);
}

Test(tile_utils, get_tile_color_with_resources)
{
    tile_t tile;
    memset(&tile, 0, sizeof(tile_t));
    tile.resources[RESOURCE_FOOD] = 1;
    
    const char *result = get_tile_color(&tile);
    
    cr_assert_str_eq(result, GREEN);
}
