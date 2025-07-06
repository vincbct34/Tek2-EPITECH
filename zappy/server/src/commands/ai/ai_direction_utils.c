/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Direction calculation utilities for AI commands
*/

#include "ai_direction_utils.h"
#include "game_types.h"

static void normalize_coordinates(int *dx, int *dy, map_dimensions_t *map)
{
    if (*dx > map->width / 2)
        *dx -= map->width;
    if (*dx < (-map->width / 2))
        *dx += map->width;
    if (*dy > map->height / 2)
        *dy -= map->height;
    if (*dy < (-map->height / 2))
        *dy += map->height;
}

static int get_base_direction(int dx, int dy)
{
    if (dx == 0 && dy == 0)
        return 0;
    if (dy < 0 && dx == 0)
        return 1;
    if (dy < 0 && dx > 0)
        return 2;
    if (dy == 0 && dx > 0)
        return 3;
    if (dy > 0 && dx > 0)
        return 4;
    if (dy > 0 && dx == 0)
        return 5;
    if (dy > 0 && dx < 0)
        return 6;
    if (dy == 0 && dx < 0)
        return 7;
    return 8;
}

static int adjust_direction_for_orientation(int direction, int orientation)
{
    direction = ((direction - orientation + 8) % 8);
    if (direction == 0)
        direction = 8;
    return direction;
}

int calculate_direction(position_pair_t *positions, map_dimensions_t *map,
    int listener_orientation)
{
    int dx = positions->dst_x - positions->src_x;
    int dy = positions->dst_y - positions->src_y;
    int direction;

    normalize_coordinates(&dx, &dy, map);
    direction = get_base_direction(dx, dy);
    return adjust_direction_for_orientation(direction, listener_orientation);
}

void calculate_new_position(position_info_t *pos, map_dimensions_t *map)
{
    switch (pos->orientation) {
        case ORIENTATION_NORTH:
            pos->y = (pos->y - 1 + map->height) % map->height;
            break;
        case ORIENTATION_EAST:
            pos->x = (pos->x + 1) % map->width;
            break;
        case ORIENTATION_SOUTH:
            pos->y = (pos->y + 1) % map->height;
            break;
        case ORIENTATION_WEST:
            pos->x = (pos->x - 1 + map->width) % map->width;
            break;
    }
}
