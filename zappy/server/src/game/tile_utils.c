/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Tile utilities implementation
*/

#include "tile_utils.h"

static int count_tile_resources(tile_t *tile, int *primary_resource)
{
    int resource_count = 0;
    int r;

    *primary_resource = -1;
    for (r = 0; r < MAX_RESOURCES; r++) {
        if (tile->resources[r] <= 0)
            continue;
        resource_count++;
        if (*primary_resource == -1)
            *primary_resource = r;
    }
    return resource_count;
}

char get_tile_char(tile_t *tile)
{
    static const char chars[] = {'#', 'L', 'D', 'S', 'M', 'P', 'T'};
    int resource_count;
    int primary_resource;

    if (!tile)
        return '?';
    resource_count = count_tile_resources(tile, &primary_resource);
    if (resource_count == 0)
        return '.';
    if (resource_count > 1)
        return '+';
    if (primary_resource >= 0 && primary_resource < MAX_RESOURCES)
        return chars[primary_resource];
    return '?';
}

const char *get_tile_color(tile_t *tile)
{
    static const char *colors[] =
        {GREEN, YELLOW, BLUE, CYAN, MAGENTA, RED, WHITE};
    int resource_count;
    int primary_resource;

    if (!tile)
        return GRAY;
    resource_count = count_tile_resources(tile, &primary_resource);
    if (resource_count == 0)
        return GRAY;
    if (resource_count > 1)
        return YELLOW;
    if (primary_resource >= 0 && primary_resource < MAX_RESOURCES)
        return colors[primary_resource];
    return GRAY;
}

int wrap_coordinate_x(int x, int width)
{
    return ((x % width) + width) % width;
}

int wrap_coordinate_y(int y, int height)
{
    return ((y % height) + height) % height;
}
