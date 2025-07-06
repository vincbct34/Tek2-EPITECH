/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Tile utilities header
*/

#ifndef TILE_UTILS_H_
    #define TILE_UTILS_H_

    #include "game_types.h"

/**
 * @brief Get the character representation of a tile.
 *
 * @param tile Tile to get the character for
 * @return char Character representing the tile
 */
char get_tile_char(tile_t *tile);

/**
 * @brief Get the color representation of a tile.
 *
 * @param tile Tile to get the color for
 * @return const char* Color code for the tile
 */
const char *get_tile_color(tile_t *tile);

/**
 * @brief Wraps the x-coordinate to ensure it stays within the map width.
 *
 * @param x X-coordinate to wrap
 * @param width Width of the map
 * @return int Wrapped x-coordinate
 */
int wrap_coordinate_x(int x, int width);

/**
 * @brief Wraps the y-coordinate to ensure it stays within the map height.
 *
 * @param y Y-coordinate to wrap
 * @param height Height of the map
 * @return int Wrapped y-coordinate
 */
int wrap_coordinate_y(int y, int height);

#endif /* !TILE_UTILS_H_ */
