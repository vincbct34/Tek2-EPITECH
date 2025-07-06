/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** ai_vision_utils header
*/

#ifndef AI_VISION_UTILS_H_
    #define AI_VISION_UTILS_H_

    #include "game/game_types.h"
    #include "game/player_manager.h"
    #include "network/command_manager.h"

/**
 * @brief Calculates the vision coordinates for a player based on their level.
 *
 * @param player The player for whom to calculate vision coordinates.
 * @param level The level of the player.
 * @param coords The array to store the calculated coordinates.
 * @param tile_count The number of tiles in the vision range.
 */
void calculate_vision_coordinates(player_t *player,
    int level, int **coords, int *tile_count);

/**
 * @brief Builds the look response for a player.
 *
 * @param coords The array of coordinates to include in the response.
 * @param tile_count The number of tiles in the vision range.
 * @param state The current game state.
 * @param response The buffer to store the response.
 */
void build_look_response(int *coords,
    int tile_count, game_state_t *state, char *response);

/**
 * @brief Transforms relative coordinates to absolute coordinates.
 *
 * @param params The parameters for the transformation.
 * @param result The result of the transformation.
 */
void transform_relative_coordinates(const coord_transform_params_t *params,
    coord_transform_result_t *result);

/**
 * @brief Counts the number of players at a specific tile.
 *
 * @param state The current game state.
 * @param x The x-coordinate of the tile.
 * @param y The y-coordinate of the tile.
 * @param player_count Pointer to store the count of players.
 */
void count_players_at_tile(game_state_t *state,
    int x, int y, int *player_count);

/**
 * @brief Gets the tile at a specific position.
 *
 * @param state The current game state.
 * @param x The x-coordinate of the tile.
 * @param y The y-coordinate of the tile.
 * @return tile_t* Pointer to the tile, or NULL if not found.
 */
tile_t *get_tile_at(game_state_t *state, int x, int y);

/**
 * @brief Wraps a coordinate value around the x-axis.
 *
 * @param x The x-coordinate to wrap.
 * @param width The width of the map.
 * @return int The wrapped x-coordinate.
 */
int wrap_coordinate_x(int x, int width);

/**
 * @brief Wraps a coordinate value around the y-axis.
 *
 * @param y The y-coordinate to wrap.
 * @param height The height of the map.
 * @return int The wrapped y-coordinate.
 */
int wrap_coordinate_y(int y, int height);

/**
 * @brief Gets the name of a resource type.
 *
 * @param resource_type The resource type.
 * @return const char* The name of the resource type.
 */
const char *get_resource_name(resource_type_t resource_type);

/**
 * @brief Adds a single resource to the content string.
 *
 * @param temp The content string.
 * @param resource The resource to add.
 */
void add_single_resource_to_content(char *temp, int resource);

/**
 * @brief Processes a coordinate transformation.
 *
 * @param params The parameters for the transformation.
 * @param result The result of the transformation.
 */
void process_coordinate(const add_coords_params_t *params,
    int rel_x, int rel_y);

#endif /* !AI_VISION_UTILS_H_ */
