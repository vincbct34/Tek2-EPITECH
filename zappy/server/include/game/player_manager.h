/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player management functions
*/

#ifndef PLAYER_MANAGER_H_
    #define PLAYER_MANAGER_H_

    #include "game_types.h"

/**
 * @brief Parameters for coordinate transformation
 */
typedef struct {
    int rel_x;
    int rel_y;
    int base_x;
    int base_y;
    orientation_t orientation;
} coord_transform_params_t;

/**
 * @brief Result of coordinate transformation
 */
typedef struct {
    int abs_x;
    int abs_y;
} coord_transform_result_t;

/**
 * @brief Create a new player for a client
 *
 * @param state Game state
 * @param client Client to create player for
 * @return Pointer to created player, NULL on error
 */
player_t *create_player_for_client(game_state_t *state, client_t *client);

/**
 * @brief Find a player by client file descriptor
 *
 * @param state Game state
 * @param client_fd Client file descriptor
 * @return Pointer to player, NULL if not found
 */
player_t *find_player_by_client_id(game_state_t *state, int client_fd);

/**
 * @brief Drop all resources from a player's inventory to the ground
 *
 * @param state Game state
 * @param player Player whose inventory should be dropped
 */
void drop_player_inventory(game_state_t *state, player_t *player);

/**
 * @brief Initialize player inventory with starting items
 *
 * @param player Player to initialize
 */
void initialize_player_inventory(player_t *player);

/**
 * @brief Place player at random position on map
 *
 * @param player Player to place
 * @param state Game state
 */
void place_player_random_position(player_t *player, game_state_t *state);

/**
 * @brief Move player in current direction
 *
 * @param player Player to move
 * @param state Game state
 * @return 0 on success, -1 on error
 */
int move_player_forward(player_t *player, game_state_t *state);

/**
 * @brief Turn player left
 *
 * @param player Player to turn
 */
void turn_player_left(player_t *player);

/**
 * @brief Turn player right
 *
 * @param player Player to turn
 */
void turn_player_right(player_t *player);

/**
 * @brief Get tile at specific coordinates
 *
 * @param state Game state
 * @param x X coordinate
 * @param y Y coordinate
 * @return Pointer to tile, NULL if out of bounds
 */
tile_t *get_tile_at(game_state_t *state, int x, int y);

/**
 * @brief Remove player from game state
 *
 * @param state Game state
 * @param player_id Player ID to remove
 */
void remove_player_from_game(game_state_t *state, int player_id);

/**
 * @brief Transform relative coordinates
 * to absolute coordinates based on orientation
 *
 * @param params Transformation parameters
 * @param result Pointer to store transformation result
 */
void transform_relative_coordinates(const coord_transform_params_t *params,
    coord_transform_result_t *result);

/**
 * @brief Find a client by player ID
 *
 * @param state Game state
 * @param player_id Player ID to find
 * @return Pointer to client, NULL if not found
 */
client_t *find_client_by_player_id(game_state_t *state, int player_id);

/**
 * @brief Count players at a specific tile
 *
 * @param state Game state
 * @param x X coordinate
 * @param y Y coordinate
 * @param player_count Pointer to store the player count result
 */
void count_players_at_tile(game_state_t *state,
    int x, int y, int *player_count);

#endif /* !PLAYER_MANAGER_H_ */
