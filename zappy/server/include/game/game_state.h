/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_state
*/

#ifndef GAME_STATE_H_
    #define GAME_STATE_H_

    #include "game_types.h"
    #include "resource_manager.h"
    #include "player_manager.h"
    #include "time_manager.h"

/**
 * @brief Initialize the game state
 *
 * @param state Game state to initialize
 * @param config Server configuration
 * @return 0 on success, -1 on error
 */
int init_game_state(game_state_t *state, server_config_t *config);

/**
 * @brief Free the game state
 *
 * @param state Game state to free
 */
void free_game_state(game_state_t *state);

/**
 * @brief Add a client to the game state
 *
 * @param state Game state
 * @param fd Client file descriptor
 * @return Pointer to the client, NULL on error
 */
client_t *add_client(game_state_t *state, int fd);

/**
 * @brief Remove a client from the game state
 *
 * @param state Game state
 * @param fd Client file descriptor
 */
void remove_client(game_state_t *state, int fd);

/**
 * @brief Find a client by file descriptor
 *
 * @param state Game state
 * @param fd Client file descriptor
 * @return Pointer to the client, NULL if not found
 */
client_t *find_client(game_state_t *state, int fd);

/**
 * @brief Print the game state initialization in a nice format
 *
 * @param state Game state to print
 */
void print_game_state_init(const game_state_t *state);

#endif /* !GAME_STATE_H_ */
