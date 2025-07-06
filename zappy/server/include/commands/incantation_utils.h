/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Incantation utilities header for AI commands
*/

#ifndef INCANTATION_UTILS_H_
    #define INCANTATION_UTILS_H_

    #include "game_types.h"
    #include "incantation_manager.h"
    #include <stdbool.h>

/**
 * @brief Count eligible players for incantation on the same tile
 * @param state Game state
 * @param player The player initiating incantation
 * @return Number of eligible players
 */
int count_eligible_players(game_state_t *state, player_t *player);

/**
 * @brief Check if tile has required resources for incantation
 * @param tile The tile to check
 * @param req The incantation requirements
 * @return 1 if requirements met, 0 otherwise
 */
bool check_tile_resources(tile_t *tile, const incantation_requirements_t *req);

/**
 * @brief Find an available ritual slot
 * @param state Game state
 * @return Index of available slot, or -1 if none
 */
int find_available_ritual_slot(game_state_t *state);

/**
 * @brief Setup a new ritual
 * @param ritual The ritual to setup
 * @param player The player initiating
 */
void setup_ritual(incantation_ritual_t *ritual, player_t *player);

/**
 * @brief Add eligible players to ritual
 * @param state Game state
 * @param player The initiating player
 * @param ritual The ritual
 * @param req The requirements
 */
void add_players_to_ritual(game_state_t *state, player_t *player,
    incantation_ritual_t *ritual, const incantation_requirements_t *req);

/**
 * @brief Check if enough players are on tile for incantation
 * @param state Game state
 * @param player The player initiating
 * @param req The incantation requirements
 * @return 0 if valid, -1 otherwise
 */
int check_player_count(game_state_t *state, player_t *player,
    const incantation_requirements_t *req);

/**
 * @brief Check all incantation requirements
 * @param state Game state
 * @param player The player initiating
 * @param req The incantation requirements
 * @return 0 if valid, -1 otherwise
 */
int check_incantation_requirements(game_state_t *state,
    player_t *player, const incantation_requirements_t *req);

/**
 * @brief Validate player and get requirements, send error if needed
 * @param player The player to validate
 * @param client The client connection
 * @param req Pointer to store requirements
 * @return 0 if valid, -1 otherwise
 */
int validate_and_get_requirements(player_t *player, client_t *client,
    const incantation_requirements_t **req);

/**
 * @brief Check ritual requirements and send error if needed
 * @param state Game state
 * @param player The player initiating
 * @param req The incantation requirements
 * @param client The client connection
 * @return 0 if valid, -1 otherwise
 */
int check_ritual_requirements(game_state_t *state, player_t *player,
    const incantation_requirements_t *req, client_t *client);

/**
 * @brief Allocate ritual slot and send error if needed
 * @param state Game state
 * @param client The client connection
 * @return Ritual slot index, or -1 if error
 */
int allocate_ritual_slot(game_state_t *state, client_t *client);

#endif /* !INCANTATION_UTILS_H_ */
