/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Incantation ritual management header
*/

#ifndef INCANTATION_MANAGER_H_
    #define INCANTATION_MANAGER_H_

    #include "game_types.h"
    #include <stdbool.h>

    #define INCANTATION_DURATION_TICKS 300

/**
 * @brief Find a player by their ID.
 *
 * @param state Game state
 * @param player_id ID of the player to find
 * @return player_t* Pointer to the found player, or NULL if not found
 */
player_t *find_player_by_id(game_state_t *state, int player_id);

/**
 * @brief Consume resources from a tile for an incantation
 *
 * @param tile Tile to consume resources from
 * @param req Incantation requirements
 */
void consume_tile_resources(tile_t *tile,
    const incantation_requirements_t *req);

/**
 * @brief Incantation requirements per level
 */
extern const incantation_requirements_t incantation_levels[8];

/**
 * @brief Process all active incantation rituals
 *
 * This function should be called regularly to check ritual timers
 * and complete or fail rituals as appropriate
 *
 * @param state Game state containing active rituals
 */
void process_incantation_rituals(game_state_t *state);

/**
 * @brief Verify if a ritual is still valid
 *
 * @param state Game state
 * @param ritual Ritual to verify
 * @return bool true if valid, false otherwise
 */
bool verify_ritual_still_valid(game_state_t *state,
    incantation_ritual_t *ritual);

/**
 * @brief Initialize the incantation system
 *
 * @param state Game state to initialize
 */
void initialize_incantation_system(game_state_t *state);

/**
 * @brief Complete a ritual successfully
 *
 * @param state Game state
 * @param ritual Ritual to complete
 */
void complete_ritual_success(game_state_t *state,
    incantation_ritual_t *ritual);

/**
 * @brief Complete a ritual with failure
 *
 * @param state Game state
 * @param ritual Ritual to fail
 */
void complete_ritual_failure(game_state_t *state,
    incantation_ritual_t *ritual);

#endif /* !INCANTATION_MANAGER_H_ */
