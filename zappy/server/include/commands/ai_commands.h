/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI commands headers for refactored object manipulation
*/

#ifndef AI_COMMANDS_H_
    #define AI_COMMANDS_H_

    #include "game_types.h"

/**
 * @brief AI command to take an object from the tile
 * @param state Game state
 * @param client Client connection
 * @param args Resource name to take
 * @return 0 on success, -1 on failure
 */
int ai_take(game_state_t *state, client_t *client, const char *args);

/**
 * @brief AI command to set down an object on the tile
 * @param state Game state
 * @param client Client connection
 * @param args Resource name to set down
 * @return 0 on success, -1 on failure
 */
int ai_set(game_state_t *state, client_t *client, const char *args);

/**
 * @brief AI command to start incantation
 * @param state Game state
 * @param client Client connection
 * @param args Not used
 * @return 0 on success, -1 on failure
 */
int ai_incantation(game_state_t *state, client_t *client, const char *args);

#endif /* !AI_COMMANDS_H_ */
