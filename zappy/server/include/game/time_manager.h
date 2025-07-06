/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** time_manager
*/

#ifndef TIME_MANAGER_H_
    #define TIME_MANAGER_H_

    #include "game_types.h"

/**
 * @brief Get current time in milliseconds
 *
 * @return Current time in milliseconds
 */
long long get_current_time_ms(void);

/**
 * @brief Check if an action is ready to execute
 *
 * @param start_time Action start time
 * @param duration Action duration in milliseconds
 * @return true if ready, false otherwise
 */
bool is_action_ready(const struct timeval *start_time, long long duration);

/**
 * @brief Update player survival times
 *
 * @param state Game state
 */
void update_player_survival(game_state_t *state);

/**
 * @brief Spawn resources on the map
 *
 * @param state Game state
 */
void spawn_resources(game_state_t *state);

#endif /* !TIME_MANAGER_H_ */
