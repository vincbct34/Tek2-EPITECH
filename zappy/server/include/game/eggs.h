/*
** EPITECH PROJECT, 2025
** server
** File description:
** eggs
*/

#ifndef EGGS_H_
    #define EGGS_H_

    #include "game_types.h"
    #include <stdbool.h>

    #define EGG_HATCH_TIME 600 // 600 time units to hatch

/**
 * @brief Parameters for creating an egg
 */
typedef struct {
    const char *team_name;
    int x;
    int y;
    int parent_player_id;
} egg_creation_params_t;

int remove_egg_bis(game_state_t *state, int egg_id);

/**
 * @brief Calculate time difference in seconds between two timevals
 *
 * @param start Start time
 * @param end End time
 * @return double Time difference in seconds
 */
double get_time_diff_seconds(struct timeval *start, struct timeval *end);

/**
 * @brief Create a new egg
 *
 * @param state Game state
 * @param params Egg creation parameters
 * @return egg_t* Pointer to the new egg, NULL on failure
 */
egg_t *create_egg(game_state_t *state, const egg_creation_params_t *params);

/**
 * @brief Remove an egg from the game
 *
 * @param state Game state
 * @param egg_id ID of the egg to remove
 * @return int 0 on success, -1 on failure
 */
int remove_egg(game_state_t *state, int egg_id);

/**
 * @brief Find an egg by its ID
 *
 * @param state Game state
 * @param egg_id ID of the egg to find
 * @return egg_t* Pointer to the egg, NULL if not found
 */
egg_t *find_egg_by_id(game_state_t *state, int egg_id);

/**
 * @brief Check if an egg should hatch and create a new player slot
 *
 * @param state Game state
 * @param egg Egg to check
 * @return bool true if the egg hatched, false otherwise
 */
bool check_egg_hatching(game_state_t *state, egg_t *egg);

/**
 * @brief Process all eggs for hatching
 *
 * @param state Game state
 */
void process_eggs_hatching(game_state_t *state);

/**
 * @brief Add an egg to the game state
 *
 * @param state Game state
 * @param egg Egg to add
 * @return int 0 on success, -1 on failure
 */
int add_egg_to_state(game_state_t *state, egg_t *egg);

/**
 * @brief Get the count of eggs for a specific team
 *
 * @param state Game state
 * @param team_name Team name
 * @return int Number of eggs for the team
 */
int get_team_egg_count(game_state_t *state, const char *team_name);

/**
 * @brief Find and hatch a ready egg for a team
 *
 * @param state Game state
 * @param team_name Team name
 * @return egg_t* Pointer to the hatched egg, NULL if none available
 */
egg_t *hatch_ready_egg_for_team(game_state_t *state, const char *team_name);

#endif /* !EGGS_H_ */
