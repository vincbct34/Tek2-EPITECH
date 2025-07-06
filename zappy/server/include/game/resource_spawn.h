/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource spawning utilities
*/

#ifndef RESOURCE_SPAWN_H_
    #define RESOURCE_SPAWN_H_

    #include "game_types.h"

/**
 * @brief Spawn a single resource type on the map.
 *
 * @param state Game state
 * @param resource_type Type of resource to spawn
 * @param total_tiles Total number of tiles on the map
 * @return int Number of resources spawned
 */
int spawn_single_resource_type(game_state_t *state, int resource_type,
    int total_tiles);

/**
 * @brief Execute the resource respawn cycle.
 *
 * @param state Game state
 * @param respawn_cycle Current respawn cycle
 */
void execute_respawn_cycle(game_state_t *state, int respawn_cycle);

/**
 * @brief Calculate the maximum number of resources that can be
 * spawned per cycle.
 *
 * This function determines how many resources can be spawned in a single cycle
 * based on the maximum resources available and the total number of tiles.
 *
 * @param max_resources Maximum number of resources available for spawning
 * @return int Maximum number of resources that can be spawned per cycle
 */
int calculate_max_spawn_per_cycle(int max_resources);

#endif /* !RESOURCE_SPAWN_H_ */
