/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource management functions
*/

#ifndef RESOURCE_MANAGER_H_
    #define RESOURCE_MANAGER_H_

    #include "game_types.h"

/**
 * @brief Initialize map with resources according to densities
 *
 * @param state Game state
 * @return 0 on success, -1 on error
 */
int initialize_map_resources(game_state_t *state);

/**
 * @brief Spawn resources on the map (respawn cycle)
 *
 * @param state Game state
 */
void spawn_resources_cycle(game_state_t *state);

/**
 * @brief Add resource to specific tile
 *
 * @param state Game state
 * @param pos Position coordinates
 * @param resource_type Type of resource
 * @param quantity Quantity to add
 * @return 0 on success, -1 on error
 */
int add_resource_to_tile(game_state_t *state, position_t pos,
    resource_type_t resource_type, int quantity);

/**
 * @brief Remove resource from tile
 *
 * @param state Game state
 * @param pos Position coordinates
 * @param resource_type Type of resource
 * @param quantity Quantity to remove
 * @return Actual quantity removed
 */
int remove_resource_from_tile(game_state_t *state, position_t pos,
    resource_type_t resource_type, int quantity);

/**
 * @brief Check if tile has specific resource
 *
 * @param state Game state
 * @param x X coordinate
 * @param y Y coordinate
 * @param resource_type Type of resource
 * @return Quantity available, 0 if none
 */
int get_resource_on_tile(game_state_t *state, int x, int y,
    resource_type_t resource_type);

/**
 * @brief Get resource name as string
 *
 * @param resource_type Type of resource
 * @return String name of the resource
 */
const char *get_resource_name(resource_type_t resource_type);

/**
 * @brief Count total resources of a specific type on the map
 *
 * @param state Game state
 * @param resource_type Type of resource to count
 * @return Total count of the resource on the map
 */
int count_total_resources_on_map(game_state_t *state,
    resource_type_t resource_type);

#endif /* !RESOURCE_MANAGER_H_ */
