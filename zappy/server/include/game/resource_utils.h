/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource utility functions
*/

#ifndef RESOURCE_UTILS_H_
    #define RESOURCE_UTILS_H_

    #include "game_types.h"

/**
 * @brief Get the name of a resource type.
 *
 * @param resource_type Type of resource
 * @return const char* Name of the resource
 */
const char *get_resource_name(resource_type_t resource_type);

/**
 * @brief Get the total amount of a specific resource type on the map.
 *
 * @param state Game state
 * @param resource_type Type of resource to count
 * @return int Total amount of the specified resource type on the map
 */
int count_total_resources_on_map(game_state_t *state,
    resource_type_t resource_type);

#endif /* !RESOURCE_UTILS_H_ */
