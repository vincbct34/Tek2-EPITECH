/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource utility functions
*/

#include "resource_utils.h"
#include "game_types.h"

const char *get_resource_name(resource_type_t resource_type)
{
    static const char *names[] = {
        "food", "linemate", "deraumere", "sibur",
        "mendiane", "phiras", "thystame"
    };

    if (resource_type >= 0 && resource_type < MAX_RESOURCES)
        return names[resource_type];
    return "unknown";
}

int count_total_resources_on_map(game_state_t *state,
    resource_type_t resource_type)
{
    int total_count;

    total_count = 0;
    if (!state || !state->map || resource_type < 0 ||
        resource_type >= MAX_RESOURCES)
        return 0;
    for (int y = 0; y < state->config->height; y++) {
        for (int x = 0; x < state->config->width; x++) {
            total_count += state->map[y][x].resources[resource_type];
        }
    }
    return total_count;
}
