/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource spawning utilities
*/

#include "map_display.h"
#include "resource_spawn.h"
#include "gui_notifications.h"

extern const double RESOURCE_DENSITIES[MAX_RESOURCES];

int calculate_max_spawn_per_cycle(int max_resources)
{
    int max_spawn_per_cycle = (int)(max_resources * 0.1);

    if (max_spawn_per_cycle < 1)
        max_spawn_per_cycle = 1;
    return max_spawn_per_cycle;
}

static void spawn_resources_at_position(game_state_t *state,
    int resource_type, int resources_to_spawn)
{
    position_t pos;

    for (int i = 0; i < resources_to_spawn; i++) {
        pos.x = rand() % state->config->width;
        pos.y = rand() % state->config->height;
        add_resource_to_tile(state, pos, resource_type, 1);
        notify_tile_content(state, pos.x, pos.y);
    }
}

static void print_spawn_info(int resource_type, int resources_to_spawn,
    int current_resources, int max_resources)
{
    if (resources_to_spawn > 0) {
        printf("   🌱 Spawning %d %s (current: %d/%d)\n",
            resources_to_spawn, get_resource_name(resource_type),
            current_resources, max_resources);
    } else {
        printf("   ✅ %s\tat capacity (%d/%d)\n",
            get_resource_name(resource_type),
            current_resources, max_resources);
    }
}

int spawn_single_resource_type(game_state_t *state, int resource_type,
    int total_tiles)
{
    int max_resources;
    int current_resources;
    int resources_to_spawn;
    int max_spawn_per_cycle;

    max_resources = (int)(total_tiles * RESOURCE_DENSITIES[resource_type]);
    current_resources = count_total_resources_on_map(state, resource_type);
    resources_to_spawn = max_resources - current_resources;
    max_spawn_per_cycle = calculate_max_spawn_per_cycle(max_resources);
    if (resources_to_spawn > max_spawn_per_cycle)
        resources_to_spawn = max_spawn_per_cycle;
    print_spawn_info(resource_type, resources_to_spawn, current_resources,
        max_resources);
    if (resources_to_spawn > 0)
        spawn_resources_at_position(state, resource_type, resources_to_spawn);
    return (resources_to_spawn > 0) ? resources_to_spawn : 0;
}

void execute_respawn_cycle(game_state_t *state, int respawn_cycle)
{
    int total_tiles;
    int total_spawned;

    total_tiles = state->config->width * state->config->height;
    total_spawned = 0;
    printf("\n🔄 === RESOURCE RESPAWN CYCLE #%d ===\n", respawn_cycle);
    for (int resource_type = 0; resource_type < MAX_RESOURCES;
        resource_type++) {
        total_spawned += spawn_single_resource_type(state, resource_type,
            total_tiles);
    }
    printf("✅ Respawn completed! %d total resources added\n", total_spawned);
    display_map_statistics(state);
    display_map(state);
    printf("🔄 === END RESPAWN CYCLE #%d ===\n\n", respawn_cycle);
}
