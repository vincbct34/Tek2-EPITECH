/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource spawning and management utilities
*/

#include "map_display.h"
#include "resource_spawn.h"
#include "resource_utils.h"

const double RESOURCE_DENSITIES[MAX_RESOURCES] = {
    0.5,
    0.3,
    0.15,
    0.1,
    0.1,
    0.08,
    0.05
};

static void spawn_resource_batch(game_state_t *state, int resource_type,
    int resource_count)
{
    position_t pos;

    for (int i = 0; i < resource_count; i++) {
        pos.x = rand() % state->config->width;
        pos.y = rand() % state->config->height;
        add_resource_to_tile(state, pos, resource_type, 1);
    }
}

static void print_resource_initialization(int resource_type,
    int resource_count)
{
    printf("   📦 Spawning %d %s across the map\n",
        resource_count, get_resource_name(resource_type));
}

int initialize_map_resources(game_state_t *state)
{
    int total_tiles;
    int resource_count;

    if (!state || !state->map)
        return -1;
    total_tiles = state->config->width * state->config->height;
    printf("🌱 Initializing map resources...\n");
    for (int resource_type = 0; resource_type < MAX_RESOURCES;
        resource_type++) {
        resource_count = (int)(total_tiles *
            RESOURCE_DENSITIES[resource_type]);
        print_resource_initialization(resource_type, resource_count);
        spawn_resource_batch(state, resource_type, resource_count);
    }
    printf("✅ Map resources initialized successfully!\n");
    display_map_statistics(state);
    display_map(state);
    return 0;
}

static long long calculate_respawn_time(game_state_t *state)
{
    return (RESOURCE_RESPAWN_TIME * 1000) / state->config->freq;
}

static long long get_time_diff(struct timeval *times)
{
    long long time_diff;

    time_diff = (times[0].tv_sec - times[1].tv_sec) * 1000;
    time_diff += (times[0].tv_usec - times[1].tv_usec) / 1000;
    return time_diff;
}

void spawn_resources_cycle(game_state_t *state)
{
    struct timeval current_time;
    struct timeval times[2];
    long long time_diff;
    long long respawn_time;
    static int respawn_cycle = 0;

    if (!state)
        return;
    gettimeofday(&current_time, NULL);
    times[0] = current_time;
    times[1] = state->last_resource_spawn;
    time_diff = get_time_diff(times);
    respawn_time = calculate_respawn_time(state);
    if (time_diff >= respawn_time) {
        respawn_cycle++;
        execute_respawn_cycle(state, respawn_cycle);
        state->last_resource_spawn = current_time;
    }
}

int add_resource_to_tile(game_state_t *state, position_t pos,
    resource_type_t resource_type, int quantity)
{
    tile_t *tile;

    if (!state || resource_type < 0 || resource_type >= MAX_RESOURCES)
        return -1;
    tile = get_tile_at(state, pos.x, pos.y);
    if (!tile)
        return -1;
    tile->resources[resource_type] += quantity;
    return 0;
}

int remove_resource_from_tile(game_state_t *state, position_t pos,
    resource_type_t resource_type, int quantity)
{
    tile_t *tile;
    int available;

    if (!state || resource_type < 0 || resource_type >= MAX_RESOURCES)
        return 0;
    tile = get_tile_at(state, pos.x, pos.y);
    if (!tile)
        return 0;
    available = tile->resources[resource_type];
    if (available >= quantity) {
        tile->resources[resource_type] -= quantity;
        return quantity;
    } else {
        tile->resources[resource_type] = 0;
        return available;
    }
}

int get_resource_on_tile(game_state_t *state, int x, int y,
    resource_type_t resource_type)
{
    tile_t *tile;

    if (!state || resource_type < 0 || resource_type >= MAX_RESOURCES)
        return 0;
    tile = get_tile_at(state, x, y);
    if (!tile)
        return 0;
    return tile->resources[resource_type];
}
