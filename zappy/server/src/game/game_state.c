/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_state implementation
*/

#include "game_state.h"
#include "incantation_manager.h"

static void cleanup_partial_map(tile_t **map, int allocated_rows)
{
    for (int i = 0; i < allocated_rows; i++)
        free(map[i]);
    free(map);
}

static int allocate_map_row(game_state_t *state, int y)
{
    int width = state->config->width;

    state->map[y] = malloc(sizeof(tile_t) * width);
    if (!state->map[y]) {
        cleanup_partial_map(state->map, y);
        return -1;
    }
    for (int x = 0; x < width; x++)
        memset(&state->map[y][x], 0, sizeof(tile_t));
    return 0;
}

static int init_map(game_state_t *state)
{
    int height = state->config->height;

    state->map = malloc(sizeof(tile_t *) * height);
    if (!state->map)
        return -1;
    for (int y = 0; y < height; y++) {
        if (allocate_map_row(state, y) != 0)
            return -1;
    }
    return 0;
}

static int init_teams(game_state_t *state)
{
    state->teams = malloc(sizeof(team_t) * state->config->team_count);
    if (!state->teams)
        return -1;
    for (int i = 0; i < state->config->team_count; i++) {
        state->teams[i].name = strdup(state->config->team_names[i]);
        if (!state->teams[i].name)
            return -1;
        state->teams[i].slots_available = state->config->clients_per_team;
        state->teams[i].max_slots = state->config->clients_per_team;
    }
    return 0;
}

int init_game_state(game_state_t *state, server_config_t *config)
{
    memset(state, 0, sizeof(game_state_t));
    state->config = config;
    state->next_player_id = 1;
    state->next_egg_id = 1;
    state->game_running = true;
    gettimeofday(&state->start_time, NULL);
    state->last_resource_spawn = state->start_time;
    if (init_map(state) != 0)
        return -1;
    if (init_teams(state) != 0) {
        free_game_state(state);
        return -1;
    }
    if (initialize_map_resources(state) != 0) {
        printf("❌ Failed to initialize map resources\n");
        free_game_state(state);
        return -1;
    }
    initialize_incantation_system(state);
    return 0;
}

static void free_map(game_state_t *state)
{
    if (state->map) {
        for (int y = 0; y < state->config->height; y++)
            free(state->map[y]);
        free(state->map);
    }
}

static void free_teams(game_state_t *state)
{
    if (state->teams) {
        for (int i = 0; i < state->config->team_count; i++)
            free(state->teams[i].name);
        free(state->teams);
    }
}

static void free_clients(game_state_t *state)
{
    client_t *next;

    while (state->clients) {
        next = state->clients->next;
        for (int i = 0; i < state->clients->buffer_size; i++)
            free(state->clients->command_buffer[i].command);
        free(state->clients->team_name);
        free(state->clients);
        state->clients = next;
    }
}

void free_game_state(game_state_t *state)
{
    free_map(state);
    free_teams(state);
    free(state->players);
    free(state->eggs);
    free_clients(state);
}
