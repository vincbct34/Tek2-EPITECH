/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player utilities and management
*/

#include "game_state.h"
#include "resource_utils.h"
#include "gui_notifications.h"

static int find_player_index(game_state_t *state, int player_id)
{
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].id == player_id) {
            return i;
        }
    }
    return -1;
}

static void shift_players_left(game_state_t *state, int player_index)
{
    for (int i = player_index; i < state->player_count - 1; i++)
        state->players[i] = state->players[i + 1];
}

player_t *find_player_by_client_id(game_state_t *state, int client_fd)
{
    client_t *client;

    if (!state)
        return NULL;
    client = find_client(state, client_fd);
    if (!client || client->player_id == 0)
        return NULL;
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].id == client->player_id) {
            return &state->players[i];
        }
    }
    return NULL;
}

tile_t *get_tile_at(game_state_t *state, int x, int y)
{
    if (!state || !state->map)
        return NULL;
    if (x < 0 || x >= state->config->width ||
        y < 0 || y >= state->config->height)
        return NULL;
    return &state->map[y][x];
}

void remove_player_from_game(game_state_t *state, int player_id)
{
    int player_index;

    if (!state || !state->players)
        return;
    player_index = find_player_index(state, player_id);
    if (player_index == -1)
        return;
    free(state->players[player_index].team_name);
    shift_players_left(state, player_index);
    state->player_count--;
    printf("🪦 Player #%d removed from game\n", player_id);
}

void transform_relative_coordinates(const coord_transform_params_t *params,
    coord_transform_result_t *result)
{
    result->abs_x = params->base_x;
    result->abs_y = params->base_y;
    switch (params->orientation) {
        case ORIENTATION_NORTH:
            result->abs_x += params->rel_x;
            result->abs_y -= params->rel_y;
            break;
        case ORIENTATION_EAST:
            result->abs_x += params->rel_y;
            result->abs_y += params->rel_x;
            break;
        case ORIENTATION_SOUTH:
            result->abs_x -= params->rel_x;
            result->abs_y += params->rel_y;
            break;
        case ORIENTATION_WEST:
            result->abs_x -= params->rel_y;
            result->abs_y -= params->rel_x;
            break;
    }
}

client_t *find_client_by_player_id(game_state_t *state, int player_id)
{
    client_t *client = state->clients;

    while (client) {
        if (client->player_id == player_id) {
            return client;
        }
        client = client->next;
    }
    return NULL;
}
