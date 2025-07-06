/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Incantation ritual management
*/

#include "game_state.h"
#include "time_manager.h"
#include "gui_notifications.h"
#include "incantation_manager.h"
#include <sys/time.h>
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>

const incantation_requirements_t incantation_levels[8] = {
    {1, 1, 0, 0, 0, 0, 0},
    {2, 1, 1, 1, 0, 0, 0},
    {2, 2, 0, 1, 0, 2, 0},
    {4, 1, 1, 2, 0, 1, 0},
    {4, 1, 2, 1, 3, 0, 0},
    {6, 1, 2, 3, 0, 1, 0},
    {6, 2, 2, 2, 2, 2, 1}
};

bool check_tile_resources(tile_t *tile,
    const incantation_requirements_t *req)
{
    return (tile->resources[RESOURCE_LINEMATE] >= req->linemate &&
            tile->resources[RESOURCE_DERAUMERE] >= req->deraumere &&
            tile->resources[RESOURCE_SIBUR] >= req->sibur &&
            tile->resources[RESOURCE_MENDIANE] >= req->mendiane &&
            tile->resources[RESOURCE_PHIRAS] >= req->phiras &&
            tile->resources[RESOURCE_THYSTAME] >= req->thystame);
}

void consume_tile_resources(tile_t *tile,
    const incantation_requirements_t *req)
{
    tile->resources[RESOURCE_LINEMATE] -= req->linemate;
    tile->resources[RESOURCE_DERAUMERE] -= req->deraumere;
    tile->resources[RESOURCE_SIBUR] -= req->sibur;
    tile->resources[RESOURCE_MENDIANE] -= req->mendiane;
    tile->resources[RESOURCE_PHIRAS] -= req->phiras;
    tile->resources[RESOURCE_THYSTAME] -= req->thystame;
}

player_t *find_player_by_id(game_state_t *state, int player_id)
{
    for (int j = 0; j < state->player_count; j++) {
        if (state->players[j].id == player_id)
            return &state->players[j];
    }
    return NULL;
}

bool is_player_valid_for_ritual(player_t *player,
    incantation_ritual_t *ritual)
{
    return (player && player->is_alive &&
            player->x == ritual->x && player->y == ritual->y &&
            player->level == ritual->target_level - 1);
}

bool verify_ritual_still_valid(game_state_t *state,
    incantation_ritual_t *ritual)
{
    const incantation_requirements_t *req;
    tile_t *tile;
    player_t *player;

    req = &incantation_levels[ritual->target_level - 2];
    tile = &state->map[ritual->y][ritual->x];
    for (int i = 0; i < ritual->player_count; i++) {
        player = find_player_by_id(state, ritual->player_ids[i]);
        if (!is_player_valid_for_ritual(player, ritual))
            return false;
    }
    if (!check_tile_resources(tile, req))
        return false;
    return true;
}
