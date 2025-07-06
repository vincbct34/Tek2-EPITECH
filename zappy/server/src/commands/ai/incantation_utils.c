/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Incantation utilities for AI commands
*/

#include "incantation_utils.h"
#include <sys/time.h>

int count_eligible_players(game_state_t *state, player_t *player)
{
    int players_on_tile = 0;

    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].is_alive &&
            state->players[i].x == player->x &&
            state->players[i].y == player->y &&
            state->players[i].level == player->level &&
            !state->players[i].is_incanting) {
            players_on_tile++;
        }
    }
    return players_on_tile;
}

int find_available_ritual_slot(game_state_t *state)
{
    for (int i = 0; i < 32; i++) {
        if (!state->active_rituals[i].is_active)
            return i;
    }
    return -1;
}

void setup_ritual(incantation_ritual_t *ritual, player_t *player)
{
    ritual->x = player->x;
    ritual->y = player->y;
    ritual->target_level = player->level + 1;
    ritual->player_count = 0;
    ritual->is_active = true;
    gettimeofday(&ritual->start_time, NULL);
}

void add_players_to_ritual(game_state_t *state, player_t *player,
    incantation_ritual_t *ritual, const incantation_requirements_t *req)
{
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].is_alive &&
            state->players[i].x == player->x &&
            state->players[i].y == player->y &&
            state->players[i].level == player->level &&
            !state->players[i].is_incanting &&
            ritual->player_count < req->players_needed) {
            ritual->player_ids[ritual->player_count] = state->players[i].id;
            ritual->player_count++;
        }
    }
}
