/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** player_creation_bis
*/

#include "game_state.h"
#include "eggs.h"

void place_player_random_position(player_t *player, game_state_t *state)
{
    if (!player || !state)
        return;
    player->x = rand() % state->config->width;
    player->y = rand() % state->config->height;
}
