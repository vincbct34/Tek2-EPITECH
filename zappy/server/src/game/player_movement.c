/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player movement and rotation
*/

#include "game_state.h"

static void calculate_new_position(player_t *player, game_state_t *state,
    int *new_x, int *new_y)
{
    switch (player->orientation) {
        case ORIENTATION_NORTH:
            *new_y = (*new_y - 1 + state->config->height)
                % state->config->height;
            break;
        case ORIENTATION_EAST:
            *new_x = (*new_x + 1) % state->config->width;
            break;
        case ORIENTATION_SOUTH:
            *new_y = (*new_y + 1) % state->config->height;
            break;
        case ORIENTATION_WEST:
            *new_x = (*new_x - 1 + state->config->width)
                % state->config->width;
            break;
    }
}

int move_player_forward(player_t *player, game_state_t *state)
{
    int new_x = player->x;
    int new_y = player->y;

    if (!player || !state)
        return -1;
    calculate_new_position(player, state, &new_x, &new_y);
    player->x = new_x;
    player->y = new_y;
    return 0;
}

void turn_player_left(player_t *player)
{
    if (!player)
        return;
    switch (player->orientation) {
        case ORIENTATION_NORTH:
            player->orientation = ORIENTATION_WEST;
            break;
        case ORIENTATION_EAST:
            player->orientation = ORIENTATION_NORTH;
            break;
        case ORIENTATION_SOUTH:
            player->orientation = ORIENTATION_EAST;
            break;
        case ORIENTATION_WEST:
            player->orientation = ORIENTATION_SOUTH;
            break;
    }
}

void turn_player_right(player_t *player)
{
    if (!player)
        return;
    switch (player->orientation) {
        case ORIENTATION_NORTH:
            player->orientation = ORIENTATION_EAST;
            break;
        case ORIENTATION_EAST:
            player->orientation = ORIENTATION_SOUTH;
            break;
        case ORIENTATION_SOUTH:
            player->orientation = ORIENTATION_WEST;
            break;
        case ORIENTATION_WEST:
            player->orientation = ORIENTATION_NORTH;
            break;
    }
}
