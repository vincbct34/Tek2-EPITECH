/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** player_utils_bis
*/

#include "game_state.h"
#include "resource_utils.h"
#include "gui_notifications.h"

void drop_player_inventory(game_state_t *state, player_t *player)
{
    tile_t *tile;

    if (!state || !player)
        return;
    tile = get_tile_at(state, player->x, player->y);
    if (!tile)
        return;
    for (int i = 0; i < MAX_RESOURCES; i++) {
        if (player->resources[i] > 0) {
            tile->resources[i] += player->resources[i];
            printf("📦 Player #%d dropped %d %s at (%d, %d)\n",
                player->id, player->resources[i],
                get_resource_name(i), player->x, player->y);
            notify_all_drops(state, player, i);
            player->resources[i] = 0;
        }
    }
}

void count_players_at_tile(game_state_t *state, int x, int y,
    int *player_count)
{
    if (!state || !player_count) {
        if (player_count)
            *player_count = 0;
        return;
    }
    *player_count = 0;
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].is_alive &&
            state->players[i].x == x &&
            state->players[i].y == y) {
            (*player_count)++;
        }
    }
}

void notify_all_drops(game_state_t *state, player_t *player, int i)
{
    for (int j = 0; j < player->resources[i]; j++)
        notify_resource_drop(state, player->id, i);
}
