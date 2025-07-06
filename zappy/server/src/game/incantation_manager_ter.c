/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** incantation_manager_ter
*/

#include "game_state.h"
#include "time_manager.h"
#include "gui_notifications.h"
#include "incantation_manager.h"
#include <sys/time.h>
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>

void level_up_player(game_state_t *state, int player_id, int new_level)
{
    player_t *player = find_player_by_id(state, player_id);
    client_t *client;
    char response[64];

    if (!player)
        return;
    player->level = new_level;
    player->is_incanting = false;
    notify_player_level(state, player);
    client = find_client_by_player_id(state, player->id);
    if (client) {
        snprintf(response, sizeof(response), "Current level: %d\n",
            player->level);
        send(client->fd, response, strlen(response), 0);
    }
    printf("🎊 Player #%d reached level %d!\n", player->id, player->level);
}

void complete_ritual_success(game_state_t *state,
    incantation_ritual_t *ritual)
{
    const incantation_requirements_t *req;
    tile_t *tile;

    req = &incantation_levels[ritual->target_level - 2];
    tile = &state->map[ritual->y][ritual->x];
    consume_tile_resources(tile, req);
    for (int i = 0; i < ritual->player_count; i++)
        level_up_player(state, ritual->player_ids[i], ritual->target_level);
    notify_incantation_end(state, ritual->x, ritual->y, true);
    printf("✨ Incantation completed! %d players reached level %d\n",
        ritual->player_count, ritual->target_level);
}

static void reset_player_incanting_status(game_state_t *state, int player_id)
{
    player_t *player = find_player_by_id(state, player_id);
    client_t *client;

    if (!player)
        return;
    player->is_incanting = false;
    client = find_client_by_player_id(state, player->id);
    if (client)
        send(client->fd, "ko\n", 3, 0);
}

void complete_ritual_failure(game_state_t *state,
    incantation_ritual_t *ritual)
{
    for (int i = 0; i < ritual->player_count; i++)
        reset_player_incanting_status(state, ritual->player_ids[i]);
    notify_incantation_end(state, ritual->x, ritual->y, false);
    printf("💥 Incantation failed! Ritual interrupted\n");
}
