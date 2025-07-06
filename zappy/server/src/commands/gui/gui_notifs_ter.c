/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** gui_notifs_ter
*/

#include "gui_notifications.h"
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

void notify_player_inventory(game_state_t *state, player_t *player)
{
    char message[256];

    snprintf(message, sizeof(message), "pin #%d %d %d %d %d %d %d %d %d %d\n",
        player->id, player->x, player->y,
        player->resources[0], player->resources[1], player->resources[2],
        player->resources[3], player->resources[4], player->resources[5],
        player->resources[6]);
    broadcast_to_gui_clients(state, message);
}

void notify_player_death(game_state_t *state, int player_id)
{
    char message[128];

    snprintf(message, sizeof(message), "pdi #%d\n", player_id);
    broadcast_to_gui_clients(state, message);
}

void notify_resource_drop(game_state_t *state, int player_id,
    resource_type_t resource_type)
{
    char message[128];

    snprintf(message, sizeof(message), "pdr #%d %d\n",
        player_id, resource_type);
    broadcast_to_gui_clients(state, message);
}

void notify_resource_pickup(game_state_t *state, int player_id,
    resource_type_t resource_type)
{
    char message[128];

    snprintf(message, sizeof(message), "pgt #%d %d\n",
        player_id, resource_type);
    broadcast_to_gui_clients(state, message);
}

void notify_incantation_start(game_state_t *state,
    const incantation_info_t *info)
{
    char message[512];
    char player_list[256] = "";
    char player_id[16];

    for (int i = 0; i < info->player_count; i++) {
        snprintf(player_id, sizeof(player_id), " #%d", info->players[i]);
        strcat(player_list, player_id);
    }
    snprintf(message, sizeof(message), "pic %d %d %d%s\n",
        info->x, info->y, info->level, player_list);
    broadcast_to_gui_clients(state, message);
}
