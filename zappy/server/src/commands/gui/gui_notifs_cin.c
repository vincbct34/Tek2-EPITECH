/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** gui_notifs_cin
*/

#include "gui_notifications.h"
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

void notify_tile_content(game_state_t *state, int x, int y)
{
    tile_t *tile = get_tile_at(state, x, y);
    char message[256];

    if (!state || !state->config || !tile)
        return;
    snprintf(message, sizeof(message),
        "bct %d %d %d %d %d %d %d %d %d\n",
        x, y,
        tile->resources[0], tile->resources[1], tile->resources[2],
        tile->resources[3], tile->resources[4], tile->resources[5],
        tile->resources[6]);
    broadcast_to_gui_clients(state, message);
}
