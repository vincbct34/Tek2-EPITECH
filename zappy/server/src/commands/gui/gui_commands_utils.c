/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** gui_commands_utils
*/

#include "command_manager.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

bool is_valid_tile_coords(game_state_t *state, int x, int y)
{
    if (!state || !state->config)
        return false;
    return (x >= 0 && x < state->config->width && y >= 0 &&
        y < state->config->height);
}

void send_tile_content(client_t *client, int x, int y, tile_t *tile)
{
    char response[256];

    snprintf(response, sizeof(response),
        "bct %d %d %d %d %d %d %d %d %d\n", x, y,
        tile->resources[0], tile->resources[1], tile->resources[2],
        tile->resources[3], tile->resources[4], tile->resources[5],
        tile->resources[6]);
    send(client->fd, response, strlen(response), 0);
}
