/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** GUI commands for map and team information
*/

#include "command_manager.h"
#include "coords.h"
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>

int gui_msz(game_state_t *state, client_t *client, const char *command)
{
    char response[64];

    (void)command;
    if (!state || !client || !state->config)
        return -1;
    snprintf(response, sizeof(response), "msz %d %d\n",
        state->config->width, state->config->height);
    if (send(client->fd, response, strlen(response), 0) < 0)
        return -1;
    return 0;
}

int gui_bct(game_state_t *state, client_t *client, const char *command)
{
    int x;
    int y;

    if (sscanf(command, "bct %d %d", &x, &y) != 2) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    if (!is_valid_tile_coords(state, x, y)) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    send_tile_content(client, x, y, &state->map[y][x]);
    return 0;
}

int gui_mct(game_state_t *state, client_t *client, const char *command)
{
    int x;
    int y;

    (void)command;
    for (y = 0; y < state->config->height; y++) {
        for (x = 0; x < state->config->width; x++) {
            send_tile_content(client, x, y, &state->map[y][x]);
        }
    }
    return 0;
}

static int send_team_name(client_t *client, const char *name)
{
    char response[128];

    snprintf(response, sizeof(response), "tna %s\n", name);
    return send(client->fd, response, strlen(response), 0);
}

int gui_tna(game_state_t *state, client_t *client, const char *command)
{
    int i;

    (void)command;
    for (i = 0; i < state->config->team_count; i++) {
        if (send_team_name(client, state->teams[i].name) < 0)
            return -1;
    }
    return 0;
}

static int send_player_position(client_t *client, int id, coords_t coords,
    int orientation)
{
    char response[128];

    snprintf(response, sizeof(response), "ppo #%d %d %d %d\n",
        id, coords.x, coords.y, orientation);
    return send(client->fd, response, strlen(response), 0);
}

int gui_ppo(game_state_t *state, client_t *client, const char *command)
{
    int player_id;
    int i;
    coords_t coords = {0, 0};

    if (sscanf(command, "ppo #%d", &player_id) != 1) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    for (i = 0; i < state->player_count; i++) {
        if (state->players[i].id == player_id && state->players[i].is_alive) {
            coords.x = state->players[i].x;
            coords.y = state->players[i].y;
            return send_player_position(client, player_id, coords,
                state->players[i].orientation);
        }
    }
    send(client->fd, "sbp\n", 4, 0);
    return -1;
}
