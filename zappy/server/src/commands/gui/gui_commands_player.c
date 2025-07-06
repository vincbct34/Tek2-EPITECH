/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** GUI commands for player and game state queries
*/

#include "command_manager.h"
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>

int gui_plv(game_state_t *state, client_t *client, const char *command)
{
    char response[128];
    int player_id;

    if (sscanf(command, "plv #%d", &player_id) != 1) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].id == player_id && state->players[i].is_alive) {
            snprintf(response, sizeof(response), "plv #%d %d\n",
                player_id, state->players[i].level);
            return send(client->fd, response, strlen(response), 0);
        }
    }
    send(client->fd, "sbp\n", 4, 0);
    return -1;
}

int gui_pin(game_state_t *state, client_t *client, const char *command)
{
    char response[256];
    int player_id;

    if (sscanf(command, "pin #%d", &player_id) != 1) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].id == player_id && state->players[i].is_alive) {
            snprintf(response, sizeof(response),
                "pin #%d %d %d %d %d %d %d %d %d %d\n",
                player_id, state->players[i].x, state->players[i].y,
                state->players[i].resources[0], state->players[i].resources[1],
                state->players[i].resources[2], state->players[i].resources[3],
                state->players[i].resources[4], state->players[i].resources[5],
                state->players[i].resources[6]);
            return send(client->fd, response, strlen(response), 0);
        }
    }
    return send(client->fd, "sbp\n", 4, 0);
}

int gui_sgt(game_state_t *state, client_t *client, const char *command)
{
    char response[64];

    (void)command;
    snprintf(response, sizeof(response), "sgt %d\n", state->config->freq);
    if (send(client->fd, response, strlen(response), 0) < 0)
        return -1;
    return 0;
}

int gui_sst(game_state_t *state, client_t *client, const char *command)
{
    int new_freq;
    char response[64];

    if (sscanf(command, "sst %d", &new_freq) != 1 || new_freq <= 0) {
        send(client->fd, "sbp\n", 4, 0);
        return -1;
    }
    state->config->freq = new_freq;
    snprintf(response, sizeof(response), "sst %d\n", new_freq);
    if (send(client->fd, response, strlen(response), 0) < 0)
        return -1;
    return 0;
}
