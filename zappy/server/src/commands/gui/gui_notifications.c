/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** GUI notifications implementation
*/

#include "gui_notifications.h"
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

void broadcast_to_gui_clients(game_state_t *state, const char *message)
{
    client_t *client = state->clients;

    while (client) {
        if (client->type == CLIENT_TYPE_GUI && client->authenticated) {
            send(client->fd, message, strlen(message), 0);
        }
        client = client->next;
    }
}

void notify_gui_clients(game_state_t *state, const char *message)
{
    broadcast_to_gui_clients(state, message);
}

void notify_player_connect(game_state_t *state, player_t *player)
{
    char message[256];

    snprintf(message, sizeof(message), "pnw #%d %d %d %d %d %s\n",
        player->id, player->x, player->y, player->orientation,
        player->level, player->team_name);
    broadcast_to_gui_clients(state, message);
}

void notify_player_position(game_state_t *state, player_t *player)
{
    char message[128];

    snprintf(message, sizeof(message), "ppo #%d %d %d %d\n",
        player->id, player->x, player->y, player->orientation);
    broadcast_to_gui_clients(state, message);
}

void notify_player_level(game_state_t *state, player_t *player)
{
    char message[128];

    snprintf(message, sizeof(message), "plv #%d %d\n",
        player->id, player->level);
    broadcast_to_gui_clients(state, message);
}
