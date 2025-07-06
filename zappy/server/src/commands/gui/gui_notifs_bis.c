/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** gui_notifs_bis
*/

#include "gui_notifications.h"
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

void notify_incantation_end(game_state_t *state, int x, int y, bool success)
{
    char message[128];

    snprintf(message, sizeof(message),
        "pie %d %d %d\n", x, y, success ? 1 : 0);
    broadcast_to_gui_clients(state, message);
}

void notify_egg_laying(game_state_t *state, int player_id)
{
    char message[128];

    snprintf(message, sizeof(message), "pfk #%d\n", player_id);
    broadcast_to_gui_clients(state, message);
}

void notify_egg_laid(game_state_t *state, egg_t *egg)
{
    char message[128];

    snprintf(message, sizeof(message), "enw #%d #%d %d %d\n",
        egg->id, egg->player_id, egg->x, egg->y);
    broadcast_to_gui_clients(state, message);
}

void notify_egg_connect(game_state_t *state, int egg_id)
{
    char message[128];

    snprintf(message, sizeof(message), "ebo #%d\n", egg_id);
    broadcast_to_gui_clients(state, message);
}

void notify_egg_death(game_state_t *state, int egg_id)
{
    char message[128];

    snprintf(message, sizeof(message), "edi #%d\n", egg_id);
    broadcast_to_gui_clients(state, message);
}
