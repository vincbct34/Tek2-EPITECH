/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** gui_notifs_qua
*/

#include "gui_notifications.h"
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

void notify_player_expulsion(game_state_t *state, int player_id)
{
    char message[128];

    snprintf(message, sizeof(message), "pex #%d\n", player_id);
    broadcast_to_gui_clients(state, message);
}

void notify_broadcast(game_state_t *state,
    int player_id, const char *message)
{
    char notification[512];

    snprintf(notification, sizeof(notification), "pbc #%d %s\n",
        player_id, message);
    broadcast_to_gui_clients(state, notification);
}

void notify_game_end(game_state_t *state, const char *winning_team)
{
    char message[128];

    snprintf(message, sizeof(message), "seg %s\n", winning_team);
    broadcast_to_gui_clients(state, message);
}

void notify_egg_hatching(game_state_t *state, int egg_id)
{
    char message[128];

    snprintf(message, sizeof(message), "eht #%d\n", egg_id);
    broadcast_to_gui_clients(state, message);
}

void notify_server_message(game_state_t *state, const char *message)
{
    char notification[512];

    snprintf(notification, sizeof(notification), "smg %s\n", message);
    broadcast_to_gui_clients(state, notification);
}
