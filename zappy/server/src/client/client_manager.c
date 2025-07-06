/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Client management utilities for game state
*/

#include "game_state.h"
#include "player_manager.h"
#include "gui_notifications.h"

static void cleanup_client(client_t *client)
{
    for (int i = 0; i < client->buffer_size; i++)
        free(client->command_buffer[i].command);
    free(client->team_name);
    free(client);
}

static void handle_player_disconnection(game_state_t *state, client_t *client)
{
    player_t *player;

    if (client->type != CLIENT_TYPE_AI || client->player_id == -1)
        return;
    player = find_player_by_client_id(state, client->fd);
    if (player && player->is_alive) {
        printf("🔌 Player #%d from team '%s' disconnected - killing player\n",
            player->id, client->team_name ? client->team_name : "Unknown");
        drop_player_inventory(state, player);
        player->is_alive = false;
        if (player->is_incanting)
            player->is_incanting = false;
        notify_player_death(state, player->id);
    }
}

client_t *add_client(game_state_t *state, int fd)
{
    client_t *client = malloc(sizeof(client_t));

    if (!client)
        return NULL;
    memset(client, 0, sizeof(client_t));
    client->fd = fd;
    client->type = CLIENT_TYPE_UNKNOWN;
    client->player_id = -1;
    client->authenticated = false;
    client->next = state->clients;
    state->clients = client;
    return client;
}

void remove_client(game_state_t *state, int fd)
{
    client_t **current = &state->clients;
    client_t *to_remove;

    while (*current) {
        if ((*current)->fd == fd) {
            to_remove = *current;
            *current = (*current)->next;
            handle_player_disconnection(state, to_remove);
            cleanup_client(to_remove);
            return;
        }
        current = &(*current)->next;
    }
}

client_t *find_client(game_state_t *state, int fd)
{
    client_t *current = state->clients;

    while (current) {
        if (current->fd == fd)
            return current;
        current = current->next;
    }
    return NULL;
}
