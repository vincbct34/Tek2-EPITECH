/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI commands for communication and player actions
*/

#include "command_manager.h"
#include "gui_notifications.h"
#include "eggs.h"

static void increment_team_slots(game_state_t *state, const char *team_name)
{
    for (int i = 0; i < state->config->team_count; i++) {
        if (strcmp(state->teams[i].name, team_name) == 0) {
            state->teams[i].slots_available++;
            printf("🔼 Incremented slots for team '%s' to %d\n",
                team_name, state->teams[i].slots_available);
            return;
        }
    }
}

static int find_team_slots(game_state_t *state, const char *team_name)
{
    for (int i = 0; i < state->config->team_count; i++) {
        if (strcmp(state->teams[i].name, team_name) == 0) {
            return state->teams[i].slots_available;
        }
    }
    return 0;
}

static int validate_fork_params(game_state_t *state, client_t *client)
{
    if (!state || !client)
        return -1;
    return 0;
}

static player_t *get_fork_player(game_state_t *state, client_t *client)
{
    player_t *player = find_player_by_client_id(state, client->fd);

    if (!player || !player->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return NULL;
    }
    return player;
}

static egg_t *create_player_egg(game_state_t *state, player_t *player)
{
    egg_creation_params_t params = {
        .team_name = player->team_name,
        .x = player->x,
        .y = player->y,
        .parent_player_id = player->id
    };

    return create_egg(state, &params);
}

int ai_connect_nbr(game_state_t *state, client_t *client, const char *args)
{
    char response[16];
    int slots;

    (void)args;
    if (!client->team_name) {
        send(client->fd, "0\n", 2, 0);
        return 0;
    }
    slots = find_team_slots(state, client->team_name);
    snprintf(response, sizeof(response), "%d\n", slots);
    send(client->fd, response, strlen(response), 0);
    return 0;
}

int ai_fork(game_state_t *state, client_t *client, const char *args)
{
    player_t *player;
    egg_t *new_egg;

    (void)args;
    if (validate_fork_params(state, client) == -1)
        return -1;
    player = get_fork_player(state, client);
    if (!player)
        return -1;
    new_egg = create_player_egg(state, player);
    if (!new_egg) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    increment_team_slots(state, player->team_name);
    notify_egg_laid(state, new_egg);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}
