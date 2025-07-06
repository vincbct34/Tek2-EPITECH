/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI eject command implementation
*/

#include "command_manager.h"
#include "gui_notifications.h"
#include "ai_direction_utils.h"

static void notify_ejected_player(game_state_t *state,
    player_t *ejected_player, int ejector_orientation)
{
    client_t *ejected_client;
    char response[64];

    ejected_client = find_client_by_player_id(state, ejected_player->id);
    if (ejected_client && ejected_client->type == CLIENT_TYPE_AI) {
        snprintf(response, sizeof(response), "eject: %d\n",
            ejector_orientation);
        send(ejected_client->fd, response, strlen(response), 0);
    }
}

static void eject_player(game_state_t *state, player_t *ejected_player,
    int ejector_orientation)
{
    position_info_t pos = {
        .x = ejected_player->x, .y = ejected_player->y,
        .orientation = ejector_orientation
    };
    map_dimensions_t map = {
        .width = state->config->width, .height = state->config->height
    };

    calculate_new_position(&pos, &map);
    ejected_player->x = pos.x;
    ejected_player->y = pos.y;
    notify_ejected_player(state, ejected_player, ejector_orientation);
    notify_player_position(state, ejected_player);
    printf("💨 Player #%d ejected to position (%d, %d)\n",
        ejected_player->id, pos.x, pos.y);
}

static int eject_players_at_position(game_state_t *state, player_t *ejector)
{
    player_t *potential_victim;
    int ejected_count = 0;

    for (int i = 0; i < state->next_player_id; i++) {
        potential_victim = &state->players[i];
        if (!potential_victim->is_alive || potential_victim->id == ejector->id)
            continue;
        if (potential_victim->x == ejector->x &&
            potential_victim->y == ejector->y) {
            eject_player(state, potential_victim, ejector->orientation);
            ejected_count++;
        }
    }
    return ejected_count;
}

static void log_eject_result(player_t *ejector, int ejected_count,
    const char *team_name)
{
    printf("💨 Player #%d from team '%s' "
        "attempts to eject players at (%d, %d)\n",
        ejector->id, team_name ? team_name : "Unknown",
        ejector->x, ejector->y);
    if (ejected_count > 0) {
        printf("💨 Player #%d successfully ejected %d players\n",
            ejector->id, ejected_count);
    } else {
        printf("💨 Player #%d found no other players to eject\n", ejector->id);
    }
}

int ai_eject(game_state_t *state, client_t *client, const char *args)
{
    player_t *ejector;
    int ejected_count;

    (void)args;
    if (!state || !client)
        return -1;
    ejector = find_player_by_client_id(state, client->fd);
    if (!ejector || !ejector->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    ejected_count = eject_players_at_position(state, ejector);
    log_eject_result(ejector, ejected_count, client->team_name);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}
