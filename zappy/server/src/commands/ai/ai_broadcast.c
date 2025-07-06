/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI broadcast command implementation
*/

#include "command_manager.h"
#include "ai_direction_utils.h"

static int validate_broadcast_params(game_state_t *state, client_t *client,
    const char *args)
{
    if (!state || !client || !args) {
        if (client)
            send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    return 0;
}

static player_t *get_broadcaster(game_state_t *state, client_t *client)
{
    player_t *broadcaster = find_player_by_client_id(state, client->fd);

    if (!broadcaster || !broadcaster->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return NULL;
    }
    return broadcaster;
}

static void send_message_to_listener(game_state_t *state,
    player_t *broadcaster, player_t *listener, const char *message)
{
    client_t *listener_client;
    char response[512];
    int direction;
    position_pair_t positions = {
        .src_x = broadcaster->x, .src_y = broadcaster->y,
        .dst_x = listener->x, .dst_y = listener->y
    };
    map_dimensions_t map = {
        .width = state->config->width, .height = state->config->height
    };

    listener_client = find_client_by_player_id(state, listener->id);
    if (!listener_client || listener_client->type != CLIENT_TYPE_AI)
        return;
    direction = calculate_direction(&positions, &map, listener->orientation);
    snprintf(response, sizeof(response), "message %d, %s\n",
            direction, message);
    send(listener_client->fd, response, strlen(response), 0);
}

static void broadcast_to_all_players(game_state_t *state,
    player_t *broadcaster, const char *message)
{
    player_t *listener;

    for (int i = 0; i < state->next_player_id; i++) {
        listener = &state->players[i];
        if (!listener->is_alive || listener->id == broadcaster->id)
            continue;
        send_message_to_listener(state, broadcaster, listener, message);
    }
}

int ai_broadcast(game_state_t *state, client_t *client, const char *args)
{
    player_t *broadcaster;
    char message_buffer[256];

    if (validate_broadcast_params(state, client, args) == -1)
        return -1;
    broadcaster = get_broadcaster(state, client);
    if (!broadcaster)
        return -1;
    strncpy(message_buffer, args, sizeof(message_buffer) - 1);
    message_buffer[sizeof(message_buffer) - 1] = '\0';
    printf("📢 Player #%d from team '%s' broadcasts: '%s'\n",
        broadcaster->id, client->team_name ? client->team_name : "Unknown",
        message_buffer);
    broadcast_to_all_players(state, broadcaster, message_buffer);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}
