/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** ai_commands
*/

#include "command_manager.h"
#include "gui_notifications.h"
#include "ai_vision_utils.h"
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int ai_forward(game_state_t *state, client_t *client, const char *args)
{
    player_t *player;

    (void)args;
    if (!state || !client)
        return -1;
    player = find_player_by_client_id(state, client->fd);
    if (!player || !player->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    if (move_player_forward(player, state) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    printf("🚶 Player #%d from team '%s' moves forward to (%d, %d)\n",
        player->id, client->team_name ? client->team_name : "Unknown",
        player->x, player->y);
    notify_player_position(state, player);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

int ai_right(game_state_t *state, client_t *client, const char *args)
{
    player_t *player;

    (void)args;
    if (!state || !client)
        return -1;
    player = find_player_by_client_id(state, client->fd);
    if (!player || !player->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    turn_player_right(player);
    printf("↩️  Player #%d from team '%s' turns right (now facing %d)\n",
        player->id, client->team_name ? client->team_name : "Unknown",
        player->orientation);
    notify_player_position(state, player);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

int ai_left(game_state_t *state, client_t *client, const char *args)
{
    player_t *player;

    (void)args;
    if (!state || !client)
        return -1;
    player = find_player_by_client_id(state, client->fd);
    if (!player || !player->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    turn_player_left(player);
    printf("↪️  Player #%d from team '%s' turns left (now facing %d)\n",
        player->id, client->team_name ? client->team_name : "Unknown",
        player->orientation);
    notify_player_position(state, player);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

static int validate_look_request(game_state_t *state, client_t *client,
    player_t **player)
{
    if (!state || !client)
        return -1;
    *player = find_player_by_client_id(state, client->fd);
    if (!*player || !(*player)->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    return 0;
}

static int process_look_vision(player_t *player, game_state_t *state,
    char *response, client_t *client)
{
    int *coords = NULL;
    int tile_count = 0;

    calculate_vision_coordinates(player, player->level, &coords, &tile_count);
    if (!coords) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    build_look_response(coords, tile_count, state, response);
    strncat(response, "]\n", 4095 - strlen(response));
    free(coords);
    return 0;
}

int ai_look(game_state_t *state, client_t *client, const char *args)
{
    char response[4096] = "[";
    player_t *player;

    (void)args;
    if (validate_look_request(state, client, &player) == -1)
        return -1;
    printf("\xF0\x9F\x91\x80Player #%d from team '%s' looks around "
        "(level %d)\n", player->id,
        client->team_name ? client->team_name : "Unknown", player->level);
    if (process_look_vision(player, state, response, client) == -1)
        return -1;
    send(client->fd, response, strlen(response), 0);
    return 0;
}

static void format_inventory_response(player_t *player,
    char *response, size_t size)
{
    snprintf(response, size,
        "[food %d, linemate %d, deraumere %d, sibur %d, mendiane %d, "
        "phiras %d, thystame %d]\n",
        player->resources[RESOURCE_FOOD],
        player->resources[RESOURCE_LINEMATE],
        player->resources[RESOURCE_DERAUMERE],
        player->resources[RESOURCE_SIBUR],
        player->resources[RESOURCE_MENDIANE],
        player->resources[RESOURCE_PHIRAS],
        player->resources[RESOURCE_THYSTAME]);
}

int ai_inventory(game_state_t *state, client_t *client, const char *args)
{
    player_t *player;
    char response[256];
    const char *team_name;

    (void)args;
    player = find_player_by_client_id(state, client->fd);
    if (!player || !player->is_alive) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    team_name = player->team_name ? player->team_name : "Unknown";
    printf("\xF0\x9F\x92\x92 Player #%d from team '%s' checks inventory\n",
        player->id, team_name);
    format_inventory_response(player, response, sizeof(response));
    send(client->fd, response, strlen(response), 0);
    return 0;
}
