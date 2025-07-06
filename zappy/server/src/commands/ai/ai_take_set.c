/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI commands for taking and setting objects
*/

#include "command_manager.h"
#include "gui_notifications.h"
#include "resource_utils.h"
#include <sys/socket.h>
#include <stdio.h>

static int validate_take_request(player_t *player, const char *args)
{
    if (!player || !player->is_alive || !args)
        return -1;
    return 0;
}

static int perform_take_action(game_state_t *state, player_t *player,
    int resource_type, const char *args)
{
    tile_t *tile = &state->map[player->y][player->x];

    if (tile->resources[resource_type] <= 0)
        return -1;
    tile->resources[resource_type]--;
    player->resources[resource_type]++;
    printf("📦 Player #%d from team '%s' takes %s at (%d,%d)\n",
        player->id, player->team_name ? player->team_name : "Unknown",
        args, player->x, player->y);
    notify_resource_pickup(state, player->id, (resource_type_t)resource_type);
    notify_player_inventory(state, player);
    return 0;
}

int ai_take(game_state_t *state, client_t *client, const char *args)
{
    player_t *player = find_player_by_client_id(state, client->fd);
    int resource_type;

    if (validate_take_request(player, args) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    resource_type = get_resource_type_from_string(args);
    if (resource_type == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    if (perform_take_action(state, player, resource_type, args) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

static int validate_set_request(player_t *player, const char *args,
    int resource_type)
{
    if (!player || !player->is_alive || !args || resource_type == -1)
        return -1;
    if (player->resources[resource_type] <= 0)
        return -1;
    return 0;
}

static void perform_set_action(game_state_t *state, player_t *player,
    int resource_type, const char *args)
{
    tile_t *tile = &state->map[player->y][player->x];

    player->resources[resource_type]--;
    tile->resources[resource_type]++;
    printf("📍 Player #%d from team '%s' sets down %s at (%d,%d)\n",
        player->id, player->team_name ? player->team_name : "Unknown",
        args, player->x, player->y);
    notify_resource_drop(state, player->id, (resource_type_t)resource_type);
    notify_player_inventory(state, player);
}

int ai_set(game_state_t *state, client_t *client, const char *args)
{
    player_t *player = find_player_by_client_id(state, client->fd);
    int resource_type = get_resource_type_from_string(args);

    if (validate_set_request(player, args, resource_type) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    perform_set_action(state, player, resource_type, args);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}
