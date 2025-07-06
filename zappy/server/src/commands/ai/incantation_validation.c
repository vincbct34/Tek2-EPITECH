/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Incantation validation utilities
*/

#include "incantation_utils.h"
#include "game_types.h"
#include <sys/socket.h>

static int validate_incantation_request(player_t *player)
{
    if (!player || !player->is_alive || player->is_incanting ||
        player->level >= 8)
        return -1;
    return 0;
}

int check_player_count(game_state_t *state, player_t *player,
    const incantation_requirements_t *req)
{
    int players_on_tile = count_eligible_players(state, player);

    if (players_on_tile < req->players_needed)
        return -1;
    return 0;
}

int check_incantation_requirements(game_state_t *state,
    player_t *player, const incantation_requirements_t *req)
{
    tile_t *tile = &state->map[player->y][player->x];

    if (check_player_count(state, player, req) == -1)
        return -1;
    if (!check_tile_resources(tile, req))
        return -1;
    return 0;
}

int validate_and_get_requirements(player_t *player, client_t *client,
    const incantation_requirements_t **req)
{
    if (validate_incantation_request(player) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    *req = &incantation_levels[player->level - 1];
    return 0;
}

int check_ritual_requirements(game_state_t *state, player_t *player,
    const incantation_requirements_t *req, client_t *client)
{
    if (check_incantation_requirements(state, player, req) == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    return 0;
}

int allocate_ritual_slot(game_state_t *state, client_t *client)
{
    int ritual_slot = find_available_ritual_slot(state);

    if (ritual_slot == -1) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    return ritual_slot;
}
