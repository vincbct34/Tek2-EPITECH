/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** AI incantation command
*/

#include "command_manager.h"
#include "gui_notifications.h"
#include "incantation_utils.h"
#include <sys/socket.h>
#include <stdio.h>

static void mark_player_as_incanting(game_state_t *state, int player_id)
{
    for (int j = 0; j < state->player_count; j++) {
        if (state->players[j].id == player_id) {
            state->players[j].is_incanting = true;
            break;
        }
    }
}

static void mark_players_as_incanting(game_state_t *state,
    incantation_ritual_t *ritual)
{
    for (int i = 0; i < ritual->player_count; i++)
        mark_player_as_incanting(state, ritual->player_ids[i]);
}

static void notify_participants(game_state_t *state,
    incantation_ritual_t *ritual)
{
    client_t *participant_client;

    for (int i = 0; i < ritual->player_count; i++) {
        participant_client = find_client_by_player_id(state,
            ritual->player_ids[i]);
        if (participant_client)
            send(participant_client->fd, "Elevation underway\n", 19, 0);
    }
}

static void setup_incantation_info(incantation_info_t *info, player_t *player,
    incantation_ritual_t *ritual)
{
    info->x = player->x;
    info->y = player->y;
    info->level = ritual->target_level;
    info->players = ritual->player_ids;
    info->player_count = ritual->player_count;
}

static void execute_incantation_ritual(incantation_params_t *params,
    client_t *client)
{
    incantation_info_t info;

    setup_ritual(params->ritual, params->player);
    add_players_to_ritual(params->state, params->player, params->ritual,
        params->req);
    mark_players_as_incanting(params->state, params->ritual);
    printf("✨ Player #%d from team '%s' starts incantation (%d->%d) with %d\n",
        params->player->id, client->team_name ? client->team_name : "Unknown",
        params->player->level, params->ritual->target_level,
        params->ritual->player_count);
    setup_incantation_info(&info, params->player, params->ritual);
    notify_incantation_start(params->state, &info);
    notify_participants(params->state, params->ritual);
}

int ai_incantation(game_state_t *state, client_t *client, const char *args)
{
    player_t *player = find_player_by_client_id(state, client->fd);
    const incantation_requirements_t *req;
    incantation_params_t params;
    int ritual_slot;

    (void)args;
    if (validate_and_get_requirements(player, client, &req) == -1)
        return -1;
    if (check_ritual_requirements(state, player, req, client) == -1)
        return -1;
    ritual_slot = allocate_ritual_slot(state, client);
    if (ritual_slot == -1)
        return -1;
    params = (incantation_params_t){state, player,
        &state->active_rituals[ritual_slot], req};
    execute_incantation_ritual(&params, client);
    return 0;
}
