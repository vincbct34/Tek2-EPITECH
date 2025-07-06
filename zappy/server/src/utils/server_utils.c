/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** server_utils
*/

#include "run_logic.h"
#include "command_buffer.h"
#include "gui_notifications.h"
#include "incantation_manager.h"
#include "player_manager.h"
#include <sys/socket.h>

static int count_max_level_players_for_team(game_state_t *state, int team_idx)
{
    int max_level_players = 0;

    for (int j = 0; j < state->player_count; j++) {
        if (!state->players[j].team_name)
            continue;
        if (strcmp(state->players[j].team_name,
            state->teams[team_idx].name) != 0)
            continue;
        if (state->players[j].level >= 8)
            max_level_players++;
    }
    return max_level_players;
}

int check_win_condition(game_state_t *state)
{
    int max_level_players;

    for (int i = 0; i < state->config->team_count; i++) {
        max_level_players =
            count_max_level_players_for_team(state, i);
        if (max_level_players >= 6) {
            notify_game_end(state, state->teams[i].name);
            return 1;
        }
    }
    return 0;
}

void cleanup_bis(game_state_t *state, int i)
{
    client_t *client;

    drop_player_inventory(state, &state->players[i]);
    state->players[i].is_alive = false;
    if (state->players[i].is_incanting)
        state->players[i].is_incanting = false;
    client = find_client_by_player_id(state, state->players[i].id);
    if (client) {
        send(client->fd, "dead\n", 5, 0);
    }
    printf("💀 Player #%d from team '%s' has died from starvation\n",
        state->players[i].id,
        state->players[i].team_name ? state->players[i].team_name : "Unknown");
    notify_player_death(state, state->players[i].id);
}

void cleanup_dead_players(game_state_t *state)
{
    long long current_time = get_current_time_ms();
    long long death_time;

    for (int i = 0; i < state->player_count; i++) {
        if (!state->players[i].is_alive)
            continue;
        death_time = state->players[i].death_time.tv_sec * 1000 +
            state->players[i].death_time.tv_usec / 1000;
        if (current_time >= death_time)
            cleanup_bis(state, i);
    }
}

void update_game_state(game_state_t *state)
{
    process_ready_commands(state);
    update_player_survival(state);
    spawn_resources(state);
    cleanup_dead_players(state);
    process_incantation_rituals(state);
    process_eggs_hatching(state);
}
