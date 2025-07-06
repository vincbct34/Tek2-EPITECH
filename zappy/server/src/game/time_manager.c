/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** time_manager implementation
*/

#include "resource_manager.h"

long long get_current_time_ms(void)
{
    struct timeval tv;

    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

static long long calculate_action_time(int action_time, int freq)
{
    return (action_time * 1000) / freq;
}

bool is_action_ready(const struct timeval *start_time, long long duration)
{
    long long start_ms = start_time->tv_sec * 1000 +
        start_time->tv_usec / 1000;
    long long current_ms = get_current_time_ms();

    return (current_ms - start_ms) >= duration;
}

static void update_single_player_survival(game_state_t *state, int i,
    long long current_time)
{
    long long food_time;
    long long death_time;

    food_time = calculate_action_time(FOOD_SURVIVAL_TIME,
        state->config->freq);
    death_time = state->players[i].death_time.tv_sec * 1000 +
        state->players[i].death_time.tv_usec / 1000;
    if (!state->players[i].is_alive || current_time < death_time)
        return;
    if (state->players[i].resources[RESOURCE_FOOD] > 0) {
        state->players[i].resources[RESOURCE_FOOD]--;
        state->players[i].death_time.tv_sec =
            (current_time + food_time) / 1000;
        state->players[i].death_time.tv_usec =
            ((current_time + food_time) % 1000) * 1000;
    } else {
        state->players[i].death_time.tv_sec = current_time / 1000;
        state->players[i].death_time.tv_usec = (current_time % 1000) * 1000;
    }
}

void update_player_survival(game_state_t *state)
{
    long long current_time = get_current_time_ms();

    for (int i = 0; i < state->player_count; i++)
        update_single_player_survival(state, i, current_time);
}

void spawn_resources(game_state_t *state)
{
    spawn_resources_cycle(state);
}
