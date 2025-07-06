/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** incentation_manager_bis
*/

#include "game_state.h"
#include "time_manager.h"
#include "gui_notifications.h"
#include "incantation_manager.h"
#include <sys/time.h>
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>

int calculate_elapsed_ticks(struct timeval *start_time,
    struct timeval *current_time)
{
    long elapsed_us;

    elapsed_us = (current_time->tv_sec - start_time->tv_sec) * 1000000 +
        (current_time->tv_usec - start_time->tv_usec);
    return (elapsed_us * 126) / 1000000;
}

void process_single_ritual(game_state_t *state,
    incantation_ritual_t *ritual, struct timeval *current_time)
{
    int elapsed_ticks;

    if (!ritual->is_active)
        return;
    elapsed_ticks = calculate_elapsed_ticks(&ritual->start_time, current_time);
    if (elapsed_ticks >= INCANTATION_DURATION_TICKS) {
        if (verify_ritual_still_valid(state, ritual))
            complete_ritual_success(state, ritual);
        else
            complete_ritual_failure(state, ritual);
        ritual->is_active = false;
    } else {
        if (!verify_ritual_still_valid(state, ritual)) {
            complete_ritual_failure(state, ritual);
            ritual->is_active = false;
        }
    }
}

void process_incantation_rituals(game_state_t *state)
{
    struct timeval current_time;

    gettimeofday(&current_time, NULL);
    for (int i = 0; i < 32; i++)
        process_single_ritual(state, &state->active_rituals[i], &current_time);
}

void initialize_incantation_system(game_state_t *state)
{
    state->ritual_count = 0;
    for (int i = 0; i < 32; i++)
        state->active_rituals[i].is_active = false;
}
