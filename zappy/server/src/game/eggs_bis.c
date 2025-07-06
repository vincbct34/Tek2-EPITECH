/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** eggs_bis
*/

#include "eggs.h"
#include "gui_notifications.h"
#include "player_manager.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>


bool check_egg_hatching(game_state_t *state, egg_t *egg)
{
    struct timeval current_time;
    double elapsed_time;
    double time_unit_duration;

    if (!state || !egg)
        return false;
    gettimeofday(&current_time, NULL);
    elapsed_time = get_time_diff_seconds(&egg->creation_time, &current_time);
    time_unit_duration = 1.0 / state->config->freq;
    if (elapsed_time >= (EGG_HATCH_TIME * time_unit_duration)) {
        printf("🐣 Egg #%d is ready to hatch! (%.2f seconds elapsed)\n",
            egg->id, elapsed_time);
        return true;
    }
    return false;
}

void process_eggs_hatching_bis(egg_t *egg, game_state_t *state)
{
    egg->is_ready_to_hatch = true;
    notify_egg_hatching(state, egg->id);
    printf("🐣 Egg #%d is now ready to hatch!\n", egg->id);
    for (int j = 0; j < state->config->team_count; j++) {
        if (strcmp(state->teams[j].name, egg->team_name) == 0) {
            state->teams[j].slots_available++;
            printf("🔼 Team '%s' gained a connection slot from ready egg "
                "(total: %d)\n", egg->team_name,
                state->teams[j].slots_available);
            break;
        }
    }
}

void process_eggs_hatching(game_state_t *state)
{
    egg_t *egg;

    if (!state)
        return;
    for (int i = 0; i < state->egg_count; i++) {
        egg = &state->eggs[i];
        if (!egg->is_ready_to_hatch && check_egg_hatching(state, egg))
            process_eggs_hatching_bis(egg, state);
    }
}

int get_team_egg_count(game_state_t *state, const char *team_name)
{
    int count = 0;

    if (!state || !team_name)
        return 0;
    for (int i = 0; i < state->egg_count; i++) {
        if (strcmp(state->eggs[i].team_name, team_name) == 0) {
            count++;
        }
    }
    return count;
}
