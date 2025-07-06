/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** eggs_ter
*/

#include "eggs.h"
#include "gui_notifications.h"
#include "player_manager.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>

int remove_egg(game_state_t *state, int egg_id)
{
    int egg_index = -1;

    if (!state)
        return -1;
    for (int i = 0; i < state->egg_count; i++) {
        if (state->eggs[i].id == egg_id) {
            egg_index = i;
            break;
        }
    }
    if (egg_index == -1) {
        printf("❌ remove_egg: Egg #%d not found\n", egg_id);
        return -1;
    }
    free(state->eggs[egg_index].team_name);
    for (int i = egg_index; i < state->egg_count - 1; i++)
        state->eggs[i] = state->eggs[i + 1];
    return remove_egg_bis(state, egg_id);
}

double get_time_diff_seconds(struct timeval *start, struct timeval *end)
{
    return (end->tv_sec - start->tv_sec) +
        (end->tv_usec - start->tv_usec) / 1000000.0;
}

static egg_t *get_temp_egg(egg_t *egg, game_state_t *state)
{
    static egg_t temp_egg;
    int egg_id;
    int egg_x;
    int egg_y;

    egg_id = egg->id;
    egg_x = egg->x;
    egg_y = egg->y;
    notify_egg_connect(state, egg_id);
    printf("🐣 Egg #%d hatches as player connects!\n", egg_id);
    remove_egg(state, egg_id);
    printf("✅ Egg #%d successfully hatched into a player!\n", egg_id);
    temp_egg.x = egg_x;
    temp_egg.y = egg_y;
    temp_egg.id = egg_id;
    return &temp_egg;
}

egg_t *hatch_ready_egg_for_team(game_state_t *state,
    const char *team_name)
{
    egg_t *egg;

    if (!state || !team_name)
        return NULL;
    for (int i = 0; i < state->egg_count; i++) {
        egg = &state->eggs[i];
        if (egg->is_ready_to_hatch &&
            strcmp(egg->team_name, team_name) == 0) {
            return get_temp_egg(egg, state);
        }
    }
    return NULL;
}
