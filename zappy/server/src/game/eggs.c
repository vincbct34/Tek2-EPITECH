/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Egg management functions
*/

#include "eggs.h"
#include "gui_notifications.h"
#include "player_manager.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>

egg_t *create_egg_bis(egg_t *new_egg, game_state_t *state,
    const egg_creation_params_t *params)
{
    new_egg->team_name = strdup(params->team_name);
    if (!new_egg->team_name) {
        printf("❌ create_egg: Team name allocation failed\n");
        free(new_egg);
        return NULL;
    }
    gettimeofday(&new_egg->creation_time, NULL);
    new_egg->is_ready_to_hatch = false;
    if (add_egg_to_state(state, new_egg) != 0) {
        printf("❌ create_egg: Failed to add egg to state\n");
        free(new_egg->team_name);
        free(new_egg);
        return NULL;
    }
    printf("🥚 Egg #%d created for team '%s' at (%d, %d)\n",
        new_egg->id, params->team_name, params->x, params->y);
    return new_egg;
}

egg_t *create_egg(game_state_t *state, const egg_creation_params_t *params)
{
    egg_t *new_egg;

    if (!state || !params || !params->team_name ||
        params->x < 0 || params->x >= state->config->width ||
        params->y < 0 || params->y >= state->config->height) {
        printf("❌ create_egg: Invalid parameters\n");
        return NULL;
    }
    new_egg = malloc(sizeof(egg_t));
    if (!new_egg) {
        printf("❌ create_egg: Memory allocation failed\n");
        return NULL;
    }
    new_egg->id = state->next_egg_id;
    state->next_egg_id++;
    new_egg->x = params->x;
    new_egg->y = params->y;
    new_egg->player_id = params->parent_player_id;
    return create_egg_bis(new_egg, state, params);
}

int add_egg_to_state(game_state_t *state, egg_t *egg)
{
    egg_t *new_eggs;

    if (!state || !egg)
        return -1;
    new_eggs = realloc(state->eggs, sizeof(egg_t) *
        (state->egg_count + 1));
    if (!new_eggs) {
        printf("❌ add_egg_to_state: Memory reallocation failed\n");
        return -1;
    }
    state->eggs = new_eggs;
    state->eggs[state->egg_count] = *egg;
    state->egg_count++;
    return 0;
}

egg_t *find_egg_by_id(game_state_t *state, int egg_id)
{
    if (!state)
        return NULL;
    for (int i = 0; i < state->egg_count; i++) {
        if (state->eggs[i].id == egg_id) {
            return &state->eggs[i];
        }
    }
    return NULL;
}

int remove_egg_bis(game_state_t *state, int egg_id)
{
    egg_t *new_eggs;

    state->egg_count--;
    if (state->egg_count > 0) {
        new_eggs = realloc(state->eggs, sizeof(egg_t) * state->egg_count);
        if (new_eggs)
            state->eggs = new_eggs;
    } else {
        free(state->eggs);
        state->eggs = NULL;
    }
    printf("🗑️ Egg #%d removed from game\n", egg_id);
    return 0;
}
