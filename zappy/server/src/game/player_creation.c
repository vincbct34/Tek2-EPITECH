/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player creation and initialization
*/

#include "game_state.h"
#include "eggs.h"

void setup_player_survival_time(player_t *player,
    game_state_t *state)
{
    long long food_time;

    gettimeofday(&player->last_action_time, NULL);
    gettimeofday(&player->death_time, NULL);
    food_time = (FOOD_SURVIVAL_TIME * 1000) / state->config->freq;
    player->death_time.tv_sec += food_time / 1000;
    player->death_time.tv_usec += (food_time % 1000) * 1000;
}

void check_hatched_egg(game_state_t *state, client_t *client,
    player_t *player)
{
    egg_t *hatched_egg = hatch_ready_egg_for_team(state, client->team_name);

    if (hatched_egg) {
        player->x = hatched_egg->x;
        player->y = hatched_egg->y;
        printf("🐣 Player #%d spawned from egg at (%d, %d)\n",
            player->id, player->x, player->y);
    } else
        place_player_random_position(player, state);
}

void initialize_new_player(player_t *player, client_t *client,
    game_state_t *state)
{
    memset(player, 0, sizeof(player_t));
    state->next_player_id++;
    player->id = state->next_player_id;
    player->level = 1;
    player->orientation = ORIENTATION_NORTH + (rand() % 4);
    player->is_alive = true;
    player->is_incanting = false;
    if (client->team_name)
        player->team_name = strdup(client->team_name);
    client->player_id = player->id;
    check_hatched_egg(state, client, player);
    initialize_player_inventory(player);
    setup_player_survival_time(player, state);
}

player_t *create_player_for_client(game_state_t *state, client_t *client)
{
    player_t *new_players;
    player_t *player;

    if (!state || !client)
        return NULL;
    new_players = realloc(state->players,
        sizeof(player_t) * (state->player_count + 1));
    if (!new_players)
        return NULL;
    state->players = new_players;
    player = &state->players[state->player_count];
    initialize_new_player(player, client, state);
    state->player_count++;
    printf("🆕 Player #%d created for team '%s' at (%d, %d)\n",
        player->id,
        player->team_name ? player->team_name : "Unknown",
        player->x, player->y);
    return player;
}

void initialize_player_inventory(player_t *player)
{
    if (!player)
        return;
    for (int i = 0; i < MAX_RESOURCES; i++)
        player->resources[i] = 0;
    player->resources[RESOURCE_FOOD] = 10;
}
