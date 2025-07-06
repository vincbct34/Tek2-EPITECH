/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_state_display functions
*/

#include "game_state.h"

static void print_world_info(const game_state_t *state)
{
    printf("   🗺️  World Map:\n");
    printf("      📐 Dimensions:               %d x %d tiles\n",
        state->config->width, state->config->height);
    printf("      🎲 Total tiles:              %d\n",
        state->config->width * state->config->height);
    printf("      💎 Resources per tile:       Randomly distributed\n");
    printf("\n");
}

static const char *get_team_color_emoji(int index)
{
    if (index % 4 == 0)
        return "🔴";
    if (index % 4 == 1)
        return "🔵";
    if (index % 4 == 2)
        return "🟢";
    return "🟡";
}

static void print_teams_info(const game_state_t *state)
{
    printf("   👥 Team Management:\n");
    printf("      🎪 Teams initialized:        %d\n",
        state->config->team_count);
    for (int i = 0; i < state->config->team_count; i++) {
        printf("      %s %s: %d/%d slots available\n",
            get_team_color_emoji(i), state->teams[i].name,
            state->teams[i].slots_available, state->teams[i].max_slots);
    }
    printf("\n");
}

static void print_game_settings(const game_state_t *state)
{
    printf("   🎮 Game Settings:\n");
    printf("      🆔 Next Player ID:           %d\n", state->next_player_id);
    printf("      🥚 Next Egg ID:              %d\n", state->next_egg_id);
    printf("      ⏰ Game Status:              %s\n",
        state->game_running ? "🟢 RUNNING" : "🔴 STOPPED");
    printf("      🌱 Resource Respawn:         Every %d time units\n",
        RESOURCE_RESPAWN_TIME);
    printf("      🍖 Food Survival Time:       %d time units\n",
        FOOD_SURVIVAL_TIME);
    printf("\n");
}

static void print_current_stats(const game_state_t *state)
{
    printf("   📊 Current Stats:\n");
    printf("      👤 Active Players:           %d\n", state->player_count);
    printf("      🥚 Active Eggs:              %d\n", state->egg_count);
    printf("      🔗 Connected Clients:        %s\n",
        state->clients ? "Some connected" : "None yet");
    printf("\n");
}

static void print_init_header(void)
{
    printf("🎯 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("🌟                        GAME WORLD INITIALIZED      "
        "               🌟\n");
    printf("   ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("\n");
}

static void print_init_footer(void)
{
    printf("🎯 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("⚡ The world is ready! Let the battle for transcendence "
        "begin! ⚡\n");
    printf("🎯 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("\n");
}

void print_game_state_init(const game_state_t *state)
{
    if (!state || !state->config) {
        fprintf(stderr, "❌ Invalid game state\n");
        return;
    }
    print_init_header();
    print_world_info(state);
    print_teams_info(state);
    print_game_settings(state);
    print_current_stats(state);
    print_init_footer();
}
