/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_manager_core
*/

#include "command_manager.h"

extern const ai_command_t ai_commands[];
extern const gui_command_t gui_commands[];

void send_existing_players(game_state_t *state, client_t *client)
{
    char response[256];

    for (int i = 0; i < state->player_count; i++) {
        if (state->players[i].is_alive) {
            snprintf(response, sizeof(response), "pnw #%d %d %d %d %d %s\n",
                state->players[i].id, state->players[i].x, state->players[i].y,
                state->players[i].orientation, state->players[i].level,
                state->players[i].team_name);
            send(client->fd, response, strlen(response), 0);
        }
    }
}

void send_map_content(game_state_t *state, client_t *client)
{
    tile_t *tile;
    char response[256];

    for (int y = 0; y < state->config->height; y++) {
        for (int x = 0; x < state->config->width; x++) {
            tile = &state->map[y][x];
            snprintf(response, sizeof(response),
                "bct %d %d %d %d %d %d %d %d %d\n", x, y,
                tile->resources[0], tile->resources[1], tile->resources[2],
                tile->resources[3], tile->resources[4], tile->resources[5],
                tile->resources[6]);
            send(client->fd, response, strlen(response), 0);
        }
    }
}

static void send_initial_gui_state(game_state_t *state, client_t *client)
{
    char response[256];

    for (int i = 0; i < state->config->team_count; i++) {
        snprintf(response, sizeof(response), "tna %s\n", state->teams[i].name);
        send(client->fd, response, strlen(response), 0);
    }
    send_map_content(state, client);
    send_existing_players(state, client);
}

static int authenticate_gui_client(game_state_t *state, client_t *client)
{
    char response[64];

    client->type = CLIENT_TYPE_GUI;
    client->authenticated = true;
    client->team_name = strdup("GRAPHIC");
    printf("📺 GUI Client authenticated! Welcome, observer!\n");
    snprintf(response, sizeof(response), "msz %d %d\n",
        state->config->width, state->config->height);
    if (send(client->fd, response, strlen(response), 0) < 0) {
        printf("❌ Failed to send map size to GUI client\n");
        return -1;
    }
    printf("📏 Sent map size (%dx%d) to GUI client\n",
        state->config->width, state->config->height);
    send_initial_gui_state(state, client);
    printf("🎮 Sent initial game state to GUI client\n");
    return 0;
}

static int send_team_info(game_state_t *state, client_t *client, int team_idx)
{
    char response[64];

    snprintf(response, sizeof(response), "%d\n",
        state->teams[team_idx].slots_available);
    send(client->fd, response, strlen(response), 0);
    snprintf(response, sizeof(response), "%d %d\n",
        state->config->width, state->config->height);
    send(client->fd, response, strlen(response), 0);
    return 0;
}

static int authenticate_ai_client(game_state_t *state, client_t *client,
    const char *team_name, int team_idx)
{
    player_t *player;

    if (state->teams[team_idx].slots_available <= 0) {
        printf("❌ Team '%s' is full! Player rejected.\n", team_name);
        return -1;
    }
    client->type = CLIENT_TYPE_AI;
    client->team_name = strdup(team_name);
    state->teams[team_idx].slots_available--;
    send_team_info(state, client, team_idx);
    client->authenticated = true;
    player = create_player_for_client(state, client);
    if (!player) {
        printf("❌ Failed to create player for team '%s'\n", team_name);
        return -1;
    }
    printf("🤖 AI Player #%d joined team '%s'! (%d slots remaining)\n",
        player->id, team_name, state->teams[team_idx].slots_available);
    notify_player_connect(state, player);
    return 0;
}

int authenticate_client(game_state_t *state, client_t *client,
    const char *team_name)
{
    if (strcmp(team_name, "GRAPHIC") == 0 ||
        strncmp(team_name, "GRAPHIC", 7) == 0) {
        return authenticate_gui_client(state, client);
    }
    for (int i = 0; i < state->config->team_count; i++) {
        if (strcmp(state->teams[i].name, team_name) == 0) {
            return authenticate_ai_client(state, client, team_name, i);
        }
    }
    printf("❌ Authentication failed for team '%s' - Unknown team!\n",
        team_name);
    return -1;
}
