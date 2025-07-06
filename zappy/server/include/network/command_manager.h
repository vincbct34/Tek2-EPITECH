/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_manager
*/

#ifndef COMMAND_MANAGER_H_
    #define COMMAND_MANAGER_H_

    #include "game_state.h"
    #include <sys/socket.h>
    #include "gui_notifications.h"

typedef struct {
    const char *name;
    int execution_time;
    bool requires_player;
    int (*execute)(game_state_t *state, client_t *client, const char *args);
} ai_command_t;

typedef struct {
    const char *name;
    bool (*validate)(const char *command, const server_config_t *config);
    int (*execute)(game_state_t *state, client_t *client, const char *command);
} gui_command_t;

typedef struct {
    const char *cmd_name;
    const char *args;
    const char *command;
} command_params_t;

typedef struct {
    game_state_t *state;
    player_t *player;
    incantation_ritual_t *ritual;
    const incantation_requirements_t *req;
} incantation_params_t;

/**
 * @brief Parameters for adding coordinates for a row
 */
typedef struct {
    player_t *player;
    int row;
    int vision_size;
    int **coords;
    int *index;
} add_coords_params_t;

/**
 * @brief Handle authentication for a new client
 *
 * @param state Game state
 * @param client Client to authenticate
 * @param team_name Team name provided
 * @return 0 on success, -1 on error
 */
int authenticate_client(game_state_t *state, client_t *client,
    const char *team_name);

/**
 * @brief Process a command from an AI client
 *
 * @param state Game state
 * @param client AI client
 * @param command Command string
 * @return 0 on success, -1 on error
 */
int process_ai_command(game_state_t *state, client_t *client,
    const char *command);

/**
 * @brief Process a command from a GUI client
 *
 * @param state Game state
 * @param client GUI client
 * @param command Command string
 * @return 0 on success, -1 on error
 */
int process_gui_command(game_state_t *state, client_t *client,
    const char *command);

/**
 * @brief Execute a ready AI command
 *
 * @param state Game state
 * @param client AI client
 * @param entry Command buffer entry
 * @return 0 on success, -1 on error
 */
int execute_ai_command(game_state_t *state, client_t *client,
    command_buffer_entry_t *entry);

/**
 * @brief Check if the given tile coordinates are valid.
 *
 * @param state Game state
 * @param x X-coordinate
 * @param y Y-coordinate
 * @return true if the coordinates are valid, false otherwise
 */
bool is_valid_tile_coords(game_state_t *state, int x, int y);

/**
 * @brief Send the content of a tile to a client.
 *
 * @param client Client to send the tile content to
 * @param x X-coordinate of the tile
 * @param y Y-coordinate of the tile
 * @param tile Tile to send
 */
void send_tile_content(client_t *client, int x, int y, tile_t *tile);

// AI Commands
int ai_forward(game_state_t *state, client_t *client, const char *args);
int ai_right(game_state_t *state, client_t *client, const char *args);
int ai_left(game_state_t *state, client_t *client, const char *args);
int ai_look(game_state_t *state, client_t *client, const char *args);
int ai_inventory(game_state_t *state, client_t *client, const char *args);
int ai_broadcast(game_state_t *state, client_t *client, const char *args);
int ai_connect_nbr(game_state_t *state, client_t *client, const char *args);
int ai_fork(game_state_t *state, client_t *client, const char *args);
int ai_eject(game_state_t *state, client_t *client, const char *args);
int ai_take(game_state_t *state, client_t *client, const char *args);
int ai_set(game_state_t *state, client_t *client, const char *args);
int ai_incantation(game_state_t *state, client_t *client, const char *args);

// GUI Commands
int gui_msz(game_state_t *state, client_t *client, const char *command);
int gui_bct(game_state_t *state, client_t *client, const char *command);
int gui_mct(game_state_t *state, client_t *client, const char *command);
int gui_tna(game_state_t *state, client_t *client, const char *command);
int gui_ppo(game_state_t *state, client_t *client, const char *command);
int gui_plv(game_state_t *state, client_t *client, const char *command);
int gui_pin(game_state_t *state, client_t *client, const char *command);
int gui_sgt(game_state_t *state, client_t *client, const char *command);
int gui_sst(game_state_t *state, client_t *client, const char *command);

#endif /* !COMMAND_MANAGER_H_ */
