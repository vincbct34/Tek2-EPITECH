/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Forward declarations for game types
*/

#ifndef GAME_TYPES_H_
    #define GAME_TYPES_H_

    #include <sys/time.h>
    #include <stdbool.h>
    #include "../args-parsing/utils.h"

    #define MAX_RESOURCES 7 // Number of resource types
    #define MAX_COMMANDS_BUFFER 10 // Maximum number of commands in the buffer
    #define FOOD_SURVIVAL_TIME 126
    #define RESOURCE_RESPAWN_TIME 20 // Time in units for resources to respawn

    #define RESET   "\033[0m"
    #define RED     "\033[31m"
    #define GREEN   "\033[32m"
    #define YELLOW  "\033[33m"
    #define BLUE    "\033[34m"
    #define MAGENTA "\033[35m"
    #define CYAN    "\033[36m"
    #define WHITE   "\033[37m"
    #define GRAY    "\033[90m"

/**
 * @brief Enumeration of client types.
 */
typedef enum {
    CLIENT_TYPE_UNKNOWN,
    CLIENT_TYPE_AI,
    CLIENT_TYPE_GUI
} client_type_t;

/**
 * @brief Enumeration of command types.
 */
typedef enum {
    RESOURCE_FOOD = 0,
    RESOURCE_LINEMATE = 1,
    RESOURCE_DERAUMERE = 2,
    RESOURCE_SIBUR = 3,
    RESOURCE_MENDIANE = 4,
    RESOURCE_PHIRAS = 5,
    RESOURCE_THYSTAME = 6
} resource_type_t;

/**
 * @brief Enumeration of player orientations.
 */
typedef enum {
    ORIENTATION_NORTH = 1,
    ORIENTATION_EAST = 2,
    ORIENTATION_SOUTH = 3,
    ORIENTATION_WEST = 4
} orientation_t;

/**
 * @brief Position structure representing coordinates on the map.
 */
typedef struct {
    int x;
    int y;
} position_t;

/**
 * @brief Tile structure representing a tile on the map.
 */
typedef struct {
    int resources[MAX_RESOURCES];
} tile_t;

/**
 * @brief Player structure representing a player in the game.
 */
typedef struct {
    int id;
    int x;
    int y;
    orientation_t orientation;
    int level;
    int resources[MAX_RESOURCES];
    char *team_name;
    struct timeval last_action_time;
    struct timeval death_time;
    bool is_alive;
    bool is_incanting;
} player_t;

/**
 * @brief Egg structure representing an egg in the game.
 */
typedef struct {
    int id;
    int x;
    int y;
    char *team_name;
    int player_id;
    struct timeval creation_time;
    bool is_ready_to_hatch;
} egg_t;

/**
 * @brief Command buffer entry structure for
 * storing commands and their execution time.
 */
typedef struct {
    char *command;
    struct timeval execution_time;
} command_buffer_entry_t;

/**
 * @brief Client structure representing a connected client.
 */
typedef struct client_s {
    int fd;
    client_type_t type;
    char *team_name;
    int player_id;
    bool authenticated;
    command_buffer_entry_t command_buffer[MAX_COMMANDS_BUFFER];
    int buffer_size;
    char input_buffer[1024];
    int input_length;
    struct client_s *next;
} client_t;

/**
 * @brief Team structure representing a team in the game.
 */
typedef struct {
    char *name;
    int slots_available;
    int max_slots;
} team_t;

/**
 * @brief Map tile statistics structure for tracking tile statistics.
 */
typedef struct {
    int total_tiles;
    int empty_tiles;
    int multi_resource_tiles;
} map_tile_stats_t;

/**
 * @brief Incantation requirements structure for tracking resource
 * requirements for incantations.
 */
typedef struct {
    int players_needed;
    int linemate;
    int deraumere;
    int sibur;
    int mendiane;
    int phiras;
    int thystame;
} incantation_requirements_t;

/**
 * @brief Incantation ritual structure for tracking active incantation rituals.
 */
typedef struct {
    int player_ids[10];
    int player_count;
    int x;
    int y;
    int target_level;
    struct timeval start_time;
    bool is_active;
} incantation_ritual_t;

/**
 * @brief Game state structure representing the current state of the game.
 */
typedef struct {
    server_config_t *config;
    tile_t **map;
    player_t *players;
    int player_count;
    int next_player_id;
    egg_t *eggs;
    int egg_count;
    int next_egg_id;
    team_t *teams;
    client_t *clients;
    struct timeval start_time;
    struct timeval last_resource_spawn;
    bool game_running;
    incantation_ritual_t active_rituals[32];
    int ritual_count;
} game_state_t;

#endif /* !GAME_TYPES_H_ */
