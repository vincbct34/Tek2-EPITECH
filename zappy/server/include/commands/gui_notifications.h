/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** gui_notifications
*/

#ifndef GUI_NOTIFICATIONS_H_
    #define GUI_NOTIFICATIONS_H_

    #include "game_state.h"

typedef struct incantation_info_s {
    int x;
    int y;
    int level;
    int *players;
    int player_count;
} incantation_info_t;

void notify_all_drops(game_state_t *state, player_t *player, int i);

/**
 * @brief Broadcast a message to all GUI clients
 *
 * @param state Game state
 * @param message Message to broadcast
 */
void broadcast_to_gui_clients(game_state_t *state, const char *message);

/**
 * @brief Send a notification to all GUI clients
 *
 * @param state Game state
 * @param message Message to send
 */
void notify_gui_clients(game_state_t *state, const char *message);

/**
 * @brief Notify GUI of a new player connection
 *
 * @param state Game state
 * @param player Player that connected
 */
void notify_player_connect(game_state_t *state, player_t *player);

/**
 * @brief Notify GUI of player position change
 *
 * @param state Game state
 * @param player Player that moved
 */
void notify_player_position(game_state_t *state, player_t *player);

/**
 * @brief Notify GUI of player level change
 *
 * @param state Game state
 * @param player Player that leveled up
 */
void notify_player_level(game_state_t *state, player_t *player);

/**
 * @brief Notify GUI of player inventory change
 *
 * @param state Game state
 * @param player Player whose inventory changed
 */
void notify_player_inventory(game_state_t *state, player_t *player);

/**
 * @brief Notify GUI of player death
 *
 * @param state Game state
 * @param player_id ID of the player that died
 */
void notify_player_death(game_state_t *state, int player_id);

/**
 * @brief Notify GUI of resource drop
 *
 * @param state Game state
 * @param player_id Player who dropped the resource
 * @param resource_type Type of resource dropped
 */
void notify_resource_drop(game_state_t *state, int player_id,
    resource_type_t resource_type);

/**
 * @brief Notify GUI of resource pickup
 *
 * @param state Game state
 * @param player_id Player who picked up the resource
 * @param resource_type Type of resource picked up
 */
void notify_resource_pickup(game_state_t *state, int player_id,
    resource_type_t resource_type);

/**
 * @brief Notify GUI of incantation start
 *
 * @param state Game state
 * @param info Incantation information
 */
void notify_incantation_start(game_state_t *state,
    const incantation_info_t *info);

/**
 * @brief Notify GUI of incantation end
 *
 * @param state Game state
 * @param x X coordinate
 * @param y Y coordinate
 * @param success Whether the incantation succeeded
 */
void notify_incantation_end(game_state_t *state, int x, int y, bool success);

/**
 * @brief Notify GUI of egg laying
 *
 * @param state Game state
 * @param player_id Player who laid the egg
 */
void notify_egg_laying(game_state_t *state, int player_id);

/**
 * @brief Notify GUI of new egg
 *
 * @param state Game state
 * @param egg Egg that was laid
 */
void notify_egg_laid(game_state_t *state, egg_t *egg);

/**
 * @brief Notify GUI that an egg is starting to hatch
 *
 * @param state Game state
 * @param egg_id ID of the egg that is hatching
 */
void notify_egg_hatching(game_state_t *state, int egg_id);

/**
 * @brief Notify GUI of egg connection (player connecting to egg)
 *
 * @param state Game state
 * @param egg_id ID of the egg that connected
 */
void notify_egg_connect(game_state_t *state, int egg_id);

/**
 * @brief Notify GUI of egg death
 *
 * @param state Game state
 * @param egg_id ID of the egg that died
 */
void notify_egg_death(game_state_t *state, int egg_id);

/**
 * @brief Notify GUI of player expulsion
 *
 * @param state Game state
 * @param player_id Player who performed the expulsion
 */
void notify_player_expulsion(game_state_t *state, int player_id);

/**
 * @brief Notify GUI of broadcast
 *
 * @param state Game state
 * @param player_id Player who broadcast
 * @param message Message that was broadcast
 */
void notify_broadcast(game_state_t *state, int player_id, const char *message);

/**
 * @brief Notify GUI of game end
 *
 * @param state Game state
 * @param winning_team Name of the winning team
 */
void notify_game_end(game_state_t *state, const char *winning_team);

/**
 * @brief Send a server message to GUI clients
 *
 * @param state Game state
 * @param message Message to send
 */
void notify_server_message(game_state_t *state, const char *message);

/**
 * @brief Notify GUI of tile content change
 *
 * @param state Game state
 * @param x X coordinate of the tile
 * @param y Y coordinate of the tile
 */
void notify_tile_content(game_state_t *state, int x, int y);

#endif /* !GUI_NOTIFICATIONS_H_ */
