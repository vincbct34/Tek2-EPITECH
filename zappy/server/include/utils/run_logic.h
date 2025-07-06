/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** run_logic
*/

#ifndef RUN_LOGIC_H_
    #define RUN_LOGIC_H_

    #include "args_parser.h"
    #include "game_state.h"
    #include "time_manager.h"
    #include "command_manager.h"
    #include "gui_notifications.h"

    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <poll.h>
    #include <unistd.h>

/**
 * @brief Run the server with the given configuration
 *
 * @param config Server configuration
 * @return 0 on success, -1 on error
 */
int run_server(server_config_t *config);

/**
 * @brief Setup the server with the given configuration
 *
 * @param config Server configuration
 * @return 0 on success, -1 on error
 */
int setup_server(server_config_t *config);

/**
 * @brief Accept a new client connection.
 *
 * @param server_socket Server socket
 * @param fds Array of pollfd structures
 * @param nfds Pointer to the number of active file descriptors
 * @param state Game state
 * @return 0 on success, -1 on error
 */
int accept_new_connection(int server_socket, struct pollfd *fds, int *nfds,
    game_state_t *state);

/**
 * @brief Receive data from a client.
 *
 * @param client_fd Client socket file descriptor
 * @param buffer Buffer to store received data
 * @param size Size of the buffer
 * @return Number of bytes received, -1 on error
 */
int receive_client_data(int client_fd, char *buffer, size_t size);

/**
 * @brief Log a message from a client.
 *
 * @param client_fd Client socket file descriptor
 * @param buffer Buffer containing the message
 */
void log_client_message(int client_fd, const char *buffer);

/**
 * @brief Process input from a client.
 *
 * @param index Index of the client in the pollfd array
 * @param fds Array of pollfd structures
 * @param nfds Pointer to the number of active file descriptors
 * @param state Game state
 * @return 0 on success, -1 on error
 */
int process_client_input(int index, struct pollfd *fds, int *nfds,
    game_state_t *state);

/**
 * @brief Handle events for a client.
 *
 * @param fds Array of pollfd structures
 * @param nfds Pointer to the number of active file descriptors
 * @param index Index of the client in the pollfd array
 * @param state Game state
 */
void handle_client_event(struct pollfd *fds, int *nfds, int index,
    game_state_t *state);

/**
 * @brief Handle events for the server.
 *
 * @param server_socket Server socket
 * @param fds Array of pollfd structures
 * @param nfds Pointer to the number of active file descriptors
 * @param state Game state
 */
void handle_server_event(int server_socket, struct pollfd *fds, int *nfds,
    game_state_t *state);

/**
 * @brief Handle poll events.
 *
 * @param fds Array of pollfd structures
 * @param nfds Pointer to the number of active file descriptors
 * @param server_socket Server socket
 * @param state Game state
 */
void handle_poll_events(struct pollfd *fds, int *nfds, int server_socket,
    game_state_t *state);

/**
 * @brief Close all sockets.
 *
 * @param fds Array of pollfd structures
 * @param nfds Number of active file descriptors
 */
void close_all_sockets(struct pollfd *fds, int nfds);

/**
 * @brief Update the game state.
 *
 * This function updates the game state by processing player actions,
 * resource spawning, and other game logic.
 *
 * @param state Game state to update
 */
void update_game_state(game_state_t *state);

/**
 * @brief Check win condition.
 *
 * This function checks if a player has met the win condition.
 *
 * @param state Game state to check
 * @return 1 if win condition is met, 0 otherwise
 */
int check_win_condition(game_state_t *state);

/**
 * @brief Cleanup dead players.
 *
 * This function removes dead players from the game state and updates
 * the necessary data structures.
 *
 * @param state Game state to clean up
 */
void cleanup_dead_players(game_state_t *state);

/**
 * @brief Process eggs hatching.
 *
 * This function checks for eggs that are ready to hatch and processes them
 * by creating new players and updating the game state accordingly.
 *
 * @param state Game state to process
 */
void process_eggs_hatching(game_state_t *state);

#endif /* !RUN_LOGIC_H_ */
