/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_buffer
*/

#ifndef COMMAND_BUFFER_H_
    #define COMMAND_BUFFER_H_

    #include "game_types.h"

/**
 * @brief Add a command to a client's buffer
 *
 * @param client Client to add command to
 * @param command Command string
 * @param action_time Action execution time
 * @param freq Server frequency
 * @return 0 on success, -1 on error
 */
int add_command_to_buffer(client_t *client, const char *command,
    int action_time, int freq);

/**
 * @brief Process ready commands for all clients
 *
 * @param state Game state
 */
void process_ready_commands(game_state_t *state);

#endif /* !COMMAND_BUFFER_H_ */
