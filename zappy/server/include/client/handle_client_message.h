/*
** EPITECH PROJECT, 2025
** server
** File description:
** handle_client_message
*/

#ifndef HANDLE_CLIENT_MESSAGE_H_
    #define HANDLE_CLIENT_MESSAGE_H_

    #include "utils.h"
    #include "check_command.h"
    #include "exec_command.h"
    #include <stdbool.h>

/**
 * @brief Command handler structure
 */
typedef struct {
    bool (*is_match)(const char *command,
        const server_config_t *config);
    int (*execute)(int client_fd, const char *command,
        server_config_t *config);
} command_handler_t;

/**
 * @brief Handles a client command by checking if it matches any known command
 * and executing the corresponding action.
 *
 * @param client_fd The file descriptor of the client.
 * @param command The command received from the client.
 * @param config The server configuration.
 * @return int Returns 0 on success, or an error code on failure.
 */
int handle_client_command(int client_fd, const char *command,
    server_config_t *config);

#endif /* !HANDLE_CLIENT_MESSAGE_H_ */
