/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** run_logic with poll and multiple clients
*/

#include "run_logic.h"

static int bind_socket(int server_socket,
    struct sockaddr_in *server_addr, char **error_message)
{
    if (bind(server_socket, (struct sockaddr *)server_addr,
        sizeof(*server_addr)) < 0) {
        *error_message = "Failed to bind socket";
        return -1;
    }
    return 0;
}

static int listen_socket(int server_socket, server_config_t **config)
{
    int max_clients = (*config)->clients_per_team * (*config)->team_count;

    if (listen(server_socket, max_clients) < 0) {
        (*config)->error_message = "Failed to listen on socket";
        return -1;
    }
    return 0;
}

static int create_and_configure_socket(server_config_t *config,
    struct sockaddr_in *server_addr)
{
    int server_socket = socket(AF_INET, SOCK_STREAM, 0);

    if (!config || server_socket < 0) {
        config->error_message = "Failed to create socket";
        return -1;
    }
    server_addr->sin_family = AF_INET;
    server_addr->sin_addr.s_addr = INADDR_ANY;
    server_addr->sin_port = htons(config->port);
    return server_socket;
}

int setup_server(server_config_t *config)
{
    struct sockaddr_in server_addr;
    int server_socket = create_and_configure_socket(config, &server_addr);

    if (server_socket < 0)
        return -1;
    if (bind_socket(server_socket, &server_addr, &config->error_message) < 0) {
        close(server_socket);
        return -1;
    }
    if (listen_socket(server_socket, &config) < 0) {
        close(server_socket);
        return -1;
    }
    printf("🚀 Server is now listening on port %d - Waiting for "
        "connections...\n", config->port);
    return server_socket;
}
