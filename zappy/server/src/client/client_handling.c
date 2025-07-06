/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client_handling
*/

#include "run_logic.h"

static int add_client_to_fds(int client_socket,
    struct pollfd *fds, int *nfds, game_state_t *state)
{
    client_t *client = add_client(state, client_socket);

    if (!client)
        return -1;
    fds[*nfds].fd = client_socket;
    fds[*nfds].events = POLLIN;
    fds[*nfds].revents = 0;
    (*nfds)++;
    return 0;
}

static void remove_client_from_fds(int index, struct pollfd *fds, int *nfds,
    game_state_t *state)
{
    int fd = fds[index].fd;

    close(fd);
    remove_client(state, fd);
    for (int i = index; i < *nfds - 1; i++)
        fds[i] = fds[i + 1];
    (*nfds)--;
}

static int welcome_client(int client_socket, game_state_t *state)
{
    if (send(client_socket, "WELCOME\n", 8, 0) < 0) {
        state->config->error_message = "Failed to send welcome message";
        close(client_socket);
        return -1;
    }
    printf("🔗 New connection established! (fd=%d) - Awaiting "
        "authentication...\n", client_socket);
    return 0;
}

int accept_new_connection(int server_socket,
    struct pollfd *fds, int *nfds, game_state_t *state)
{
    int client_socket = accept(server_socket, NULL, NULL);

    if (client_socket < 0) {
        state->config->error_message = "Failed to accept connection";
        return -1;
    }
    if (welcome_client(client_socket, state) < 0)
        return -1;
    return add_client_to_fds(client_socket, fds, nfds, state);
}

int receive_client_data(int client_fd, char *buffer, size_t size)
{
    ssize_t bytes_received = recv(client_fd, buffer, size - 1, 0);

    if (bytes_received <= 0)
        return -1;
    buffer[bytes_received] = '\0';
    return bytes_received;
}

static int handle_client_authentication(game_state_t *state, client_t *client,
    const char *buffer)
{
    char *team_name = strdup(buffer);
    char *newline = strchr(team_name, '\n');

    if (newline)
        *newline = '\0';
    if (authenticate_client(state, client, team_name) == 0) {
        free(team_name);
        return 0;
    }
    free(team_name);
    close(client->fd);
    return -1;
}

static int handle_authenticated_client(game_state_t *state, client_t *client,
    const char *buffer)
{
    if (client->type == CLIENT_TYPE_AI)
        return process_ai_command(state, client, buffer);
    else if (client->type == CLIENT_TYPE_GUI)
        return process_gui_command(state, client, buffer);
    return -1;
}

int process_client_input(int index,
    struct pollfd *fds, int *nfds, game_state_t *state)
{
    char buffer[1024];
    int client_fd = fds[index].fd;
    int result = receive_client_data(client_fd, buffer, sizeof(buffer));
    client_t *client = find_client(state, client_fd);

    if (result < 0 || !client) {
        printf("👋 Client disconnected gracefully (fd=%d) - See you "
            "next time!\n", client_fd);
        remove_client_from_fds(index, fds, nfds, state);
        return 0;
    }
    if (!client->authenticated) {
        if (handle_client_authentication(state, client, buffer) < 0) {
            remove_client_from_fds(index, fds, nfds, state);
            return 0;
        }
        return 0;
    }
    return handle_authenticated_client(state, client, buffer);
}

void handle_client_event(struct pollfd *fds,
    int *nfds, int index, game_state_t *state)
{
    if (process_client_input(index, fds, nfds, state) < 0)
        fprintf(stderr, "⚠️  Client error: %s\n",
            state->config->error_message);
}
