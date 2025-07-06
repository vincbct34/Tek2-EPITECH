/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** server_main
*/

#include "run_logic.h"

void handle_server_event(int server_socket,
    struct pollfd *fds, int *nfds, game_state_t *state)
{
    if (accept_new_connection(server_socket, fds, nfds, state) < 0)
        fprintf(stderr, "%s\n", state->config->error_message);
}

void handle_poll_events(struct pollfd *fds,
    int *nfds, int server_socket, game_state_t *state)
{
    for (int i = 0; i < *nfds; ++i) {
        if (!(fds[i].revents & POLLIN))
            continue;
        if (fds[i].fd == server_socket)
            handle_server_event(server_socket, fds, nfds, state);
        else
            handle_client_event(fds, nfds, i, state);
    }
}

void close_all_sockets(struct pollfd *fds, int nfds)
{
    for (int i = 0; i < nfds; ++i)
        close(fds[i].fd);
}

static int setup_polling(int server_socket, struct pollfd *fds,
    int max_clients)
{
    memset(fds, 0, sizeof(struct pollfd) * (max_clients + 1));
    fds[0].fd = server_socket;
    fds[0].events = POLLIN;
    return 1;
}

static int initialize_server_resources(server_config_t *config,
    game_state_t *state)
{
    int server_socket = setup_server(config);

    if (server_socket < 0)
        return -1;
    if (init_game_state(state, config) != 0) {
        close(server_socket);
        return -1;
    }
    print_game_state_init(state);
    return server_socket;
}

static void run_main_loop(game_state_t *state, struct pollfd *fds,
    int *nfds, int server_socket)
{
    int poll_result;

    while (state->game_running) {
        poll_result = poll(fds, *nfds, 100);
        if (poll_result < 0) {
            printf("❌ Network polling failed - Server shutting down...\n");
            break;
        }
        if (poll_result > 0)
            handle_poll_events(fds, nfds, server_socket, state);
        update_game_state(state);
        if (check_win_condition(state)) {
            printf("🏆 Victory condition reached! Game ending...\n");
            state->game_running = false;
        }
    }
}

int run_server(server_config_t *config)
{
    struct pollfd fds[config->clients_per_team * config->team_count + 1];
    int nfds;
    game_state_t state;
    int server_socket = initialize_server_resources(config, &state);

    if (server_socket < 0)
        return -1;
    nfds = setup_polling(server_socket, fds,
        config->clients_per_team * config->team_count);
    run_main_loop(&state, fds, &nfds, server_socket);
    printf("🔚 Server shutting down gracefully...\n");
    close_all_sockets(fds, nfds);
    free_game_state(&state);
    printf("✅ Server stopped successfully. Thanks for playing Zappy!\n");
    return 0;
}
