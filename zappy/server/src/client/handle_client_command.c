/*
** EPITECH PROJECT, 2025
** server
** File description:
** client_command
*/

#include "handle_client_message.h"

const command_handler_t handlers[] = {
    { is_msz, exec_msz },
    { is_mct, exec_mct },
    { is_bct, exec_bct },
    { is_tna, exec_tna },
    { is_ppo, exec_ppo },
    { is_plv, exec_plv },
    { is_pin, exec_pin },
    { is_sgt, exec_sgt },
    { is_sst, exec_sst },
    { NULL, NULL }
};

int handle_client_command(int client_fd, const char *command,
    server_config_t *config)
{
    static char error_buf[256];

    printf("[fd=%d] %s\n", client_fd, command);
    for (int i = 0; handlers[i].is_match != NULL; i++) {
        if (handlers[i].is_match(command, config))
            return handlers[i].execute(client_fd, command, config);
    }
    snprintf(error_buf, sizeof(error_buf), "Invalid command: '%s'", command);
    config->error_message = error_buf;
    return 84;
}
