/*
** EPITECH PROJECT, 2025
** server
** File description:
** exec_command
*/

#ifndef EXEC_COMMAND_H_
    #define EXEC_COMMAND_H_

    #include "utils.h"
    #include <stdio.h>
    #include <string.h>
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <poll.h>
    #include <unistd.h>

int exec_msz(int fd, const char *cmd, server_config_t *cfg);
int exec_bct(int fd, const char *cmd, server_config_t *cfg);
int exec_mct(int fd, const char *cmd, server_config_t *cfg);
int exec_tna(int fd, const char *cmd, server_config_t *cfg);
int exec_ppo(int fd, const char *cmd, server_config_t *cfg);
int exec_plv(int fd, const char *cmd, server_config_t *cfg);
int exec_pin(int fd, const char *cmd, server_config_t *cfg);
int exec_sgt(int fd, const char *cmd, server_config_t *cfg);
int exec_sst(int fd, const char *cmd, server_config_t *cfg);

#endif /* !EXEC_COMMAND_H_ */
