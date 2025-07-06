/*
** EPITECH PROJECT, 2025
** server
** File description:
** check_command_basic
*/

#include "check_command.h"

bool is_msz(const char *cmd, const server_config_t *cfg)
{
    (void)cfg;
    return strcmp(cmd, "msz\n") == 0;
}

bool is_bct(const char *cmd, const server_config_t *cfg)
{
    char *endptr;
    long x;
    long y;

    if (strncmp(cmd, "bct ", 4) != 0)
        return false;
    if (cmd[strlen(cmd) - 1] != '\n' || cmd[strlen(cmd) - 2] != ' ')
        return false;
    x = strtol(cmd + 4, &endptr, 10);
    if (*endptr != ' ')
        return false;
    y = strtol(endptr + 1, &endptr, 10);
    if (*endptr != '\n' || x < 0 || y < 0)
        return false;
    if (x >= cfg->width || y >= cfg->height) {
        snprintf(cfg->error_message, 256,
            "Invalid coordinates: (%ld, %ld)", x, y);
        return false;
    }
    return true;
}

bool is_mct(const char *cmd, const server_config_t *cfg)
{
    (void)cfg;
    return strcmp(cmd, "mct\n") == 0;
}

bool is_tna(const char *cmd, const server_config_t *cfg)
{
    (void)cfg;
    return strcmp(cmd, "tna\n") == 0;
}

bool is_sgt(const char *cmd, const server_config_t *cfg)
{
    (void)cfg;
    return strcmp(cmd, "sgt\n") == 0;
}
