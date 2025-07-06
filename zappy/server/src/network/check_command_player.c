/*
** EPITECH PROJECT, 2025
** server
** File description:
** check_command_player
*/

#include "check_command.h"

bool is_ppo(const char *cmd, const server_config_t *cfg)
{
    char *endptr;
    long id;

    if (strncmp(cmd, "ppo #", 5) != 0 ||
        cmd[strlen(cmd) - 1] != '\n')
        return false;
    id = strtol(cmd + 5, &endptr, 10);
    if (*endptr != '\n' || id < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return false;
    }
    if (id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Player ID out of range: %ld", id);
        return false;
    }
    return true;
}

bool is_plv(const char *cmd, const server_config_t *cfg)
{
    long id;

    if (strncmp(cmd, "plv #", 5) != 0 ||
        cmd[strlen(cmd) - 1] != '\n')
        return false;
    if (sscanf(cmd, "plv #%ld\n", &id) != 1 || id < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return false;
    }
    if (id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Player ID out of range: %ld", id);
        return false;
    }
    return true;
}

bool is_pin(const char *cmd, const server_config_t *cfg)
{
    long id;

    if (strncmp(cmd, "pin #", 5) != 0 ||
        cmd[strlen(cmd) - 1] != '\n')
        return false;
    if (sscanf(cmd, "pin #%ld\n", &id) != 1 || id < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return false;
    }
    if (id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Player ID out of range: %ld", id);
        return false;
    }
    return true;
}

bool is_sst(const char *cmd, const server_config_t *cfg)
{
    long time;

    if (strncmp(cmd, "sst ", 4) != 0 ||
        cmd[strlen(cmd) - 1] != '\n')
        return false;
    if (sscanf(cmd, "sst %ld\n", &time) != 1 || time < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid time value: '%s'", cmd + 4);
        return false;
    }
    return true;
}
