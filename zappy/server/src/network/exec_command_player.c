/*
** EPITECH PROJECT, 2025
** server
** File description:
** exec_command_player
*/

#include "exec_command.h"

int exec_ppo(int fd, const char *cmd, server_config_t *cfg)
{
    long id;
    char buffer[256];

    if (sscanf(cmd, "ppo #%ld", &id) != 1 || id < 0 ||
        id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return 84;
    }
    snprintf(buffer, sizeof(buffer), "ppo #%ld 0 0 0\n", id);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send player position: %.200s", buffer);
        return 84;
    }
    return 0;
}

int exec_plv(int fd, const char *cmd, server_config_t *cfg)
{
    long id;
    char buffer[256];

    if (sscanf(cmd, "plv #%ld", &id) != 1 || id < 0 ||
        id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return 84;
    }
    snprintf(buffer, sizeof(buffer), "plv #%ld 0\n", id);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send player level: %.200s", buffer);
        return 84;
    }
    return 0;
}

int exec_pin(int fd, const char *cmd, server_config_t *cfg)
{
    long id;
    char buffer[256];

    if (sscanf(cmd, "pin #%ld", &id) != 1 || id < 0 ||
        id >= cfg->team_count * cfg->clients_per_team) {
        snprintf(cfg->error_message, 256,
            "Invalid player ID: '%s'", cmd + 5);
        return 84;
    }
    snprintf(buffer, sizeof(buffer), "pin #%ld 0 0 0\n", id);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send player inventory: %.200s", buffer);
        return 84;
    }
    return 0;
}

int exec_sgt(int fd, const char *cmd, server_config_t *cfg)
{
    int freq;
    char buffer[64];

    if (sscanf(cmd, "sgt %d", &freq) != 1 || freq < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid frequency: '%s'", cmd + 4);
        return 84;
    }
    cfg->freq = freq;
    snprintf(buffer, sizeof(buffer), "sgt %d\n", freq);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send frequency: %.200s", buffer);
        return 84;
    }
    return 0;
}

int exec_sst(int fd, const char *cmd, server_config_t *cfg)
{
    long time;
    char buffer[64];

    if (sscanf(cmd, "sst %ld", &time) != 1 || time < 0) {
        snprintf(cfg->error_message, 256,
            "Invalid time: '%s'", cmd + 4);
        return 84;
    }
    snprintf(buffer, sizeof(buffer), "sst %ld\n", time);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send time: %.200s", buffer);
        return 84;
    }
    return 0;
}
