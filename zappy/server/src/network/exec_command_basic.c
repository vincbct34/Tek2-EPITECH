/*
** EPITECH PROJECT, 2025
** server
** File description:
** exec_command_basic
*/

#include "exec_command.h"

int send_bct_data(int fd, long x, long y, server_config_t *cfg)
{
    char buffer[256];

    sprintf(buffer, "bct %ld %ld 0 0 0 0 0 0 0\n", x, y);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send data for coordinates (%ld, %ld)", x, y);
        return 84;
    }
    return 0;
}

int exec_msz(int fd, const char *cmd, server_config_t *cfg)
{
    char buffer[64];

    (void)cmd;
    snprintf(buffer, sizeof(buffer), "msz %d %d\n", cfg->width, cfg->height);
    if (send(fd, buffer, strlen(buffer), 0) < 0) {
        snprintf(cfg->error_message, 256,
            "Failed to send map size: %.200s", buffer);
        return 84;
    }
    return 0;
}

int exec_bct(int fd, const char *cmd, server_config_t *cfg)
{
    long x;
    long y;

    if (sscanf(cmd, "bct %ld %ld", &x, &y) != 2 || x < 0 || y < 0 ||
        x >= cfg->width || y >= cfg->height) {
        snprintf(cfg->error_message, 256,
            "Invalid coordinates: '%s'", cmd);
        return 84;
    }
    return send_bct_data(fd, x, y, cfg);
}

static int send_all_tiles_for_row(int fd, long y, server_config_t *cfg)
{
    int res;

    for (long x = 0; x < cfg->width; x++) {
        res = send_bct_data(fd, x, y, cfg);
        if (res != 0)
            return res;
    }
    return 0;
}

int exec_mct(int fd, const char *cmd, server_config_t *cfg)
{
    int res;

    (void)cmd;
    for (long y = 0; y < cfg->height; y++) {
        res = send_all_tiles_for_row(fd, y, cfg);
        if (res != 0)
            return res;
    }
    return 0;
}

int exec_tna(int fd, const char *cmd, server_config_t *cfg)
{
    char buffer[256];

    (void)cmd;
    for (int i = 0; i < cfg->team_count; i++) {
        snprintf(buffer, sizeof(buffer), "tna %s\n", cfg->team_names[i]);
        if (send(fd, buffer, strlen(buffer), 0) < 0) {
            snprintf(cfg->error_message, 256,
                "Failed to send team name: %.200s", buffer);
            return 84;
        }
    }
    return 0;
}
