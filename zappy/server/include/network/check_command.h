/*
** EPITECH PROJECT, 2025
** server
** File description:
** check_command
*/

#ifndef CHECK_COMMAND_H_
    #define CHECK_COMMAND_H_

    #include "utils.h"
    #include <stdbool.h>

// GUI command check functions
bool is_msz(const char *cmd, const server_config_t *cfg);
bool is_bct(const char *cmd, const server_config_t *cfg);
bool is_mct(const char *cmd, const server_config_t *cfg);
bool is_tna(const char *cmd, const server_config_t *cfg);
bool is_ppo(const char *cmd, const server_config_t *cfg);
bool is_plv(const char *cmd, const server_config_t *cfg);
bool is_pin(const char *cmd, const server_config_t *cfg);
bool is_sgt(const char *cmd, const server_config_t *cfg);
bool is_sst(const char *cmd, const server_config_t *cfg);

#endif /* !CHECK_COMMAND_H_ */
