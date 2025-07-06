/*
** EPITECH PROJECT, 2025
** Zappy [WSL: Ubuntu-24.04]
** File description:
** argsparser
*/

#include "args_parser.h"

void init_config(server_config_t *config)
{
    config->port = 0;
    config->width = 0;
    config->height = 0;
    config->team_names = NULL;
    config->team_count = 0;
    config->clients_per_team = 0;
    config->freq = 0;
    config->error_message = NULL;
}

static int handle_argument(int argc,
    char **argv, int *i, server_config_t *config)
{
    int result;

    for (const arg_factory_entry_t *entry = arg_factory;
        entry->flag;
        ++entry) {
        if (strcmp(argv[*i], entry->flag) != 0)
            continue;
        result = entry->handler(argv, i, argc, config);
        return result < 0 ? -1 : 1;
    }
    fprintf(stderr, "Unknown argument: %s\n", argv[*i]);
    return -1;
}

int parse_args(int argc, char **argv, server_config_t *config)
{
    int handled;

    if (!config)
        return -1;
    init_config(config);
    for (int i = 1; i < argc; i++) {
        handled = handle_argument(argc, argv, &i, config);
        if (handled < 0)
            return -1;
        if (handled == 0) {
            fprintf(stderr, "Unknown argument: %s\n", argv[i]);
            return -1;
        }
    }
    if (!config->port || !config->width || !config->height ||
        !config->clients_per_team || !config->freq || config->team_count == 0)
        return -1;
    return 0;
}

void free_config(server_config_t *config)
{
    if (config && config->team_names) {
        free(config->team_names);
        config->team_names = NULL;
    }
}
