/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** args_factory
*/

#include "args_factory.h"

static int handle_port(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_simple_arg(argv, i, argc, &config->port);
}

static int handle_width(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_simple_arg(argv, i, argc, &config->width);
}

static int handle_height(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_simple_arg(argv, i, argc, &config->height);
}

static int handle_clients(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_simple_arg(argv, i, argc, &config->clients_per_team);
}

static int handle_freq(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_simple_arg(argv, i, argc, &config->freq);
}

static int handle_teams(char **argv,
    int *i, int argc, server_config_t *config)
{
    return parse_teams(argv, i, argc, config);
}

const arg_factory_entry_t arg_factory[] = {
    { "-p", handle_port },
    { "-x", handle_width },
    { "-y", handle_height },
    { "-c", handle_clients },
    { "-f", handle_freq },
    { "-n", handle_teams },
    { NULL, NULL }
};
