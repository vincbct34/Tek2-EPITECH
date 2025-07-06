/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** utils
*/

#include "utils.h"

int parse_simple_arg(char **argv, int *i, int argc, int *field)
{
    if (*i + 1 >= argc)
        return -1;
    (*i)++;
    for (int j = 0; argv[*i][j]; j++)
        if (argv[*i][j] < '0' || argv[*i][j] > '9')
            return -1;
    *field = atoi(argv[*i]);
    return 0;
}

int is_flag(const char *arg)
{
    return arg && (
        strcmp(arg, "-p") == 0 ||
        strcmp(arg, "-x") == 0 ||
        strcmp(arg, "-y") == 0 ||
        strcmp(arg, "-c") == 0 ||
        strcmp(arg, "-f") == 0 ||
        strcmp(arg, "-n") == 0
    );
}

int count_teams(char **argv, int start, int argc)
{
    int count = 0;

    while (start < argc && !is_flag(argv[start])) {
        count++;
        start++;
    }
    return count;
}

int parse_teams(char **argv, int *i, int argc, server_config_t *config)
{
    int team_start = *i + 1;

    config->team_count = count_teams(argv, team_start, argc);
    if (config->team_count == 0)
        return -1;
    config->team_names = malloc(sizeof(char *) * config->team_count);
    if (!config->team_names)
        return -1;
    for (int j = 0; j < config->team_count; ++j)
        config->team_names[j] = argv[team_start + j];
    *i += config->team_count;
    return 0;
}
