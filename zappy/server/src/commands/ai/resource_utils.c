/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Resource utilities for AI commands
*/

#include "resource_utils.h"
#include "game_types.h"
#include <string.h>

int get_resource_type_from_string(const char *resource_name)
{
    if (!resource_name)
        return -1;
    if (strcmp(resource_name, "food") == 0)
        return RESOURCE_FOOD;
    if (strcmp(resource_name, "linemate") == 0)
        return RESOURCE_LINEMATE;
    if (strcmp(resource_name, "deraumere") == 0)
        return RESOURCE_DERAUMERE;
    if (strcmp(resource_name, "sibur") == 0)
        return RESOURCE_SIBUR;
    if (strcmp(resource_name, "mendiane") == 0)
        return RESOURCE_MENDIANE;
    if (strcmp(resource_name, "phiras") == 0)
        return RESOURCE_PHIRAS;
    if (strcmp(resource_name, "thystame") == 0)
        return RESOURCE_THYSTAME;
    return -1;
}
