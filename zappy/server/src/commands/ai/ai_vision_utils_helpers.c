/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** ai_vision_utils_helpers
*/

#include "ai_vision_utils.h"
#include <string.h>

void add_single_resource_to_content(char *temp, int resource)
{
    if (strlen(temp) > 0)
        strncat(temp, " ", 255 - strlen(temp));
    strncat(temp, get_resource_name(resource), 255 - strlen(temp));
}

void process_coordinate(const add_coords_params_t *params,
    int rel_x, int rel_y)
{
    coord_transform_params_t transform_params;
    coord_transform_result_t result;

    transform_params.rel_x = rel_x;
    transform_params.rel_y = rel_y;
    transform_params.base_x = params->player->x;
    transform_params.base_y = params->player->y;
    transform_params.orientation = params->player->orientation;
    transform_relative_coordinates(&transform_params, &result);
    (*(params->coords))[*(params->index) * 2] = result.abs_x;
    (*(params->coords))[*(params->index) * 2 + 1] = result.abs_y;
    (*(params->index))++;
}
