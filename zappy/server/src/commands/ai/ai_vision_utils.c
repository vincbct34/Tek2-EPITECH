/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** ai_vision_utils
*/

#include "ai_vision_utils.h"
#include "command_manager.h"
#include "tile_utils.h"
#include "player_manager.h"
#include <string.h>
#include <stdlib.h>

static void add_players_to_content(char *temp, int player_count)
{
    for (int i = 0; i < player_count; i++) {
        if (strlen(temp) > 0)
            strncat(temp, " ", 255 - strlen(temp));
        strncat(temp, "player", 255 - strlen(temp));
    }
}

static void add_resources_to_content(tile_t *tile, char *temp)
{
    for (int resource = 0; resource < MAX_RESOURCES; resource++) {
        for (int count = 0; count < tile->resources[resource]; count++) {
            add_single_resource_to_content(temp, resource);
        }
    }
}

static void get_tile_contents(game_state_t *state, int x, int y, char *buffer)
{
    tile_t *tile;
    int player_count = 0;
    char temp[256] = {0};

    x = wrap_coordinate_x(x, state->config->width);
    y = wrap_coordinate_y(y, state->config->height);
    tile = get_tile_at(state, x, y);
    if (!tile) {
        buffer[0] = '\0';
        return;
    }
    count_players_at_tile(state, x, y, &player_count);
    add_players_to_content(temp, player_count);
    add_resources_to_content(tile, temp);
    strncpy(buffer, temp, 255);
    buffer[255] = '\0';
}

static void add_coordinates_for_row(const add_coords_params_t *params)
{
    int tiles_in_row = 2 * (params->vision_size - params->row) + 1;
    int start_col = -(params->vision_size - params->row);
    int rel_x;
    int rel_y;

    for (int col = 0; col < tiles_in_row; col++) {
        rel_x = start_col + col;
        rel_y = params->row;
        if (rel_x == 0 && rel_y == 0)
            continue;
        process_coordinate(params, rel_x, rel_y);
    }
}

static int calculate_total_tiles(int vision_size)
{
    int total_tiles = 0;
    int tiles_in_row;

    for (int row = 0; row <= vision_size; row++) {
        tiles_in_row = 2 * (vision_size - row) + 1;
        total_tiles += tiles_in_row;
    }
    return total_tiles;
}

static void populate_vision_coordinates(player_t *player, int vision_size,
    int **coords, int *index)
{
    add_coords_params_t params;

    for (int row = 0; row <= vision_size; row++) {
        params.player = player;
        params.row = row;
        params.vision_size = vision_size;
        params.coords = coords;
        params.index = index;
        add_coordinates_for_row(&params);
    }
}

void calculate_vision_coordinates(player_t *player, int level, int **coords,
    int *tile_count)
{
    int vision_size = level;
    int total_tiles = calculate_total_tiles(vision_size);
    int index = 0;

    *coords = malloc(total_tiles * 2 * sizeof(int));
    *tile_count = total_tiles;
    if (!*coords)
        return;
    (*coords)[index * 2] = player->x;
    (*coords)[index * 2 + 1] = player->y;
    index++;
    populate_vision_coordinates(player, vision_size, coords, &index);
    *tile_count = index;
}

void build_look_response(int *coords, int tile_count, game_state_t *state,
    char *response)
{
    int x;
    int y;
    char tile_content[256];

    for (int i = 0; i < tile_count; i++) {
        x = coords[i * 2];
        y = coords[i * 2 + 1];
        get_tile_contents(state, x, y, tile_content);
        strncat(response, tile_content, 4095 - strlen(response));
        if (i < tile_count - 1)
            strncat(response, ",", 4095 - strlen(response));
    }
}
