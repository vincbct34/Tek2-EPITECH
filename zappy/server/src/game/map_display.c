/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Map display implementation
*/

#include "map_display.h"

static void print_map_header(game_state_t *state)
{
    printf("\n🗺️  ═══════════════════════════════════════════════════════\n");
    printf("🌍                    WORLD MAP (%dx%d)                  \n",
        state->config->width, state->config->height);
    printf("🗺️  ═══════════════════════════════════════════════════════\n");
}

static void print_map_legend(void)
{
    printf("\n📖 Legend:\n");
    printf("   %s.%s = Empty\t\t%s#%s = Food\t%sL%s = Linemate\n",
        GRAY, RESET, GREEN, RESET, YELLOW, RESET);
    printf("   %sD%s = Deraumere\t%sS%s = Sibur\t%sM%s = Mendiane\n",
        BLUE, RESET, CYAN, RESET, MAGENTA, RESET);
    printf("   %sP%s = Phiras\t\t%sT%s = Thystame\n",
        RED, RESET, WHITE, RESET);
    printf("   %s+%s = Multiple resources on same tile\n", YELLOW, RESET);
    printf("\n");
}

static void print_map_row(game_state_t *state, int y)
{
    int x;
    tile_t *tile;

    printf("%2d│ ", y);
    for (x = 0; x < state->config->width; x++) {
        tile = get_tile_at(state, x, y);
        if (tile)
            printf("%s%c%s ", get_tile_color(tile), get_tile_char(tile),
                RESET);
        else
            printf("? ");
    }
    printf("│%2d\n", y);
}

static void print_borders_and_content(game_state_t *state)
{
    int y;
    int x;

    printf("    ");
    for (x = 0; x < state->config->width; x++)
        printf("%2d", x % 10);
    printf("\n    ");
    for (x = 0; x < state->config->width; x++)
        printf("──");
    printf("\n");
    for (y = 0; y < state->config->height; y++)
        print_map_row(state, y);
    printf("    ");
    for (x = 0; x < state->config->width; x++)
        printf("──");
    printf("\n    ");
    for (x = 0; x < state->config->width; x++)
        printf("%2d", x % 10);
}

void display_map(game_state_t *state)
{
    if (!state || !state->map) {
        printf("❌ Invalid map state\n");
        return;
    }
    print_map_header(state);
    print_map_legend();
    print_borders_and_content(state);
    printf("\n🗺️  ═══════════════════════════════════════════════════════\n");
}
