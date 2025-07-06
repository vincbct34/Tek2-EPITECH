/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Map statistics implementation
*/

#include "map_display.h"

static void count_resources_in_tile(tile_t *tile, int *resource_counts,
    int *resources_on_tile)
{
    int r;

    *resources_on_tile = 0;
    for (r = 0; r < MAX_RESOURCES; r++) {
        resource_counts[r] += tile->resources[r];
        if (tile->resources[r] > 0)
            (*resources_on_tile)++;
    }
}

static void process_tile(tile_t *tile, int *resource_counts,
    map_tile_stats_t *stats)
{
    int resources_on_tile = 0;

    if (!tile)
        return;
    (stats->total_tiles)++;
    count_resources_in_tile(tile, resource_counts, &resources_on_tile);
    if (resources_on_tile == 0)
        (stats->empty_tiles)++;
    else if (resources_on_tile > 1)
        (stats->multi_resource_tiles)++;
}

static void process_map_row(game_state_t *state, int y,
    int *resource_counts, map_tile_stats_t *stats)
{
    int x;
    tile_t *tile;

    for (x = 0; x < state->config->width; x++) {
        tile = get_tile_at(state, x, y);
        process_tile(tile, resource_counts, stats);
    }
}

static void calculate_map_stats(game_state_t *state,
    int *resource_counts, map_tile_stats_t *stats)
{
    int y;

    for (y = 0; y < state->config->height; y++) {
        process_map_row(state, y, resource_counts, stats);
    }
}

static void print_stats_header(void)
{
    printf("\n📊 ═══════════════════════════════════════════════════════\n");
    printf("📈                        MAP STATISTICS                    \n");
    printf("📊 ═══════════════════════════════════════════════════════\n");
}

static void print_overview_stats(int total_tiles, int empty_tiles,
    int multi_resource_tiles)
{
    printf("\n🌍 Map Overview:\n");
    printf("   📐 Total tiles:              %d\n", total_tiles);
    printf("   🌫️  Empty tiles:              %d (%.1f%%)\n",
        empty_tiles, (float)empty_tiles / total_tiles * 100);
    printf("   🎯 Multi-resource tiles:      %d (%.1f%%)\n",
        multi_resource_tiles,
        (float)multi_resource_tiles / total_tiles * 100);
}

static void print_resource_distribution(int *resource_counts, int total_tiles)
{
    int r;
    float density;

    printf("\n📦 Resource Distribution:\n");
    for (r = 0; r < MAX_RESOURCES; r++) {
        density = (float)resource_counts[r] / total_tiles;
        printf("   %-12s %4d units (density: %.3f)\n",
            get_resource_name(r), resource_counts[r], density);
    }
    printf("\n📊 ═══════════════════════════════════════════════════════\n");
}

void display_map_statistics(game_state_t *state)
{
    int resource_counts[MAX_RESOURCES] = {0};
    map_tile_stats_t stats = {0, 0, 0};

    if (!state || !state->map) {
        printf("❌ Invalid map state for statistics\n");
        return;
    }
    calculate_map_stats(state, resource_counts, &stats);
    print_stats_header();
    print_overview_stats(stats.total_tiles, stats.empty_tiles,
        stats.multi_resource_tiles);
    print_resource_distribution(resource_counts, stats.total_tiles);
}
