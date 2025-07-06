/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Configuration display utilities for server configuration
*/

#include "args_parser.h"

static void print_header(void)
{
    printf("\n");
    printf("🎮 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("🏆                         ZAPPY SERVER CONFIGURATION"
        "                 🏆\n");
    printf("   ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("\n");
}

static void print_network_config(const server_config_t *config)
{
    printf("   🌐 Network Configuration:\n");
    printf("      📡 Port:                     %d\n", config->port);
    printf("\n");
}

static void print_world_config(const server_config_t *config)
{
    printf("   🗺️  World Configuration:\n");
    printf("      📏 World Size:               %d x %d\n",
        config->width, config->height);
    printf("      ⏱️  Frequency:               %d time units per second\n",
        config->freq);
    printf("\n");
}

static void print_team_config(const server_config_t *config)
{
    printf("   👥 Team Configuration:\n");
    printf("      🔢 Number of Teams:          %d\n", config->team_count);
    printf("      👤 Clients per Team:         %d\n", config->clients_per_team);
    printf("\n");
}

static void print_team_names(const server_config_t *config)
{
    printf("   🏷️  Team Names:\n");
    for (int i = 0; i < config->team_count; i++) {
        printf("      %d. %s%s%s\n", i + 1, get_team_icon(i),
            config->team_names[i], get_team_color_name(i));
    }
    printf("\n");
}

static void print_footer(void)
{
    printf("   🚀 Server Status:              READY TO START\n");
    printf("\n");
    printf("🎮 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("✨ May the best team win! Good luck! ✨\n");
    printf("🎮 ════════════════════════════════════════════════════"
        "════════════════\n");
    printf("\n");
}

void print_config(const server_config_t *config)
{
    if (!config) {
        fprintf(stderr, "❌ Invalid configuration\n");
        return;
    }
    print_header();
    print_network_config(config);
    print_world_config(config);
    print_team_config(config);
    print_team_names(config);
    print_footer();
}

const char *get_team_icon(int index)
{
    const char *icons[] = {"🔴", "🔵", "🟢", "🟡"};

    if (index >= 0 && index < 4)
        return icons[index];
    return "⚪";
}

const char *get_team_color_name(int index)
{
    if (index == 0) {
        return " (Red Team)";
    }
    if (index == 1) {
        return " (Blue Team)";
    }
    if (index == 2) {
        return " (Green Team)";
    }
    return " (Yellow Team)";
}
