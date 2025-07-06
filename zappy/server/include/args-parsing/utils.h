/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** utils
*/

#ifndef UTILS_H_
    #define UTILS_H_

    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

/**
 * @brief Server configuration structure
 */
typedef struct {
    int port;
    int width;
    int height;
    char **team_names;
    int team_count;
    int clients_per_team;
    int freq;
    char *error_message;
} server_config_t;

typedef int (*arg_handler_t)(char **argv,
    int *i, int argc, server_config_t *config);

/**
 * @brief Argument factory entry structure
 */
typedef struct {
    const char *flag;
    arg_handler_t handler;
} arg_factory_entry_t;

/**
 * @brief Parse a simple argument
 *
 * @param argv Argument vector
 * @param i Current argument index
 * @param argc Argument count
 * @param field Pointer to the field to set
 * @return 0 on success, -1 on error
 */
int parse_simple_arg(char **argv, int *i, int argc, int *field);
/**
 * @brief Count the number of teams in the argument vector
 *
 * @param argv Argument vector
 * @param start Starting index to count from
 * @param argc Argument count
 * @return Number of teams found
 */
int count_teams(char **argv, int start, int argc);
/**
 * @brief Parse the team names from the argument vector
 *
 * @param argv Argument vector
 * @param i Current argument index
 * @param argc Argument count
 * @param config Server configuration structure to fill
 * @return 0 on success, -1 on error
 */
int parse_teams(char **argv, int *i, int argc, server_config_t *config);

/**
 * @brief Get the icon for a team based on its index
 *
 * @param index Team index
 * @return Icon string for the team
 */
const char *get_team_icon(int index);

/**
 * @brief Get the color name for a team based on its index
 *
 * @param index Team index
 * @return Color name string for the team
 */
const char *get_team_color_name(int index);

#endif /* !UTILS_H_ */
