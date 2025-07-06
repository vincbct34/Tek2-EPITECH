/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** args_parser
*/

#ifndef ARGS_PARSER_H_
    #define ARGS_PARSER_H_

    #include "args_factory.h"
    #include "utils.h"

    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

/**
 * @brief Parse command line arguments
 *
 * @param argc Argument count
 * @param argv Argument vector
 * @param config Server configuration structure
 * @return 0 on success, -1 on error
 */
int parse_args(int argc, char **argv, server_config_t *config);

/**
 * @brief Initialize the server configuration structure
 *
 * @param config Server configuration structure to initialize
 */
void init_config(server_config_t *config);
/**
 * @brief Free the server configuration structure
 *
 * @param config Server configuration structure to free
 */
void free_config(server_config_t *config);

/**
 * @brief Print the server configuration in a nice format
 *
 * @param config Server configuration structure to print
 */
void print_config(const server_config_t *config);

#endif /* !ARGS_PARSER_H_ */
