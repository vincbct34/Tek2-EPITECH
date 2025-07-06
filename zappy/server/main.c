/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** main
*/

#include "args_parser.h"
#include "run_logic.h"

int main(int argc, char **argv)
{
    server_config_t config = {0};

    if (parse_args(argc, argv, &config) != 0) {
        fprintf(stderr,
            "Usage: %s -p port -x width -y height -n name1 name2 ... -c "
            "clientsNb -f freq\n",
            argv[0]);
        return 84;
    }
    print_config(&config);
    if (run_server(&config) != 0) {
        fprintf(stderr, "💥 Server encountered an error: %s\n",
            config.error_message);
        free_config(&config);
        return 84;
    }
    free_config(&config);
    return 0;
}
