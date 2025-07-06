/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** run_logic utility functions
*/

#include "utils/run_logic.h"
#include <stdio.h>

void log_client_message(int client_fd, const char *buffer)
{
    if (buffer != NULL) {
        printf("[fd=%d] %s", client_fd, buffer);
    }
}
