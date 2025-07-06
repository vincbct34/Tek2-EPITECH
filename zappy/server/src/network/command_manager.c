/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_manager implementation
*/

#include "command_manager.h"
#include "command_buffer.h"

static const ai_command_t ai_commands[] = {
    {"Forward", 7, true, ai_forward},
    {"Right", 7, true, ai_right},
    {"Left", 7, true, ai_left},
    {"Look", 7, true, ai_look},
    {"Inventory", 1, true, ai_inventory},
    {"Broadcast", 7, true, ai_broadcast},
    {"Connect_nbr", 0, false, ai_connect_nbr},
    {"Fork", 42, true, ai_fork},
    {"Eject", 7, true, ai_eject},
    {"Take", 7, true, ai_take},
    {"Set", 7, true, ai_set},
    {"Incantation", 300, true, ai_incantation},
    {NULL, 0, false, NULL}
};

static const gui_command_t gui_commands[] = {
    {"msz", NULL, gui_msz},
    {"bct", NULL, gui_bct},
    {"mct", NULL, gui_mct},
    {"tna", NULL, gui_tna},
    {"ppo", NULL, gui_ppo},
    {"plv", NULL, gui_plv},
    {"pin", NULL, gui_pin},
    {"sgt", NULL, gui_sgt},
    {"sst", NULL, gui_sst},
    {NULL, NULL, NULL}
};

static int execute_immediate_command(game_state_t *state, client_t *client,
    const char *args, int cmd_index)
{
    return ai_commands[cmd_index].execute(state, client, args);
}

static int execute_buffered_command(client_t *client, const char *command,
    int cmd_index, int freq)
{
    return add_command_to_buffer(client, command,
        ai_commands[cmd_index].execution_time, freq);
}

static int execute_found_command(game_state_t *state, client_t *client,
    command_params_t *params, int cmd_index)
{
    if (ai_commands[cmd_index].execution_time == 0)
        return execute_immediate_command(state, client,
            params->args, cmd_index);
    return execute_buffered_command(client, params->command, cmd_index,
        state->config->freq);
}

static int find_and_execute_ai_command(game_state_t *state, client_t *client,
    command_params_t *params)
{
    for (int i = 0; ai_commands[i].name; i++) {
        if (strcmp(ai_commands[i].name, params->cmd_name) == 0)
            return execute_found_command(state, client, params, i);
    }
    send(client->fd, "ko\n", 3, 0);
    return 0;
}

int process_ai_command(game_state_t *state, client_t *client,
    const char *command)
{
    char *cmd_copy = strdup(command);
    char *cmd_name = strtok(cmd_copy, " \n");
    char *args = strtok(NULL, "\n");
    command_params_t params = {cmd_name, args, command};
    int result;

    if (!cmd_name) {
        free(cmd_copy);
        send(client->fd, "ko\n", 3, 0);
        return 0;
    }
    result = find_and_execute_ai_command(state, client, &params);
    free(cmd_copy);
    return result;
}

int process_gui_command(game_state_t *state, client_t *client,
    const char *command)
{
    for (int i = 0; gui_commands[i].name; i++) {
        if (strncmp(gui_commands[i].name, command,
            strlen(gui_commands[i].name)) == 0) {
            return gui_commands[i].execute(state, client, command);
        }
    }
    send(client->fd, "suc\n", 4, 0);
    return 0;
}

int execute_ai_command(game_state_t *state, client_t *client,
    command_buffer_entry_t *entry)
{
    char *cmd_copy = strdup(entry->command);
    char *cmd_name = strtok(cmd_copy, " \n");
    char *args = strtok(NULL, "\n");
    int result = -1;

    if (!cmd_name) {
        free(cmd_copy);
        send(client->fd, "ko\n", 3, 0);
        return 0;
    }
    for (int i = 0; ai_commands[i].name; i++) {
        if (strcmp(ai_commands[i].name, cmd_name) == 0) {
            result = ai_commands[i].execute(state, client, args);
            break;
        }
    }
    free(cmd_copy);
    if (result == -1)
        send(client->fd, "ko\n", 3, 0);
    return result;
}

// Notification functions moved to gui_notifications.c
