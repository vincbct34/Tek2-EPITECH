/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_buffer implementation
*/

#include "command_manager.h"

static long long calculate_action_time(int action_time, int freq)
{
    return (action_time * 1000) / freq;
}

static void calculate_execution_time(command_buffer_entry_t *entry,
    int action_time, int freq)
{
    long long start_ms;
    long long duration;

    duration = calculate_action_time(action_time, freq);
    start_ms = entry->execution_time.tv_sec * 1000 +
        entry->execution_time.tv_usec / 1000;
    start_ms += duration;
    entry->execution_time.tv_sec = start_ms / 1000;
    entry->execution_time.tv_usec = (start_ms % 1000) * 1000;
}

int add_command_to_buffer(client_t *client, const char *command,
    int action_time, int freq)
{
    if (client->buffer_size >= MAX_COMMANDS_BUFFER)
        return -1;
    client->command_buffer[client->buffer_size].command = strdup(command);
    if (!client->command_buffer[client->buffer_size].command)
        return -1;
    gettimeofday(&client->command_buffer[client->buffer_size].execution_time,
        NULL);
    calculate_execution_time(&client->command_buffer[client->buffer_size],
        action_time, freq);
    client->buffer_size++;
    return 0;
}

static void remove_command_from_buffer(client_t *client, int index)
{
    free(client->command_buffer[index].command);
    for (int i = index; i < client->buffer_size - 1; i++)
        client->command_buffer[i] = client->command_buffer[i + 1];
    client->buffer_size--;
}

static void process_client_commands(game_state_t *state, client_t *client)
{
    if (client->type != CLIENT_TYPE_AI || client->buffer_size <= 0)
        return;
    if (is_action_ready(&client->command_buffer[0].execution_time, 0)) {
        execute_ai_command(state, client, &client->command_buffer[0]);
        remove_command_from_buffer(client, 0);
    }
}

void process_ready_commands(game_state_t *state)
{
    client_t *client = state->clients;

    while (client) {
        process_client_commands(state, client);
        client = client->next;
    }
}
