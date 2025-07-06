/*
** EPITECH PROJECT, 2025
** Zappy Tests
** File description:
** Simple unit tests for args parsing structures
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "args_parser.h"
#include "utils.h"

Test(simple_args, init_config_basic)
{
    server_config_t config;
    
    init_config(&config);
    
    cr_assert_eq(config.port, 0);
    cr_assert_eq(config.width, 0);
    cr_assert_eq(config.height, 0);
    cr_assert_eq(config.team_count, 0);
    cr_assert_eq(config.clients_per_team, 0);
    cr_assert_eq(config.freq, 0);
    cr_assert_null(config.team_names);
    cr_assert_null(config.error_message);
}

Test(simple_args, parse_simple_arg_valid_number)
{
    char *argv[] = {"program", "-p", "4242"};
    int argc = 3;
    int i = 1;
    int field = 0;
    int result = parse_simple_arg(argv, &i, argc, &field);
    
    cr_assert_eq(result, 0);
    cr_assert_eq(field, 4242);
    cr_assert_eq(i, 2);
}

Test(simple_args, parse_simple_arg_invalid_non_numeric)
{
    char *argv[] = {"program", "-p", "abc"};
    int argc = 3;
    int i = 1;
    int field = 0;
    int result = parse_simple_arg(argv, &i, argc, &field);
    
    cr_assert_eq(result, -1);
}

Test(simple_args, parse_simple_arg_missing_value)
{
    char *argv[] = {"program", "-p"};
    int argc = 2;
    int i = 1;
    int field = 0;
    int result = parse_simple_arg(argv, &i, argc, &field);
    
    cr_assert_eq(result, -1);
}

Test(simple_args, count_teams_basic)
{
    char *argv[] = {"program", "-n", "team1", "team2", "team3", "-p", "4242"};
    int argc = 7;
    int count = count_teams(argv, 2, argc);
    
    cr_assert_eq(count, 3);
}

Test(simple_args, count_teams_no_teams)
{
    char *argv[] = {"program", "-n", "-p", "4242"};
    int argc = 4;
    int count = count_teams(argv, 2, argc);
    
    cr_assert_eq(count, 0);
}
