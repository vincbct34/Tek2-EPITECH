/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "test.h"
#include <stdlib.h>
#include "../ex04/print.h"

Test(print_normal, test_print_normal)
{
    cr_redirect_stdout();
    print_normal("Hello World");
    fflush(stdout);
    cr_assert_stdout_eq_str("Hello World\n");
}

Test(print_reverse, test_print_reverse)
{
    cr_redirect_stdout();
    print_reverse("Hello World");
    fflush(stdout);
    cr_assert_stdout_eq_str("dlroW olleH\n");
}

Test(print_upper, test_print_upper)
{
    cr_redirect_stdout();
    print_upper("Hello World");
    fflush(stdout);
    cr_assert_stdout_eq_str("HELLO WORLD\n");
}

Test(print_42, test_print_42)
{
    cr_redirect_stdout();
    print_42("Hello World");
    fflush(stdout);
    cr_assert_stdout_eq_str("42\n");
}

Test(do_action, test_do_action)
{
    cr_redirect_stdout();
    do_action(PRINT_NORMAL, "Hello World");
    fflush(stdout);
    cr_assert_stdout_eq_str("Hello World\n");
}
