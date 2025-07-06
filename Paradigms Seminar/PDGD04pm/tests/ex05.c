/*
** EPITECH PROJECT, 2025
** ex05.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "stack.h"

Test(stack, stack_get_size)
{
    stack_t *stack = NULL;
    cr_assert_eq(stack_get_size(stack), 0);
    stack_push(&stack, (void *)1);
    cr_assert_eq(stack_get_size(stack), 1);
    stack_push(&stack, (void *)2);
    cr_assert_eq(stack_get_size(stack), 2);
    stack_clear(&stack);
}

Test(stack, stack_is_empty)
{
    stack_t *stack = NULL;
    cr_assert(stack_is_empty(stack));
    stack_push(&stack, (void *)1);
    cr_assert_not(stack_is_empty(stack));
    stack_clear(&stack);
}

Test(stack, stack_push)
{
    stack_t *stack = NULL;
    cr_assert(stack_push(&stack, (void *)1));
    cr_assert_eq(stack_get_size(stack), 1);
    stack_clear(&stack);
}

Test(stack, stack_pop)
{
    stack_t *stack = NULL;
    stack_push(&stack, (void *)1);
    stack_push(&stack, (void *)2);
    cr_assert(stack_pop(&stack));
    cr_assert_eq(stack_get_size(stack), 1);
    cr_assert(stack_pop(&stack));
    cr_assert_eq(stack_get_size(stack), 0);
    cr_assert_not(stack_pop(&stack));
    stack_clear(&stack);
}

Test(stack, stack_clear)
{
    stack_t *stack = NULL;
    stack_push(&stack, (void *)1);
    stack_push(&stack, (void *)2);
    stack_clear(&stack);
    cr_assert_eq(stack_get_size(stack), 0);
}

Test(stack, stack_top)
{
    stack_t *stack = NULL;
    stack_push(&stack, (void *)1);
    stack_push(&stack, (void *)2);
    cr_assert_eq(stack_top(stack), (void *)2);
    stack_pop(&stack);
    cr_assert_eq(stack_top(stack), (void *)1);
    stack_clear(&stack);
}