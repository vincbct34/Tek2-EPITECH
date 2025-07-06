/*
** EPITECH PROJECT, 2025
** ex06.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include <criterion/criterion.h>
#include "queue.h"

Test(queue, queue_get_size)
{
    queue_t *queue = NULL;
    cr_assert_eq(queue_get_size(queue), 0);
    queue_push(&queue, (void *)1);
    cr_assert_eq(queue_get_size(queue), 1);
    queue_push(&queue, (void *)2);
    cr_assert_eq(queue_get_size(queue), 2);
    queue_clear(&queue);
}

Test(queue, queue_is_empty)
{
    queue_t *queue = NULL;
    cr_assert(queue_is_empty(queue));
    queue_push(&queue, (void *)1);
    cr_assert_not(queue_is_empty(queue));
    queue_clear(&queue);
}

Test(queue, queue_push)
{
    queue_t *queue = NULL;
    cr_assert(queue_push(&queue, (void *)1));
    cr_assert_eq(queue_get_size(queue), 1);
    queue_clear(&queue);
}

Test(queue, queue_pop)
{
    queue_t *queue = NULL;
    queue_push(&queue, (void *)1);
    queue_push(&queue, (void *)2);
    cr_assert(queue_pop(&queue));
    cr_assert_eq(queue_get_size(queue), 1);
    cr_assert(queue_pop(&queue));
    cr_assert_eq(queue_get_size(queue), 0);
    cr_assert_not(queue_pop(&queue));
    queue_clear(&queue);
}

Test(queue, queue_clear)
{
    queue_t *queue = NULL;
    queue_push(&queue, (void *)1);
    queue_push(&queue, (void *)2);
    queue_clear(&queue);
    cr_assert_eq(queue_get_size(queue), 0);
}

Test(queue, queue_front)
{
    queue_t *queue = NULL;
    queue_push(&queue, (void *)1);
    queue_push(&queue, (void *)2);
    cr_assert_eq(queue_front(queue), (void *)1);
    queue_pop(&queue);
    cr_assert_eq(queue_front(queue), (void *)2);
    queue_clear(&queue);
}
