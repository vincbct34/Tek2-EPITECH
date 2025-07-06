/*
** EPITECH PROJECT, 2025
** ex06.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "queue.h"
#include "list.h"

void *queue_front(queue_t *queue)
{
    return (list_get_elem_at_front(queue));
}
