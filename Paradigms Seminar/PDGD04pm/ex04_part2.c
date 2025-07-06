/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "list.h"
#include <stdlib.h>

void *list_get_elem_at_front(list_t *list)
{
    if (!list)
        return (0);
    return (list->value);
}

void *list_get_elem_at_back(list_t *list)
{
    list_t *tmp = list;

    if (!list)
        return (0);
    while (tmp->next)
        tmp = tmp->next;
    return (tmp->value);
}

void *list_get_elem_at_position(list_t *list, unsigned int position)
{
    list_t *tmp = list;
    unsigned int i = 0;

    if (position == 0 || !list)
        return (list_get_elem_at_front(list));
    while (tmp && i < position) {
        tmp = tmp->next;
        i++;
    }
    if (!tmp)
        return (0);
    return (tmp->value);
}

bool list_del_elem_at_front(list_t **front_ptr)
{
    list_t *tmp = *front_ptr;

    if (!tmp)
        return (false);
    *front_ptr = tmp->next;
    free(tmp);
    return (true);
}

bool list_del_elem_at_back(list_t **front_ptr)
{
    list_t *tmp = *front_ptr;
    list_t *prev = NULL;

    if (!tmp)
        return (false);
    while (tmp->next) {
        prev = tmp;
        tmp = tmp->next;
    }
    if (prev)
        prev->next = NULL;
    else
        *front_ptr = NULL;
    free(tmp);
    return (true);
}
