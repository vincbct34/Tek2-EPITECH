/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "list.h"
#include <stdlib.h>

unsigned int list_get_size(list_t *list)
{
    list_t *tmp = list;
    unsigned int size = 0;

    while (tmp) {
        size++;
        tmp = tmp->next;
    }
    return (size);
}

bool list_is_empty(list_t *list)
{
    return (!list);
}

bool list_add_elem_at_front(list_t **front_ptr, void *elem)
{
    list_t *new_elem = malloc(sizeof(list_t));

    if (!new_elem)
        return (false);
    new_elem->value = elem;
    new_elem->next = *front_ptr;
    *front_ptr = new_elem;
    return (true);
}

bool list_add_elem_at_back(list_t **front_ptr, void *elem)
{
    list_t *new_elem = malloc(sizeof(list_t));
    list_t *tmp = *front_ptr;

    if (!new_elem)
        return (false);
    new_elem->value = elem;
    new_elem->next = NULL;
    if (!tmp) {
        *front_ptr = new_elem;
        return (true);
    }
    while (tmp->next)
        tmp = tmp->next;
    tmp->next = new_elem;
    return (true);
}

bool list_add_elem_at_position(list_t **front_ptr, void *elem,
    unsigned int pos)
{
    list_t *new_elem = malloc(sizeof(list_t));
    list_t *tmp = *front_ptr;

    if (!new_elem)
        return false;
    new_elem->value = elem;
    if (pos == 0) {
        new_elem->next = *front_ptr;
        *front_ptr = new_elem;
        return true;
    }
    for (unsigned int i = 0; i < pos - 1 && tmp; i++)
        tmp = tmp->next;
    if (!tmp) {
        free(new_elem);
        return false;
    }
    new_elem->next = tmp->next;
    tmp->next = new_elem;
    return true;
}
