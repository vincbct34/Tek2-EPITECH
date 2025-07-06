/*
** EPITECH PROJECT, 2025
** ex04.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "list.h"
#include <stdlib.h>

bool list_del_first_elem(list_t **front_ptr)
{
    list_t *tmp = *front_ptr;

    if (!tmp)
        return (false);
    *front_ptr = tmp->next;
    free(tmp);
    return (true);
}

bool list_del_elem_at_position(list_t **front_ptr, unsigned int position)
{
    list_t *tmp = *front_ptr;
    list_t *prev = NULL;
    unsigned int i = 0;

    if (position == 0)
        return list_del_first_elem(front_ptr);

    while (tmp && i < position) {
        prev = tmp;
        tmp = tmp->next;
        i++;
    }
    if (!tmp)
        return (false);
    if (prev)
        prev->next = tmp->next;
    free(tmp);
    return (true);
}

void list_clear(list_t **front_ptr)
{
    list_t *tmp = *front_ptr;
    list_t *next = NULL;

    while (tmp) {
        next = tmp->next;
        free(tmp);
        tmp = next;
    }
    *front_ptr = NULL;
}

void list_dump(list_t *list, value_displayer_t val_disp)
{
    list_t *tmp = list;

    while (tmp) {
        val_disp(tmp->value);
        tmp = tmp->next;
    }
}
