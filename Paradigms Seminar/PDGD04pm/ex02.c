/*
** EPITECH PROJECT, 2025
** ex02.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "int_list.h"
#include <stdlib.h>
#include <stdio.h>

int int_list_get_elem_at_front(int_list_t *list)
{
    if (!list)
        return (0);
    return (list->value);
}

int int_list_get_elem_at_back(int_list_t *list)
{
    int_list_t *tmp = list;

    if (!list)
        return (0);
    while (tmp->next)
        tmp = tmp->next;
    return (tmp->value);
}

int int_list_get_elem_at_position(int_list_t *list, unsigned int position)
{
    int_list_t *tmp = list;
    unsigned int i = 0;

    if (position == 0 || !list)
        return (int_list_get_elem_at_front(list));
    while (tmp && i < position) {
        tmp = tmp->next;
        i++;
    }
    if (!tmp)
        return (0);
    return (tmp->value);
}
