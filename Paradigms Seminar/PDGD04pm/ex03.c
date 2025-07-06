/*
** EPITECH PROJECT, 2025
** ex03.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "int_list.h"
#include <stdlib.h>

bool int_list_del_elem_at_front(int_list_t **front_ptr)
{
    int_list_t *tmp = *front_ptr;

    if (!tmp)
        return (false);
    *front_ptr = tmp->next;
    free(tmp);
    return (true);
}

bool int_list_del_elem_at_back(int_list_t **front_ptr)
{
    int_list_t *tmp = *front_ptr;
    int_list_t *prev = NULL;

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

bool int_list_del_elem_at_position(int_list_t **front_ptr,
    unsigned int position)
{
    int_list_t *tmp = *front_ptr;
    int_list_t *prev = NULL;
    unsigned int i = 0;

    if (position == 0 || !tmp)
        return (int_list_del_elem_at_front(front_ptr));
    while (tmp && i < position) {
        prev = tmp;
        tmp = tmp->next;
        i++;
    }
    if (!tmp)
        return (false);
    if (prev)
        prev->next = tmp->next;
    else
        *front_ptr = tmp->next;
    free(tmp);
    return (true);
}
