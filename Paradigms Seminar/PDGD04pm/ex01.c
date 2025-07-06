/*
** EPITECH PROJECT, 2025
** ex01
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "int_list.h"
#include <stdlib.h>
#include <stdio.h>

bool int_list_add_elem_at_front(int_list_t **front_ptr, int elem)
{
    int_list_t *new_elem = malloc(sizeof(int_list_t));

    if (!new_elem)
        return (false);
    new_elem->value = elem;
    new_elem->next = *front_ptr;
    *front_ptr = new_elem;
    return (true);
}

static int_list_t *create_new_int_elem(int elem)
{
    int_list_t *new_elem = malloc(sizeof(int_list_t));

    if (!new_elem)
        return (NULL);
    new_elem->value = elem;
    new_elem->next = NULL;
    return (new_elem);
}

static bool insert_int_elem_at_position(int_list_t **front_ptr,
    int_list_t *new_elem, unsigned int position)
{
    int_list_t *tmp = *front_ptr;
    unsigned int i = 0;

    if (position == 0) {
        new_elem->next = *front_ptr;
        *front_ptr = new_elem;
        return (true);
    }
    while (tmp && i < position - 1) {
        tmp = tmp->next;
        i++;
    }
    if (!tmp)
        return (false);
    new_elem->next = tmp->next;
    tmp->next = new_elem;
    return (true);
}

bool int_list_add_elem_at_position(int_list_t **front_ptr,
    int elem, unsigned int position)
{
    int_list_t *new_elem = create_new_int_elem(elem);

    if (!new_elem)
        return (false);
    return insert_int_elem_at_position(front_ptr, new_elem, position);
}
