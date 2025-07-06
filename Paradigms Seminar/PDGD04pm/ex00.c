/*
** EPITECH PROJECT, 2025
** ex00.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "int_list.h"
#include <stdlib.h>
#include <stdio.h>

bool int_list_add_elem_at_back(int_list_t **front_ptr, int elem)
{
    int_list_t *new_elem = malloc(sizeof(int_list_t));
    int_list_t *tmp = *front_ptr;

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

void int_list_dump(int_list_t *list)
{
    int_list_t *tmp = list;

    while (tmp) {
        printf("%d\n", tmp->value);
        tmp = tmp->next;
    }
}

unsigned int int_list_get_size(int_list_t *list)
{
    int_list_t *tmp = list;
    unsigned int size = 0;

    while (tmp) {
        size++;
        tmp = tmp->next;
    }
    return (size);
}

bool int_list_is_empty(int_list_t *list)
{
    return (!list);
}

void int_list_clear(int_list_t **front_ptr)
{
    int_list_t *tmp = *front_ptr;
    int_list_t *next = NULL;

    while (tmp) {
        next = tmp->next;
        free(tmp);
        tmp = next;
    }
    *front_ptr = NULL;
}
