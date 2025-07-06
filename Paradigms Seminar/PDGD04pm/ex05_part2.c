/*
** EPITECH PROJECT, 2025
** ex05.c
** File description:
** Paradigms Seminar - First day of C pool
*/

#include "list.h"
#include "stack.h"

void *stack_top(stack_t *stack)
{
    return (list_get_elem_at_front(stack));
}
