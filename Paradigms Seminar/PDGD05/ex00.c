/*
** EPITECH PROJECT, 2025
** ex00.c
** File description:
** Paradigms Seminar - Second day of C pool
*/

#include "string.h"
#include <string.h>

void string_init(string_t *this, const char *s)
{
    if (!this || !s)
        return;
    this->str = malloc(sizeof(char) * (strlen(s) + 1));
    this->str = strcpy(this->str, s);
    this->assign_s = &assign_s;
    this->assign_c = &assign_c;
    this->append_s = &append_s;
    this->append_c = &append_c;
    this->at = &at_pos;
    this->clear = &clear;
    this->length = &length;
    this->compare_s = &compare_s;
    this->compare_c = &compare_c;
    this->copy = &copy;
    this->c_str = &c_str;
    this->empty = &empty;
    this->find_s = &find_s;
    this->find_c = &find_c;
}

void string_destroy(string_t *this)
{
    if (!this)
        return;
    free(this->str);
    this->str = NULL;
}
