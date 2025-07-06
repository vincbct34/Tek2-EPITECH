/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Bonus
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "string.h"
#include "new.h"

/*
* Define the String class.
*/
typedef struct
{
    Class   base;
    char    *str;
}   StringClass;

/*
* Function to construct a new String object.
*/
static void String_ctor(StringClass *this, va_list *args)
{
    const char *str = va_arg(*args, const char *);
    this->str = strdup(str);
}

/*
* Function to destruct a String object.
*/
static void String_dtor(StringClass *this)
{
    free(this->str);
}

/*
* Function to return a string representation of a String object.
*/
static char *String_str(StringClass *this)
{
    char *str;
    asprintf(&str, "<String (%s)>", this->str);
    return str;
}

/*
* Function to concatenate two String objects.
*/
static Object *String_add(const Object *first, const Object *second)
{
    StringClass *result = new(String, "");
    result->str = malloc(strlen(((StringClass *)first)->str) + strlen(((StringClass *)second)->str) + 1);
    strcpy(result->str, ((StringClass *)first)->str);
    strcat(result->str, ((StringClass *)second)->str);
    return result;
}

/*
* Function to check if two String objects are equal.
*/
static bool String_eq(const Object *first, const Object *second)
{
    return (strcmp(((StringClass *)first)->str, ((StringClass *)second)->str) == 0);
}

/*
* Function to check if the first String object is greater than the second.
*/
static bool String_gt(const Object *first, const Object *second)
{
    return (strcmp(((StringClass *)first)->str, ((StringClass *)second)->str) > 0);
}

/*
* Function to check if the first String object is less than the second.
*/
static bool String_lt(const Object *first, const Object *second)
{
    return (strcmp(((StringClass *)first)->str, ((StringClass *)second)->str) < 0);
}

/*
* Define the String class and its methods.
*/
static const StringClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(StringClass),
        .__name__ = "String",
        .__ctor__ = (ctor_t)&String_ctor,
        .__dtor__ = (dtor_t)&String_dtor,
        .__str__ = (to_string_t)&String_str,
        .__add__ = (binary_operator_t)&String_add,
        .__eq__ = (binary_comparator_t)&String_eq,
        .__gt__ = (binary_comparator_t)&String_gt,
        .__lt__ = (binary_comparator_t)&String_lt
    },
    .str = NULL
};

const Class *String = (const Class *)&_description;
