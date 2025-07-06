/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 04
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "float.h"
#include "new.h"

/*
* Define the Float class
*/
typedef struct
{
    Class   base;
    float nb;
}   FloatClass;

/*
* Function to create a new Float object
*/
static void Float_ctor(FloatClass *this, va_list *args)
{
    this->nb = va_arg(*args, double);
}

/*
* Function to delete a Float object
*/
static void Float_dtor(FloatClass *this)
{
    (void)this;
}

/*
* Function to get a string representation of a Float object
*/
static char *Float_str(FloatClass *this)
{
    char *str;

    asprintf(&str, "<Float (%f)>", this->nb);
    return str;
}

/*
* Function to add two Float objects
*/
static Object *Float_add(const Object *first, const Object *second)
{
    FloatClass *result = new(Float, 0);

    result->nb = ((FloatClass *)first)->nb + ((FloatClass *)second)->nb;
    return result;
}

/*
* Function to subtract two Float objects
*/
static Object *Float_sub(const Object *first, const Object *second)
{
    FloatClass *result = new(Float, 0);

    result->nb = ((FloatClass *)first)->nb - ((FloatClass *)second)->nb;
    return result;
}

/*
* Function to multiply two Float objects
*/
static Object *Float_mul(const Object *first, const Object *second)
{
    FloatClass *result = new(Float, 0);

    result->nb = ((FloatClass *)first)->nb * ((FloatClass *)second)->nb;
    return result;
}

/*
* Function to divide two Float objects
*/
static Object *Float_div(const Object *first, const Object *second)
{
    FloatClass *result = new(Float, 0);

    result->nb = ((FloatClass *)first)->nb / ((FloatClass *)second)->nb;
    return result;
}

/*
* Function to check if two Float objects are equal
*/
static bool Float_eq(const Object *first, const Object *second)
{
    return (((FloatClass *)first)->nb == ((FloatClass *)second)->nb);
}

/*
* Function to check if one Float object is greater than another
*/
static bool Float_gt(const Object *first, const Object *second)
{
    return (((FloatClass *)first)->nb > ((FloatClass *)second)->nb);
}

/*
* Function to check if one Float object is less than another
*/
static bool Float_lt(const Object *first, const Object *second)
{
    return (((FloatClass *)first)->nb < ((FloatClass *)second)->nb);
}

/*
* Define the Float class and its methods
*/
static const FloatClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(FloatClass),
        .__name__ = "Float",
        .__ctor__ = (ctor_t)&Float_ctor,
        .__dtor__ = (dtor_t)&Float_dtor,
        .__str__ = (to_string_t)&Float_str,
        .__add__ = (binary_operator_t)&Float_add,
        .__sub__ = (binary_operator_t)&Float_sub,
        .__mul__ = (binary_operator_t)&Float_mul,
        .__div__ = (binary_operator_t)&Float_div,
        .__eq__ = (binary_comparator_t)&Float_eq,
        .__gt__ = (binary_comparator_t)&Float_gt,
        .__lt__ = (binary_comparator_t)&Float_lt
    },
    .nb = 0
};

const Class *Float = (const Class *)&_description;