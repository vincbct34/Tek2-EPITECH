/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 04
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "int.h"
#include "new.h"

/*
* Define the Int class
*/
typedef struct
{
    Class   base;
    int nb;
}   IntClass;

/*
* Function to create a new Int object
*/
static void Int_ctor(IntClass *this, va_list *args)
{
    this->nb = va_arg(*args, int);
}

/*
* Function to delete an Int object
*/
static void Int_dtor(IntClass *this)
{
    (void)this;
}

/*
* Function to get a string representation of an Int object
*/
static char *Int_str(IntClass *this)
{
    char *str;

    asprintf(&str, "<Int (%d)>", this->nb);
    return str;
}

/*
* Function to add two Int objects
*/
static Object *Int_add(const Object *first, const Object *second)
{
    IntClass *result = new(Int, 0);

    result->nb = ((IntClass *)first)->nb + ((IntClass *)second)->nb;
    return result;
}

/*
* Function to subtract two Int objects
*/
static Object *Int_sub(const Object *first, const Object *second)
{
    IntClass *result = new(Int, 0);

    result->nb = ((IntClass *)first)->nb - ((IntClass *)second)->nb;
    return result;
}

/*
* Function to multiply two Int objects
*/
static Object *Int_mul(const Object *first, const Object *second)
{
    IntClass *result = new(Int, 0);

    result->nb = ((IntClass *)first)->nb * ((IntClass *)second)->nb;
    return result;
}

/*
* Function to divide two Int objects
*/
static Object *Int_div(const Object *first, const Object *second)
{
    IntClass *result = new(Int, 0);

    result->nb = ((IntClass *)first)->nb / ((IntClass *)second)->nb;
    return result;
}

/*
* Function to check if two Int objects are equal
*/
static bool Int_eq(const Object *first, const Object *second)
{
    return (((IntClass *)first)->nb == ((IntClass *)second)->nb);
}

/*
* Function to check if the first Int object is greater than the second
*/
static bool Int_gt(const Object *first, const Object *second)
{
    return (((IntClass *)first)->nb > ((IntClass *)second)->nb);
}

/*
* Function to check if the first Int object is less than the second
*/
static bool Int_lt(const Object *first, const Object *second)
{
    return (((IntClass *)first)->nb < ((IntClass *)second)->nb);
}

/*
* Define the Int class and its methods
*/
static const IntClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(IntClass),
        .__name__ = "Int",
        .__ctor__ = (ctor_t)&Int_ctor,
        .__dtor__ = (dtor_t)&Int_dtor,
        .__str__ = (to_string_t)&Int_str,
        .__add__ = (binary_operator_t)&Int_add,
        .__sub__ = (binary_operator_t)&Int_sub,
        .__mul__ = (binary_operator_t)&Int_mul,
        .__div__ = (binary_operator_t)&Int_div,
        .__eq__ = (binary_comparator_t)&Int_eq,
        .__gt__ = (binary_comparator_t)&Int_gt,
        .__lt__ = (binary_comparator_t)&Int_lt
    },
    .nb = 0
};

const Class *Int = (const Class *)&_description;