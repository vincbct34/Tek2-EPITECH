/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 04
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "char.h"
#include "new.h"

/*
* Define the Char class
*/
typedef struct
{
    Class   base;
    char    caracter;
}   CharClass;

/*
* Function to create a new Char object
*/
static void Char_ctor(CharClass *this, va_list *args)
{
    this->caracter = (char)va_arg(*args, int);
}

/*
* Function to delete a Char object
*/
static void Char_dtor(CharClass *this)
{
    (void)this;
}

/*
* Function to get a string representation of a Char object
*/
static char *Char_str(CharClass *this)
{
    char *str;

    asprintf(&str, "<Char (%c)>", this->caracter);
    return str;
}

/*
* Function to add two Char objects
*/
static Object *Char_add(const Object *first, const Object *second)
{
    CharClass *result = new(Char, 0);

    result->caracter = ((CharClass *)first)->caracter + ((CharClass *)second)->caracter;
    return result;
}

/*
* Function to subtract two Char objects
*/
static Object *Char_sub(const Object *first, const Object *second)
{
    CharClass *result = new(Char, 0);

    result->caracter = ((CharClass *)first)->caracter - ((CharClass *)second)->caracter;
    return result;
}

/*
* Function to multiply two Char objects
*/
static Object *Char_mul(const Object *first, const Object *second)
{
    CharClass *result = new(Char, 0);

    result->caracter = ((CharClass *)first)->caracter * ((CharClass *)second)->caracter;
    return result;
}

/*
* Function to divide two Char objects
*/
static Object *Char_div(const Object *first, const Object *second)
{
    CharClass *result = new(Char, 0);

    result->caracter = ((CharClass *)first)->caracter / ((CharClass *)second)->caracter;
    return result;
}

/*
* Function to check if two Char objects are equal
*/
static bool Char_eq(const Object *first, const Object *second)
{
    return (((CharClass *)first)->caracter == ((CharClass *)second)->caracter);
}

/*
* Function to check if the first Char object is greater than the second
*/
static bool Char_gt(const Object *first, const Object *second)
{
    return (((CharClass *)first)->caracter > ((CharClass *)second)->caracter);
}

/*
* Function to check if the first Char object is less than the second
*/
static bool Char_lt(const Object *first, const Object *second)
{
    return (((CharClass *)first)->caracter < ((CharClass *)second)->caracter);
}

/*
* Define the Char class and its methods
*/
static const CharClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(CharClass),
        .__name__ = "Char",
        .__ctor__ = (ctor_t)&Char_ctor,
        .__dtor__ = (dtor_t)&Char_dtor,
        .__str__ = (to_string_t)&Char_str,
        .__add__ = (binary_operator_t)&Char_add,
        .__sub__ = (binary_operator_t)&Char_sub,
        .__mul__ = (binary_operator_t)&Char_mul,
        .__div__ = (binary_operator_t)&Char_div,
        .__eq__ = (binary_comparator_t)&Char_eq,
        .__gt__ = (binary_comparator_t)&Char_gt,
        .__lt__ = (binary_comparator_t)&Char_lt
    },
    .caracter = '\0'
};

const Class *Char = (const Class *)&_description;
