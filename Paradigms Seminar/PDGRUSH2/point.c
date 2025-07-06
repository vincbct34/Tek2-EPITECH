/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 02
*/

#include <stdio.h>
#include "point.h"
#include "new.h"

/*
* Define the Point class
*/
typedef struct
{
    Class   base;
    int     x, y;
}   PointClass;

/*
* Function to create a new Point object
*/
static void Point_ctor(PointClass *this, va_list *args)
{
    this->x = va_arg(*args, int);
    this->y = va_arg(*args, int);
}

/*
* Function to delete a Point object
*/
static void Point_dtor(PointClass *this)
{
    (void)this;
}

/*
* Function to get a string representation of a Point object
*/
static char *Point_str(PointClass *this)
{
    char *str;

    asprintf(&str, "<Point (%d, %d)>", this->x, this->y);
    return str;
}

/*
* Function to add two Point objects
*/
static Object *Point_add(const Object *first, const Object *second)
{
    PointClass *result = new(Point, 0, 0);

    result->x = ((PointClass *)first)->x + ((PointClass *)second)->x;
    result->y = ((PointClass *)first)->y + ((PointClass *)second)->y;
    return result;
}

/*
* Function to subtract two Point objects
*/
static Object *Point_sub(const Object *first, const Object *second)
{
    PointClass *result = new(Point, 0, 0);

    result->x = ((PointClass *)first)->x - ((PointClass *)second)->x;
    result->y = ((PointClass *)first)->y - ((PointClass *)second)->y;
    return result;
}

/*
* Define the Point class and its methods
*/
static const PointClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(PointClass),
        .__name__ = "Point",
        .__ctor__ = (ctor_t)&Point_ctor,
        .__dtor__ = (dtor_t)&Point_dtor,
        .__str__ = (to_string_t)&Point_str,
        .__add__ = (binary_operator_t)&Point_add,
        .__sub__ = (binary_operator_t)&Point_sub,
        .__mul__ = NULL,
        .__div__ = NULL,
        .__eq__ = NULL,
        .__gt__ = NULL,
        .__lt__ = NULL
    },
    .x = 0,
    .y = 0
};

const Class   *Point = (const Class *)&_description;
