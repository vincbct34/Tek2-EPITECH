/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 02
*/

#include <stdio.h>
#include "vertex.h"
#include "new.h"

/*
* Define the Vertex class
*/
typedef struct
{
    Class   base;
    int     x, y, z;
}   VertexClass;

/*
* Function to create a new Vertex object
*/
static void Vertex_ctor(VertexClass *this, va_list *args)
{
    this->x = va_arg(*args, int);
    this->y = va_arg(*args, int);
    this->z = va_arg(*args, int);
}

/*
* Function to delete a Vertex object
*/
static void Vertex_dtor(VertexClass *this)
{
    (void)this;
}

/*
* Function to get a string representation of a Vertex object
*/
static char *Vertex_str(VertexClass *this)
{
    char *str;

    asprintf(&str, "<Vertex (%d, %d, %d)>", this->x, this->y, this->z);
    return str;
}

/*
* Function to add two Vertex objects
*/
static Object *Vertex_add(const Object *first, const Object *second)
{
    VertexClass *result = new(Vertex, 0, 0, 0);

    result->x = ((VertexClass *)first)->x + ((VertexClass *)second)->x;
    result->y = ((VertexClass *)first)->y + ((VertexClass *)second)->y;
    result->z = ((VertexClass *)first)->z + ((VertexClass *)second)->z;
    return result;
}

/*
* Function to subtract two Vertex objects
*/
static Object *Vertex_sub(const Object *first, const Object *second)
{
    VertexClass *result = new(Vertex, 0, 0, 0);

    result->x = ((VertexClass *)first)->x - ((VertexClass *)second)->x;
    result->y = ((VertexClass *)first)->y - ((VertexClass *)second)->y;
    result->z = ((VertexClass *)first)->z - ((VertexClass *)second)->z;
    return result;
}

/*
* Define the Vertex class and its methods
*/
static const VertexClass _description = {
    {   /* Class struct */
        .__size__ = sizeof(VertexClass),
        .__name__ = "Vertex",
        .__ctor__ = (ctor_t)&Vertex_ctor,
        .__dtor__ = (dtor_t)&Vertex_dtor,
        .__str__ = (to_string_t)&Vertex_str,
        .__add__ = (binary_operator_t)&Vertex_add,
        .__sub__ = (binary_operator_t)&Vertex_sub,
        .__mul__ = NULL,
        .__div__ = NULL,
        .__eq__ = NULL,
        .__gt__ = NULL,
        .__lt__ = NULL
    },
    .x = 0,
    .y = 0,
    .z = 0
};

const Class   *Vertex = (const Class *)&_description;