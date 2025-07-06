/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercice 05
*/

#include <stdlib.h>
#include <stdarg.h>
#include "raise.h"
#include "array.h"
#include "new.h"

/*
* Define the Array class
*/
typedef struct
{
    Container   base;
    Class       *_type;
    size_t      _size;
    Object      **_tab;
}   ArrayClass;

/*
* Define the ArrayIterator class
*/
typedef struct {
    Iterator    base;
    ArrayClass  *_array;
    size_t      _idx;
}   ArrayIteratorClass;

/*
* Function to create a new Array object
*/
static void     ArrayIterator_ctor(ArrayIteratorClass *this, va_list *args)
{
    this->_array = va_arg(*args, ArrayClass *);
    this->_idx = va_arg(*args, int);
}

/*
* Function to check if two ArrayIterator objects are equal
*/
static bool     ArrayIterator_eq(ArrayIteratorClass *this, ArrayIteratorClass *other)
{
    return (this->_idx == other->_idx);
}

/*
* Function to check if one ArrayIterator object is greater than another
*/
static bool     ArrayIterator_gt(ArrayIteratorClass *this, ArrayIteratorClass *other)
{
    return (this->_idx > other->_idx);
}

/*
* Function to check if one ArrayIterator object is less than another
*/
static bool     ArrayIterator_lt(ArrayIteratorClass *this, ArrayIteratorClass *other)
{
    return (this->_idx < other->_idx);
}

/*
* Function to increment the index of an ArrayIterator object
*/
static void     ArrayIterator_incr(ArrayIteratorClass *this)
{
    this->_idx += 1;
}

/*
* Function to get the value of an ArrayIterator object
*/
static Object   *ArrayIterator_getval(ArrayIteratorClass *this)
{
    if (this->_idx >= this->_array->_size)
        raise("Out of range");
    return (this->_array->_tab[this->_idx]);
}

/*
* Function to set the value of an ArrayIterator object
*/
static void     ArrayIterator_setval(ArrayIteratorClass *this, ...)
{
    va_list args;
    va_start(args, this);
    if (this->_idx >= this->_array->_size)
        raise("Out of range");
    delete(this->_array->_tab[this->_idx]);
    this->_array->_tab[this->_idx] = va_new(this->_array->_type, &args);
    va_end(args);
}

/*
* Define the ArrayIterator class and its methods
*/
static const ArrayIteratorClass   ArrayIteratorDescr = {
    {   /* Iterator struct */
        {   /* Class struct */
            .__size__ = sizeof(ArrayIteratorClass),
            .__name__ = "ArrayIterator",
            .__ctor__ = (ctor_t)&ArrayIterator_ctor,
            .__dtor__ = NULL,
            .__str__ = NULL,
            .__add__ = NULL,
            .__sub__ = NULL,
            .__mul__ = NULL,
            .__div__ = NULL,
            .__eq__ = (binary_comparator_t)&ArrayIterator_eq,
            .__gt__ = (binary_comparator_t)&ArrayIterator_gt,
            .__lt__ = (binary_comparator_t)&ArrayIterator_lt,
        },
        .__incr__ = (incr_t)&ArrayIterator_incr,
        .__getval__ = (getval_t)&ArrayIterator_getval,
        .__setval__ = (setval_t)&ArrayIterator_setval,
    },
    ._array = NULL,
    ._idx = 0
};

static const Class    *ArrayIterator = (const Class *)&ArrayIteratorDescr;

/*
* Function to create a new Array object
*/
static void     Array_ctor(ArrayClass *this, va_list *args)
{
    void *params;

    this->_size = va_arg(*args, size_t);
    this->_type = va_arg(*args, Class *);
    this->_tab = malloc(sizeof(Object *) * this->_size);
    if (!this->_tab)
        raise("Out of memory");
    params = va_arg(*args, void *);
    for (unsigned int i = 0; i < this->_size; i++) {
        this->_tab[i] = new(this->_type, params);
    }
}

/*
* Function to delete an Array object
*/
static void     Array_dtor(ArrayClass *this)
{
    for (unsigned int i = 0; i < this->_size; i++)
        delete(this->_tab[i]);
    free(this->_tab);
}

/*
* Function to get the length of an Array object
*/
static size_t   Array_len(ArrayClass *this)
{
    return (this->_size);
}

/*
* Function to get the beginning of an Array object
*/
static Iterator *Array_begin(ArrayClass *this)
{
    return (new(ArrayIterator, this, 0));
}

/*
* Function to get the end of an Array object
*/
static Iterator *Array_end(ArrayClass *this)
{
    return (new(ArrayIterator, this, this->_size));
}

/*
* Function to get an item from an Array object
*/
static Object   *Array_getitem(ArrayClass *this, ...)
{
    va_list args;
    va_start(args, this);
    size_t idx = va_arg(args, size_t);
    va_end(args);
    if (idx >= this->_size)
        raise("Out of range");
    return (this->_tab[idx]);
}

/*
* Function to set an item in an Array object
*/
static void     Array_setitem(ArrayClass *this, ...)
{
    va_list args;
    va_start(args, this);
    size_t idx = va_arg(args, size_t);

    if (idx >= this->_size)
        raise("Out of range");
    delete(this->_tab[idx]);
    this->_tab[idx] = va_new(this->_type, &args);
    va_end(args);
}

/*
* Define the Array class and its methods
*/
static const ArrayClass   _descr = {
    {   /* Container struct */
        {   /* Class struct */
            .__size__ = sizeof(ArrayClass),
            .__name__ = "Array",
            .__ctor__ = (ctor_t)&Array_ctor,
            .__dtor__ = (dtor_t)&Array_dtor,
            .__str__ = NULL,
            .__add__ = NULL,
            .__sub__ = NULL,
            .__mul__ = NULL,
            .__div__ = NULL,
            .__eq__ = NULL,
            .__gt__ = NULL,
            .__lt__ = NULL,
        },
        .__len__ = (len_t)&Array_len,
        .__begin__ = (iter_t)&Array_begin,
        .__end__ = (iter_t)&Array_end,
        .__getitem__ = (getitem_t)&Array_getitem,
        .__setitem__ = (setitem_t)&Array_setitem,
    },
    ._type = NULL,
    ._size = 0,
    ._tab = NULL
};

const Class   *Array = (const Class *)&_descr;
