/*
** EPITECH PROJECT, 2025
** Rush2
** File description:
** new
*/

#include "new.h"
#include "player.h"

/*
* Create a new object of the given class
*/
Object *new(const Class *class, ...)
{
    Object *object = malloc(class->__size__);
    va_list args;

    if (object == NULL)
        raise("Out of memory");

    va_start(args, class);
    memcpy(object, class, class->__size__);

    if (class->__ctor__ != NULL)
        class->__ctor__(object, &args);

    va_end(args);
    return object;
}

/*
* Delete an object
*/
void delete(Object *ptr)
{
    if (ptr == NULL)
        return;

    Class *class_info = (Class *)ptr;

    if (class_info->__dtor__ != NULL)
        class_info->__dtor__(ptr);

    free(ptr);
}

/*
* Create a new object of the given class with a va_list already initialized
*/
Object  *va_new(const Class *class, va_list* ap)
{
    Object *object = malloc(class->__size__);

    if (object == NULL)
        raise("Out of memory");

    memcpy(object, class, class->__size__);

    if (class->__ctor__ != NULL)
        class->__ctor__(object, ap);

    return object;
}
