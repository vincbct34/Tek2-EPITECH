/*
** EPITECH PROJECT, 2025
** Test
** File description:
** base
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "../new.h"
#include "../player.h"
#include "../point.h"
#include "../vertex.h"
#include "../char.h"
#include "../int.h"
#include "../float.h"
#include "../array.h"
#include "../raise.h"
#include "../iterator.h"

typedef struct
{
    Class   base;
    int nb;
}   IntClass;

Test(array, test_array_creation)
{
    Object *array = new(Array, 5, Int, 0);

    cr_assert_not_null(array);
    cr_assert_eq(len(array), 5);

    delete(array);
}

Test(array, test_array_getitem)
{
    Object *array = new(Array, 5, Int, 42);

    for (size_t i = 0; i < 5; i++) {
        Object *item = getitem(array, i);
        cr_assert_not_null(item);
        cr_assert_eq(((IntClass *)item)->nb, 42);
    }

    delete(array);
}

Test(array, test_array_setitem)
{
    Object *array = new(Array, 5, Int, 0);

    for (size_t i = 0; i < 5; i++) {
        setitem(array, i, 42);
        Object *item = getitem(array, i);
        cr_assert_not_null(item);
        cr_assert_eq(((IntClass *)item)->nb, 42);
    }

    delete(array);
}

Test(array, test_array_iterator)
{
    Object *array = new(Array, 5, Int, 42);
    Iterator *it = begin(array);
    Iterator *end_it = end(array);

    for (size_t i = 0; i < 5; i++) {
        Object *item = getval(it);
        cr_assert_not_null(item);
        cr_assert_eq(((IntClass *)item)->nb, 42);
        incr(it);
    }

    delete(it);
    delete(end_it);
    delete(array);
}

Test(array, test_array_iterator_setval)
{
    Object *array = new(Array, 5, Int, 0);
    Iterator *it = begin(array);

    for (size_t i = 0; i < 5; i++) {
        setval(it, 42);
        Object *item = getval(it);
        cr_assert_not_null(item);
        cr_assert_eq(((IntClass *)item)->nb, 42);
        incr(it);
    }

    delete(it);
    delete(array);
}

Test(array, test_array_iterator_eq)
{
    Object *array = new(Array, 5, Int, 42);
    Iterator *it1 = begin(array);
    Iterator *it2 = begin(array);

    cr_assert(eq(it1, it2));

    incr(it1);
    cr_assert_not(eq(it1, it2));

    incr(it2);
    cr_assert(eq(it1, it2));

    delete(it1);
    delete(it2);
    delete(array);
}

Test(array, test_array_iterator_gt)
{
    Object *array = new(Array, 5, Int, 42);
    Iterator *it1 = begin(array);
    Iterator *it2 = begin(array);

    incr(it1);
    cr_assert(gt(it1, it2));

    incr(it2);
    cr_assert_not(gt(it1, it2));

    delete(it1);
    delete(it2);
    delete(array);
}

Test(array, test_array_iterator_lt)
{
    Object *array = new(Array, 5, Int, 42);
    Iterator *it1 = begin(array);
    Iterator *it2 = begin(array);

    incr(it2);
    cr_assert(lt(it1, it2));

    incr(it1);
    cr_assert_not(lt(it1, it2));

    delete(it1);
    delete(it2);
    delete(array);
}
