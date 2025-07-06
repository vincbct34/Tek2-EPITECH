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
    float nb;
}   FloatClass;

Test(FloatClass, creation)
{
    Object *f = new(Float, 42.42);

    cr_assert_not_null(f);
    cr_assert_str_eq(str(f), "<Float (42.419998)>"); // Le C arroundi il faut donc ajuster
    delete(f);
}

Test(FloatClass, addition)
{
    Object *f1 = new(Float, 21.21);
    Object *f2 = new(Float, 21.21);
    Object *result = addition(f1, f2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Float (42.419998)>");  // Le C arroundi il faut donc ajuster
    delete(f1);
    delete(f2);
    delete(result);
}

Test(FloatClass, subtraction)
{
    Object *f1 = new(Float, 42.42);
    Object *f2 = new(Float, 21.21);
    Object *result = subtraction(f1, f2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Float (21.209999)>"); // Le C arroundi il faut donc ajuster
    delete(f1);
    delete(f2);
    delete(result);
}

Test(FloatClass, multiplication)
{
    Object *f1 = new(Float, 2.0);
    Object *f2 = new(Float, 21.21);
    Object *result = multiplication(f1, f2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Float (42.419998)>"); // Le C arroundi il faut donc ajuster
    delete(f1);
    delete(f2);
    delete(result);
}

Test(FloatClass, division)
{
    Object *f1 = new(Float, 42.42);
    Object *f2 = new(Float, 2.0);
    Object *result = division(f1, f2);

    cr_assert_not_null(result);
    cr_assert_str_eq(str(result), "<Float (21.209999)>"); // Le C arroundi il faut donc ajuster
    delete(f1);
    delete(f2);
    delete(result);
}

Test(FloatClass, equality)
{
    Object *f1 = new(Float, 42.42);
    Object *f2 = new(Float, 42.42);

    cr_assert(eq(f1, f2));
    delete(f1);
    delete(f2);
}

Test(FloatClass, greater_than)
{
    Object *f1 = new(Float, 42.42);
    Object *f2 = new(Float, 21.21);

    cr_assert(gt(f1, f2));
    delete(f1);
    delete(f2);
}

Test(FloatClass, less_than)
{
    Object *f1 = new(Float, 21.21);
    Object *f2 = new(Float, 42.42);

    cr_assert(lt(f1, f2));
    delete(f1);
    delete(f2);
}
