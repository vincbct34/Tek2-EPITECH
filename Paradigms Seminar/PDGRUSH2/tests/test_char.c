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
    char    caracter;
}   CharClass;

Test(CharClass, test_char_ctor)
{
    Object *c = new(Char, 'a');

    cr_assert_str_eq(str(c), "<Char (a)>");
    delete(c);
}

Test(CharClass, test_char_add)
{
    Object *c1 = new(Char, 'a');
    Object *c2 = new(Char, 1);
    Object *result = addition(c1, c2);

    cr_assert_str_eq(str(result), "<Char (b)>");
    delete(c1);
    delete(c2);
    delete(result);
}

Test(CharClass, test_char_sub)
{
    Object *c1 = new(Char, 'b');
    Object *c2 = new(Char, 1);
    Object *result = subtraction(c1, c2);

    cr_assert_str_eq(str(result), "<Char (a)>");
    delete(c1);
    delete(c2);
    delete(result);
}

Test(CharClass, test_char_mul)
{
    Object *c1 = new(Char, 1);
    Object *c2 = new(Char, 38);
    Object *result = multiplication(c1, c2);

    cr_assert_str_eq(str(result), "<Char (&)>");
    delete(c1);
    delete(c2);
    delete(result);
}

Test(CharClass, test_char_div)
{
    Object *c1 = new(Char, 126);
    Object *c2 = new(Char, 2);
    Object *result = division(c1, c2);

    cr_assert_str_eq(str(result), "<Char (?)>");
    delete(c1);
    delete(c2);
    delete(result);
}

Test(CharClass, test_char_eq)
{
    Object *c1 = new(Char, 'a');
    Object *c2 = new(Char, 'a');

    cr_assert(eq(c1, c2));
    delete(c1);
    delete(c2);
}

Test(CharClass, test_char_gt)
{
    Object *c1 = new(Char, 'b');
    Object *c2 = new(Char, 'a');

    cr_assert(gt(c1, c2));
    delete(c1);
    delete(c2);
}

Test(CharClass, test_char_lt)
{
    Object *c1 = new(Char, 'a');
    Object *c2 = new(Char, 'b');

    cr_assert(lt(c1, c2));
    delete(c1);
    delete(c2);
}


