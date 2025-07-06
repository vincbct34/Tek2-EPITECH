/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - The Priest
*/

#include "../Priest.hpp"

#include <criterion/criterion.h>

Test(Priest, test_rest) {
    Priest priest("John", 100);
    priest.rest();
    cr_assert(priest.getPower() == 200, "Rest should increase power.");
}

Test(Priest, test_rest_no_hp) {
    Priest priest("John", 100);
    priest.damage(100);
    priest.rest();
    cr_assert(priest.getPower() == 100, "Rest should not increase power if out of combat.");
}
