/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Knight
*/

#include "../Knight.hpp"

#include <criterion/criterion.h>

Test(Knight, test_attack) {
    Knight knight("Arthur", 100);
    int damage = knight.attack();
    cr_assert(damage > 0, "Attack should deal positive damage.");
}

Test(Knight, test_attack_no_power) {
    Knight knight("Arthur", 5);
    int damage = knight.attack();
    cr_assert_eq(damage, 0, "Attack should deal no damage if out of power.");
}

Test(Knight, test_attack_no_hp) {
    Knight knight("Arthur", 100);
    knight.damage(100);
    int damage = knight.attack();
    cr_assert_eq(damage, 0, "Attack should deal no damage if out of combat.");
}

Test(Knight, test_special) {
    Knight knight("Lancelot", 100);
    int special_damage = knight.special();
    cr_assert(special_damage > 0, "Special attack should deal positive damage.");
}

Test(Knight, test_special_no_power) {
    Knight knight("Lancelot", 5);
    int special_damage = knight.special();
    cr_assert_eq(special_damage, 0, "Special attack should deal no damage if out of power.");
}

Test(Knight, test_special_no_hp) {
    Knight knight("Lancelot", 100);
    knight.damage(100);
    int special_damage = knight.special();
    cr_assert_eq(special_damage, 0, "Special attack should deal no damage if out of combat.");
}

Test(Knight, test_rest) {
    Knight knight("Gawain", 100);
    knight.rest();
    cr_assert(knight.getPower() == 100, "Rest should increase power.");
}

Test(Knight, test_rest_no_hp) {
    Knight knight("Gawain", 100);
    knight.damage(100);
    knight.rest();
    cr_assert(knight.getPower() == 100, "Rest should not increase power if out of combat.");
}
