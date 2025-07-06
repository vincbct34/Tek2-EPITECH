/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - The Enchanter
*/


#include "../Enchanter.hpp"

#include <criterion/criterion.h>

Test(Enchanter, test_attack) {
    Enchanter enchanter("Arthur", 100);
    int damage = enchanter.attack();
    cr_assert(damage == 0, "Attack should deal no damage.");
}

Test(Enchanter, test_attack_no_power) {
    Enchanter enchanter("Arthur", 5);
    int damage = enchanter.attack();
    cr_assert_eq(damage, 0, "Attack should deal no damage if out of power.");
}

Test(Enchanter, test_attack_no_hp) {
    Enchanter enchanter("Arthur", 100);
    enchanter.damage(100);
    int damage = enchanter.attack();
    cr_assert_eq(damage, 0, "Attack should deal no damage if out of combat.");
}

Test(Enchanter, test_special) {
    Enchanter enchanter("Lancelot", 100);
    int special_damage = enchanter.special();
    cr_assert(special_damage > 0, "Special attack should deal positive damage.");
}

Test(Enchanter, test_special_no_power) {
    Enchanter enchanter("Lancelot", 5);
    int special_damage = enchanter.special();
    cr_assert_eq(special_damage, 0, "Special attack should deal no damage if out of power.");
}

Test(Enchanter, test_special_no_hp) {
    Enchanter enchanter("Lancelot", 100);
    enchanter.damage(100);
    int special_damage = enchanter.special();
    cr_assert_eq(special_damage, 0, "Special attack should deal no damage if out of combat.");
}

Test(Enchanter, test_rest) {
    Enchanter enchanter("Gawain", 100);
    enchanter.rest();
    cr_assert(enchanter.getPower() == 100, "Rest should increase power.");
}

Test(Enchanter, test_rest_no_hp) {
    Enchanter enchanter("Gawain", 100);
    enchanter.damage(100);
    enchanter.rest();
    cr_assert(enchanter.getPower() == 100, "Rest should not increase power if out of combat.");
}
