/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Peasant
*/

#include "../Peasant.hpp"

#include <criterion/criterion.h>

Test(Peasant, getName) {
    Peasant p("John", 10);
    cr_assert_str_eq(p.getName().c_str(), "John");
}

Test(Peasant, getPower) {
    Peasant p("John", 10);
    cr_assert_eq(p.getPower(), 10);
}

Test(Peasant, getHp) {
    Peasant p("John", 10);
    cr_assert_eq(p.getHp(), 100);
}

Test(Peasant, attack) {
    Peasant p("John", 10);
    int damage = p.attack();
    cr_assert(damage > 0);
}

Test(Peasant, attack_no_hp) {
    Peasant p("John", 10);
    p.damage(100);
    int damage = p.attack();
    cr_assert_eq(damage, 0);
}

Test(Peasant, attack_no_power) {
    Peasant p("John", 5);
    int damage = p.attack();
    cr_assert_eq(damage, 0);
}

Test(Peasant, special) {
    Peasant p("John", 10);
    int specialDamage = p.special();
    cr_assert(specialDamage == 0);
}

Test(Peasant, special_no_power) {
    Peasant p("John", 0);
    int specialDamage = p.special();
    cr_assert_eq(specialDamage, 0);
}

Test(Peasant, special_no_hp) {
    Peasant p("John", 10);
    p.damage(100);
    int specialDamage = p.special();
    cr_assert_eq(specialDamage, 0);
}

Test(Peasant, rest) {
    Peasant p("John", 10);
    p.damage(50);
    p.rest();
    cr_assert(p.getHp() >= 50);
}

Test(Peasant, rest_no_hp) {
    Peasant p("John", 10);
    p.damage(100);
    p.rest();
    cr_assert_eq(p.getHp(), 0);
}

Test(Peasant, damage) {
    Peasant p("John", 10);
    p.damage(20);
    cr_assert_eq(p.getHp(), 80);
}
