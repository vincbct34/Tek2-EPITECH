/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Borgs
*/

#pragma once

#include "Destination.hpp"
#include "WarpSystem.hpp"
#include "Federation.hpp"

namespace Federation {
    namespace Starfleet {
        class Captain;
        class Ensign;
        class Ship;
    }
    class Ship;
}

namespace Borg {
    class Ship {
        private:
            int _side;
            short _maxWarp;
            WarpSystem::Core *_core = nullptr;
            Destination _home;
            Destination _location;
            int _shield;
            int _weaponFrequency;
            short _repair;

        public:
            Ship(int weaponFrequency = 20, short repair = 3);
            ~Ship();

            void setupCore(WarpSystem::Core *core);
            void checkCore();

            bool move(int warp, Destination d);
            bool move(int warp);
            bool move(Destination d);
            bool move();

            int getShield();
            void setShield(int shield);
            int getWeaponFrequency();
            void setWeaponFrequency(int frequency);
            short getRepair();
            void setRepair(short repair);

            void fire(Federation::Starfleet::Ship *target);
            void fire(Federation::Ship *target);
            void repair();
    };
};
