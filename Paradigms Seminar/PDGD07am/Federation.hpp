/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Federation
*/

#pragma once

#include "Destination.hpp"
#include "WarpSystem.hpp"
#include "Borg.hpp"

#include <string>

namespace Borg {
    class Ship;
}

namespace Federation {
    namespace Starfleet {
        class Captain {
            private:
                std::string _name;
                int _age;

            public:
                Captain(std::string name);
                ~Captain();

                std::string getName();
                int getAge();
                void setAge(int age);
        };

        class Ensign {
            private:
                std::string _name;

            public:
                Ensign(std::string name);
                ~Ensign();
        };

        class Ship {
            private:
                int _length;
                int _width;
                std::string _name;
                short _maxWarp;
                WarpSystem::Core *_core = nullptr;
                Captain *_captain = nullptr;
                Destination _home;
                Destination _location;
                int _shield;
                int _photonTorpedo;

            public:
                Ship(int length = 289, int width = 132, std::string name = "Entreprise", short maxWarp = 6, int photonTorpedo = 0);
                ~Ship();

                void setupCore(WarpSystem::Core *core);
                void checkCore();
                void promote(Captain *captain);

                bool move(int warp, Destination d);
                bool move(int warp);
                bool move(Destination d);
                bool move();

                int getShield();
                void setShield(int shield);
                int getTorpedo();
                void setTorpedo(int torpedo);

                void fire(Borg::Ship *target);
                void fire(int torpedoes, Borg::Ship *target);
        };
    }

    class Ship {
        private:
            int _length;
            int _width;
            std::string _name;
            WarpSystem::Core *_core = nullptr;
            short _maxWarp = 1;
            Destination _home;
            Destination _location;

        public:
            Ship(int length, int width, std::string name);
            ~Ship();

            void setupCore(WarpSystem::Core *core);
            void checkCore();

            bool move(int warp, Destination d);
            bool move(int warp);
            bool move(Destination d);
            bool move();

            WarpSystem::Core *getCore() { return _core; }
    };
};
