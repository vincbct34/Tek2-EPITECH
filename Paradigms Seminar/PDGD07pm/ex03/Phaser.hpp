/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - Lock'n load, baby
*/

#pragma once

class Phaser
{
    public:
        enum AmmoType {
            REGULAR,
            PLASMA,
            ROCKET
        };

        Phaser(int maxAmmo = 20, AmmoType type = REGULAR);
        ~Phaser();

        void fire();
        void ejectClip();
        void changeType(AmmoType newType);
        void reload();
        void addAmmo(AmmoType type);

        int getCurrentAmmos() const;
    
    private:
        static const int Empty = 0;
        int m_maxAmmo;
        int m_currentAmmo[3];
        AmmoType m_type;
};
