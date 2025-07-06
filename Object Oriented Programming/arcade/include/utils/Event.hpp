/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** event
*/

#pragma once

#include "IGraphical.hpp"
#include "Entity.hpp"

#include <filesystem>
#include <string>
#include <tuple>

namespace Arcade {
    class Event {
        public:
            enum class TypeEvent {
                UNDEFINED,
                LMOUSE,
                RMOUSE,
                UP_ARROW,
                DOWN_ARROW,
                LEFT_ARROW,
                RIGHT_ARROW,
                A_KEY,
                B_KEY,
                C_KEY,
                D_KEY,
                E_KEY,
                F_KEY,
                G_KEY,
                H_KEY,
                I_KEY,
                J_KEY,
                K_KEY,
                L_KEY,
                M_KEY,
                N_KEY,
                O_KEY,
                P_KEY,
                Q_KEY,
                R_KEY,
                S_KEY,
                T_KEY,
                U_KEY,
                V_KEY,
                W_KEY,
                X_KEY,
                Y_KEY,
                Z_KEY,
                ENTER_KEY,
                ESCAPE_KEY,
                SPACE_KEY,
                TAB_KEY,
                CHARACTER,
                BACKSPACE,
                SHIFT_KEY
            };

            Event();
            ~Event() = default;

            void setMousePos(std::pair<double, double> pos);
            void setClickedEntity(IEntity *entity);
            void setType(TypeEvent type);
            void setKey(int key);
            void setChar(char c);

            char getChar() const;

            int getKey() const;

            TypeEvent getType() const;

            std::pair<double, double> getMousePos() const;

            IEntity *getClickedEntity() const;
        private:
            TypeEvent _type;

            IEntity *_clickedEntity = nullptr;

            std::pair<double, double> _mousePos;

            int _key = 0;

            char _char = '\0';
    };
}
