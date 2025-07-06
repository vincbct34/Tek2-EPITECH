/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Federation
*/

#pragma once

namespace WarpSystem {
    class QuantumReactor {
        private:
            bool _stability;
        
        public:
            QuantumReactor();
            ~QuantumReactor();

            bool isStable();
            void setStability(bool stability);
    };

    class Core {
        private:
            QuantumReactor *_coreReactor;

        public:
            Core(QuantumReactor *coreReactor);
            ~Core();

            QuantumReactor *checkReactor();
    };
};
