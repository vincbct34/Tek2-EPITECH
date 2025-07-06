/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Stack
*/

#pragma once

#include <stack>
#include <string>

class Stack {
    public:
        class Error : public std::exception {
            public:
                Error(const std::string& message);
                const char* what() const noexcept override;

            private:
                std::string _message;
        };

        void push(double value);
        
        double pop();

        double top() const;

        void add();
        void sub();
        void mul();
        void div();

    private:
        std::stack<double> _stack;
};
