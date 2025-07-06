/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Stack
*/

#include "Stack.hpp"

Stack::Error::Error(const std::string& message)
    : _message(message)
{
}

const char* Stack::Error::what() const noexcept
{
    return _message.c_str();
}

void Stack::push(double value)
{
    _stack.push(value);
}

double Stack::pop()
{
    if (_stack.empty())
        throw Error("Empty stack");
    double value = _stack.top();
    _stack.pop();
    return value;
}

double Stack::top() const
{
    if (_stack.empty())
        throw Error("Empty stack");
    return _stack.top();
}

void Stack::add()
{
    if (_stack.size() < 2)
        throw Error("Not enough operands");
    double a = pop();
    double b = pop();
    push(a + b);
}

void Stack::sub()
{
    if (_stack.size() < 2)
        throw Error("Not enough operands");
    double a = pop();
    double b = pop();
    push(a - b);
}

void Stack::mul()
{
    if (_stack.size() < 2)
        throw Error("Not enough operands");
    double a = pop();
    double b = pop();
    push(a * b);
}

void Stack::div()
{
    if (_stack.size() < 2)
        throw Error("Not enough operands");
    double a = pop();
    double b = pop();
    if (b == 0)
        throw Error("Division by zero");
    push(a / b);
}
