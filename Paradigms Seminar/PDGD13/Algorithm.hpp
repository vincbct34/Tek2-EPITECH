/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Algorithm
*/

#pragma once

template <typename T>
void swap(T& a, T& b)
{
    T tmp = a;
    a = b;
    b = tmp;
}

template <typename T>
const T& min(const T& a, const T& b)
{
    return (a < b) ? a : b;
}

template <typename T>
const T& max(const T& a, const T& b)
{
    return (a < b) ? b : a;
}

template <typename T>
const T& clamp(const T& value, const T& minVal, const T& maxVal)
{
    return (value < minVal) ? minVal : (maxVal < value) ? maxVal : value;
}
