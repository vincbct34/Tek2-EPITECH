/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - Array
*/

#pragma once

#include <cstddef>
#include <iostream>
#include <functional>

template <typename T, size_t N>
class Array {
    private:
        T _array[N];

    public:
        Array() : _array() {}
        ~Array() = default;

        T &operator[](int index)
        {
            if (static_cast<size_t>(index) < N)
                return this->_array[index];
            throw std::range_error("Out of range");
            return _array[0];
        }

        T operator[](int index) const
        {
            if (static_cast<size_t>(index) < N)
                return this->_array[index];
            throw std::range_error("Out of range");
            return _array[0];
        }

        std::size_t size() const
        {
            return N;
        }

        void forEach(const std::function<void(const T&)> &task) const
        {
            for (size_t i = 0; i < N; i++)
                task(this->_array[i]);
        }

        template <typename U>
        Array<U, N> convert(const std::function<U(const T&)> &converter) const
        {
            Array<U, N> newArray;

            for (size_t i = 0; i < N; i++)
                newArray[i] = converter(this->_array[i]);
            return newArray;
        }
};

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& os, const Array<T, N>& array)
{
    os << "[";
    for (size_t i = 0; i < N - 1; i++)
        os << array[i] << ", ";
    os << array[N - 1];
    os << "]";
    return os;
}
