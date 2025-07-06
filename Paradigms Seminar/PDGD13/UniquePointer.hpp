/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - UniquePointer
*/

#pragma once

template <typename T>
class UniquePointer {
    public:
        UniquePointer(T *ptr = nullptr) : _ptr(ptr) {}
        UniquePointer(const UniquePointer &other) = delete;
        ~UniquePointer() { if (_ptr) {delete _ptr;} }

        void reset() { if (_ptr) {delete _ptr; _ptr = nullptr;} }
        T *get() const { return _ptr; }

        T *operator->() const { return _ptr; }

        UniquePointer &operator=(UniquePointer &&other) noexcept {
        if (this != &other) {
                delete _ptr;
                _ptr = other._ptr;
                other._ptr = nullptr;
            }
            return *this;
        }
        UniquePointer &operator=(const UniquePointer &other) = delete;

    private:
        T *_ptr;
};
