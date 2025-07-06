/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Point
*/

#pragma once

namespace Math {
    class Color {
        public:
            Color(float r, float g, float b) : r(r), g(g), b(b) {}
            ~Color() = default;

            float getR() const { return r; }
            float getG() const { return g; }
            float getB() const { return b; }

            Color operator+(const Color &other) const { return Color(r + other.r, g + other.g, b + other.b); }
            Color operator+=(const Color &other) { r += other.r; g += other.g; b += other.b; return *this; }
        private:
            float r;
            float g;
            float b;
    };
}