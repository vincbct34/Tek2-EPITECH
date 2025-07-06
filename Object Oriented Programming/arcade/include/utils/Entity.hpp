/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** entity
*/

#pragma once

#include <filesystem>
#include <string>
#include <tuple>

namespace Arcade {
    class IEntity {
        public:
            virtual ~IEntity() = default;

            virtual void setSize(std::pair<double, double> size) = 0;
            virtual void setPos(std::pair<double, double> pos) = 0;

            virtual std::pair<double, double> getSize() const = 0;
            virtual std::pair<double, double> getPos() const = 0;
    };

    class AEntity : public IEntity  {
        public:
            AEntity(std::pair<double, double> size, std::pair<double, double> pos, int id) : _size(size), _pos(pos), _id(id) {};

            void setSize(std::pair<double, double> size) override;
            void setPos(std::pair<double, double> pos) override;

            std::pair<double, double> getSize() const override;
            std::pair<double, double> getPos() const override;

            void setID(int id);
            int getID() const;
        private:
            std::pair<double, double> _size;
            std::pair<double, double> _pos;

            int _id = 0;
    };

    class AText : public AEntity {
        public:
            AText(std::pair<double, double> size, std::pair<double, double> pos, std::string str, bool isHighlighted, int id) : AEntity(size, pos, id), _str(str), _isHighlighted(isHighlighted) {};

            void setColor(std::tuple<int, int, int> color);
            void setHighlighted(bool isHighlighted);
            void setText(std::string str);

            std::string getText() const;

            bool isHighlighted() const;

            std::tuple<int, int, int> getColor() const;
        private:
            std::string _str;

            bool _isHighlighted = false;

            std::tuple<int, int, int> _color = {255, 0, 0};
    };

    class ARectangle : public AEntity {
        public:
            ARectangle(std::pair<double, double> size, std::pair<double, double> pos, std::tuple<int, int, int> color, int id = -1) : AEntity(size, pos, id), _color(color) {};
            
            std::tuple<int, int, int> getColor() const;

            void setColor(std::tuple<int, int, int> color) {
                _color = color;
            }
        private:
            std::tuple<int, int, int> _color;
    };
}
