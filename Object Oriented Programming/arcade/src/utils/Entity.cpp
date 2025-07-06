/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** entity
*/

#include "Entity.hpp"

namespace Arcade {

//////////
// AEntity
void AEntity::setPos(std::pair<double, double> pos)
{
    _pos = pos;
}

std::pair<double, double> AEntity::getPos() const
{
    return _pos;
}

void AEntity::setSize(std::pair<double, double> size)
{
    _size = size;
}

std::pair<double, double> AEntity::getSize() const
{
    return _size;
}

void AEntity::setID(int id)
{
    _id = id;
}

int AEntity::getID() const
{
    return _id;
}

////////
// AText
void AText::setText(std::string str)
{
    _str = str;
}

std::string AText::getText() const
{
    return _str;
}

void AText::setHighlighted(bool isHighlighted)
{
    _isHighlighted = isHighlighted;
}

bool AText::isHighlighted() const
{
    return _isHighlighted;
}

void AText::setColor(std::tuple<int, int, int> color)
{
    _color = color;
}

std::tuple<int, int, int> AText::getColor() const
{
    return _color;
}

////////
// ARectangle
std::tuple<int, int, int> ARectangle::getColor() const
{
    return _color;
}

} // namespace Arcade

/*
void Entity::setID(int id)
{
    ID = id;
}

int Entity::getID() const
{
    return ID;
}

void Entity::setPath(std::filesystem::path path)
{
    _spritePath = path;
}

std::filesystem::path Entity::getPath() const
{
    return _spritePath;
}

void Entity::setSize(std::pair<double, double> size)
{
    _size = size;
}

std::pair<double, double> Entity::getSize() const
{
    return _size;
}

void Entity::setPos(std::pair<double, double> pos)
{
    _pos = pos;
}

std::pair<double, double> Entity::getPos() const
{
    return _pos;
}

void Entity::setOriginePos(std::pair<double, double> pos)
{
    _originePos = pos;
}

std::pair<double, double> Entity::getOriginePos() const
{
    return _originePos;
}

void Entity::setColor(std::tuple<int, int, int> color)
{
    _color = color;
}

std::tuple<int, int, int> Entity::getColor() const
{
    return _color;
}

std::string Entity::getText() const
{
    return _str;
}

void Entity::setText(std::string str)
{
    _str = str;
}

void Entity::setType(TypeDisplay type)
{
    _type = type;
}

Entity::TypeDisplay Entity::getType() const
{
    return _type;
}
*/