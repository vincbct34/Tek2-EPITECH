/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** event
*/

#include "Event.hpp"

namespace Arcade {

Event::Event() : _type(TypeEvent::UNDEFINED), _mousePos({0, 0}) {}

void Event::setType(TypeEvent type)
{
    _type = type;
}

Event::TypeEvent Event::getType() const
{
    return _type;
}

void Event::setMousePos(std::pair<double, double> pos)
{
    _mousePos = pos;
}

std::pair<double, double> Event::getMousePos() const
{
    return _mousePos;
}

void Event::setKey(int key)
{
    _key = key;
}

int Event::getKey() const
{
    return _key;
}

void Event::setClickedEntity(IEntity *entity)
{
    _clickedEntity = entity;

} 

IEntity *Event::getClickedEntity() const
{
    return _clickedEntity;

}

void Event::setChar(char c)
{
    _char = c;
}

char Event::getChar() const
{
    return _char;
}

} // namespace Arcade
