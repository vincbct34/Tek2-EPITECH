/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** ncursesInput
*/

#include "Ncurses.hpp"

namespace Arcade {

    Event Ncurse::getInput(const std::vector<std::unique_ptr<IEntity>> &elements) {
        (void)elements;
        Event event;
        int key = wgetch(_win);
    
        switch (key) {
            case 259: event.setType(Event::TypeEvent::UP_ARROW); break;
            case 258: event.setType(Event::TypeEvent::DOWN_ARROW); break;
            case 260: event.setType(Event::TypeEvent::LEFT_ARROW); break;
            case 261: event.setType(Event::TypeEvent::RIGHT_ARROW); break;
            case 10:  event.setType(Event::TypeEvent::ENTER_KEY); break;
            case 27:  event.setType(Event::TypeEvent::ESCAPE_KEY); break;
            case 32:  event.setType(Event::TypeEvent::SPACE_KEY); break;
            case 127: event.setType(Event::TypeEvent::BACKSPACE); break;
            case 9:   event.setType(Event::TypeEvent::TAB_KEY); break;
            case 338: event.setType(Event::TypeEvent::SHIFT_KEY); break;
            
            default:
                if (key >= 33 && key <= 126) {
                    event.setType(Event::TypeEvent::CHARACTER);
                    event.setChar(static_cast<char>(key));
                } else {
                    event.setType(Event::TypeEvent::UNDEFINED);
                }
                break;
        }
        event.setKey(key);
        return event;
    }

}
