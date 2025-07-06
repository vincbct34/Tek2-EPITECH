/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sdlInput
*/

#include "Sdl.hpp"

namespace Arcade {

    Event SDL::getInput(const std::vector<std::unique_ptr<IEntity>> &elements) {
        Event event;
        SDL_Event sldEvent;

        while (SDL_PollEvent(&sldEvent)) {
            if (sldEvent.type == SDL_QUIT) {
                event.setType(Event::TypeEvent::ESCAPE_KEY);
                stop();
                return event;
            } else if (sldEvent.type == SDL_KEYDOWN) {
                switch (sldEvent.key.keysym.sym) {
                    case SDLK_UP: event.setType(Event::TypeEvent::UP_ARROW); break;
                    case SDLK_DOWN: event.setType(Event::TypeEvent::DOWN_ARROW); break;
                    case SDLK_LEFT: event.setType(Event::TypeEvent::LEFT_ARROW); break;
                    case SDLK_RIGHT: event.setType(Event::TypeEvent::RIGHT_ARROW); break;
                    case SDLK_RETURN: event.setType(Event::TypeEvent::ENTER_KEY); break;
                    case SDLK_ESCAPE: event.setType(Event::TypeEvent::ESCAPE_KEY); break;
                    case SDLK_SPACE: event.setType(Event::TypeEvent::SPACE_KEY); break;
                    case SDLK_TAB: event.setType(Event::TypeEvent::TAB_KEY); break;
                    case SDLK_LSHIFT: event.setType(Event::TypeEvent::SHIFT_KEY); break;
                    case SDLK_BACKSPACE: event.setType(Event::TypeEvent::BACKSPACE); break;
                    default: event.setType(Event::TypeEvent::UNDEFINED); break;
                }
            } else if (sldEvent.type == SDL_MOUSEBUTTONDOWN && sldEvent.button.button == SDL_BUTTON_LEFT) {
                event.setType(Event::TypeEvent::LMOUSE);

                for (const auto &elem : elements) {
                    auto textElem = dynamic_cast<AText *>(elem.get());
                    if (textElem) {
                        int mouseX, mouseY;
                        SDL_GetMouseState(&mouseX, &mouseY);
                        float posX = (textElem->getPos().first * 800) / 100;
                        float posY = (textElem->getPos().second * 600) / 100;

                        if (mouseX >= posX && mouseX <= posX + textElem->getSize().first &&
                            mouseY >= posY && mouseY <= posY + textElem->getSize().second) {
                            event.setClickedEntity(textElem);
                            break;
                        }
                    }
                }
            }
        }
        return event;
    }

}
