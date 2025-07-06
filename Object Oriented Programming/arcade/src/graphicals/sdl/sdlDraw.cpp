/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sdlDraw
*/

#include "Sdl.hpp"

namespace Arcade {

    void SDL::drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) {
        for (const auto &elem : elements) {
            if (auto *text = dynamic_cast<AText *>(elem.get())) drawTextElement(text);
            else if (auto *rect = dynamic_cast<ARectangle *>(elem.get())) drawRectElement(rect);
        }
    }

    void SDL::drawTextElement(const AText *textElem) {
        float posX = (textElem->getPos().first * 800) / 100;
        float posY = (textElem->getPos().second * 600) / 100;

        TTF_SetFontStyle(_font, textElem->isHighlighted() ? TTF_STYLE_BOLD : TTF_STYLE_NORMAL);
        auto [r, g, b] = textElem->getColor();
        SDL_Color color = {static_cast<Uint8>(r), static_cast<Uint8>(g), static_cast<Uint8>(b), 255};

        SDL_Surface *surface = TTF_RenderText_Blended(_font, textElem->getText().c_str(), color);
        if (!surface) return;

        SDL_Texture *texture = SDL_CreateTextureFromSurface(_renderer, surface);
        SDL_FreeSurface(surface);
        if (!texture) return;

        SDL_Rect rect;
        rect.x = static_cast<int>(posX);
        rect.y = static_cast<int>(posY);
        SDL_QueryTexture(texture, nullptr, nullptr, &rect.w, &rect.h);
        rect.w /= 2; // Divide width by 2
        rect.h /= 2; // Divide height by 2
        rect.x -= rect.w / 2;
        rect.y -= rect.h / 2;

        SDL_RenderCopy(_renderer, texture, nullptr, &rect);
        SDL_DestroyTexture(texture);
    }

    void SDL::drawRectElement(const ARectangle *rectElem) {
        float posX = (rectElem->getPos().first * 800) / 100;
        float posY = (rectElem->getPos().second * 600) / 100;
        float sizeX = (rectElem->getSize().first * 800) / 100 / 2; // Divide width by 2
        float sizeY = (rectElem->getSize().second * 600) / 100 / 2; // Divide height by 2

        auto [r, g, b] = rectElem->getColor();
        SDL_SetRenderDrawColor(_renderer, static_cast<Uint8>(r), static_cast<Uint8>(g), static_cast<Uint8>(b), 255);

        SDL_Rect rect = { static_cast<int>(posX), static_cast<int>(posY), static_cast<int>(sizeX), static_cast<int>(sizeY) };
        SDL_RenderFillRect(_renderer, &rect);
    }

}
