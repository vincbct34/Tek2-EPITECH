/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** sdl
*/

// Sdl.hpp
#pragma once

#include "IGraphical.hpp"
#include "Entity.hpp"
#include "Event.hpp"

#include <SDL2/SDL_mixer.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL.h>

namespace Arcade {

    class SDL : public IGraphical {
        public:
            SDL() = default;
            ~SDL() = default;

            void init() override;
            void clearScreen() override;
            void drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void refresh() override;
            Event getInput(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void stop() override;
            std::pair<int, int> getWindowSize() const override;
            bool canUpdate() override;
            std::string getPlayerName() override;

        private:
            SDL_Window *_window = nullptr;
            SDL_Renderer* _renderer = nullptr;
            TTF_Font *_font = nullptr;
            SDL_Texture *_textTexture = nullptr;
            SDL_Rect _textRect = {0, 0, 0, 0};
            SDL_Event _event;
            Uint32 _lastUpdate = 0;
            Uint32 _updateInterval = 12;

            void drawTextElement(const AText *textElem);
            void drawRectElement(const ARectangle *rectElem);
    };

}
