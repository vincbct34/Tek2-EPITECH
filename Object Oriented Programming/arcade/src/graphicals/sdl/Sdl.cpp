/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** sdl
*/

#include "Sdl.hpp"
#include <SDL2/SDL_keyboard.h>
#include <SDL2/SDL_timer.h>
#include <SDL2/SDL_video.h>
#include <string>

namespace Arcade {

    void SDL::init() {
        if (SDL_Init(SDL_INIT_VIDEO) != 0) {
            std::cerr << "Erreur SDL_Init: " << SDL_GetError() << std::endl;
            return;
        }
        if (TTF_Init() != 0) {
            std::cerr << "Erreur TTF_Init: " << TTF_GetError() << std::endl;
            SDL_Quit();
            return;
        }

        _window = SDL_CreateWindow("Arcade SDL", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 800, 600, SDL_WINDOW_SHOWN);
        if (!_window) {
            std::cerr << "Erreur SDL_CreateWindow: " << SDL_GetError() << std::endl;
            SDL_Quit();
            return;
        }

        _renderer = SDL_CreateRenderer(_window, -1, SDL_RENDERER_ACCELERATED);
        if (!_renderer) {
            std::cerr << "Erreur SDL_CreateRenderer: " << SDL_GetError() << std::endl;
            SDL_DestroyWindow(_window);
            TTF_Quit();
            SDL_Quit();
            return;
        }

        _font = TTF_OpenFont("src/graphicals/fonts/LEMONMILK-Medium.otf", 48);
        if (!_font) {
            std::cerr << "Erreur TTF_OpenFont: " << TTF_GetError() << std::endl;
            SDL_DestroyRenderer(_renderer);
            SDL_DestroyWindow(_window);
            TTF_Quit();
            SDL_Quit();
            return;
        }

        _lastUpdate = SDL_GetTicks();
        
    }

    void SDL::clearScreen() {
        SDL_SetRenderDrawColor(_renderer, 0, 0, 0, 255);
        SDL_RenderClear(_renderer);
    }

    void SDL::refresh() {
        SDL_RenderPresent(_renderer);
        SDL_Delay(10);
    }

    bool SDL::canUpdate()
    {
        Uint32 now = SDL_GetTicks();
        if (now - _lastUpdate >= _updateInterval) {
            _lastUpdate = now;
            return true;
        }
        return false;
    }


    void SDL::stop()
    {
        if (_textTexture) {
            SDL_DestroyTexture(_textTexture);
            _textTexture = nullptr;
        }
        if (_font) {
            TTF_CloseFont(_font);
            _font = nullptr;
        }
        if (_renderer) {
            SDL_DestroyRenderer(_renderer);
            _renderer = nullptr;
        }
        if (_window) {
            SDL_DestroyWindow(_window);
            _window = nullptr;
        }
        SDL_PumpEvents(); // Vide les évents restants
        TTF_Quit();
        SDL_Quit();
    }


    std::pair<int, int> SDL::getWindowSize() const {
        return {800, 600};
    }

    std::string SDL::getPlayerName() {
        if (!_font || !_renderer) {
            std::cerr << "Error: SDL resources not initialized properly." << std::endl;
            return "";
        }

        std::string name;
        SDL_Event event;

        SDL_StartTextInput();

        SDL_Color white = {255, 255, 255, 255};
        SDL_Color green = {0, 255, 0, 255};

        SDL_Surface* promptSurface = TTF_RenderText_Solid(_font, "Enter your name:", white);
        if (!promptSurface) {
            std::cerr << "Error: Failed to create prompt surface: " << TTF_GetError() << std::endl;
            SDL_StopTextInput();
            return "";
        }

        SDL_Texture* promptTexture = SDL_CreateTextureFromSurface(_renderer, promptSurface);
        SDL_FreeSurface(promptSurface);
        if (!promptTexture) {
            std::cerr << "Error: Failed to create prompt texture: " << SDL_GetError() << std::endl;
            SDL_StopTextInput();
            return "";
        }

        SDL_Rect promptRect = {200, 200, 0, 0};
        SDL_QueryTexture(promptTexture, NULL, NULL, &promptRect.w, &promptRect.h);

        SDL_Texture* inputTexture = nullptr;
        SDL_Rect inputRect = {200, 260, 0, 0};

        while (true) {
            while (SDL_PollEvent(&event)) {
                if (event.type == SDL_QUIT) {
                    SDL_StopTextInput();
                    if (inputTexture) SDL_DestroyTexture(inputTexture);
                    SDL_DestroyTexture(promptTexture);
                    return "";
                } else if (event.type == SDL_TEXTINPUT) {
                    name += event.text.text;
                } else if (event.type == SDL_KEYDOWN) {
                    if (event.key.keysym.sym == SDLK_BACKSPACE && !name.empty()) {
                        name.pop_back();
                    } else if (event.key.keysym.sym == SDLK_RETURN) {
                        SDL_StopTextInput();
                        if (inputTexture) SDL_DestroyTexture(inputTexture);
                        SDL_DestroyTexture(promptTexture);
                        return name;
                    }
                }
            }

            if (name.empty()) {
                if (inputTexture) SDL_DestroyTexture(inputTexture);
                inputTexture = nullptr;
                inputRect.w = 0;
                inputRect.h = 0;
            } else {
                if (inputTexture) SDL_DestroyTexture(inputTexture);
                SDL_Surface* inputSurface = TTF_RenderText_Solid(_font, name.c_str(), green);
                if (!inputSurface) {
                    std::cerr << "Error: Failed to create input surface: " << TTF_GetError() << std::endl;
                    continue;
                }

                inputTexture = SDL_CreateTextureFromSurface(_renderer, inputSurface);
                SDL_FreeSurface(inputSurface);
                if (!inputTexture) {
                    std::cerr << "Error: Failed to create input texture: " << SDL_GetError() << std::endl;
                    continue;
                }

                SDL_QueryTexture(inputTexture, NULL, NULL, &inputRect.w, &inputRect.h);
            }

            SDL_RenderClear(_renderer);
            SDL_RenderCopy(_renderer, promptTexture, NULL, &promptRect);
            SDL_RenderCopy(_renderer, inputTexture, NULL, &inputRect);
            SDL_RenderPresent(_renderer);
        }
    }

}
