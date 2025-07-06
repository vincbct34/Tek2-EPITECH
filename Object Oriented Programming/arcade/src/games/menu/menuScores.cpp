/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** menuScores
*/

#include "Menu.hpp"

namespace Arcade {

    void Menu::displayScores() {
        _elements.erase(std::remove_if(_elements.begin(), _elements.end(), [](const std::unique_ptr<IEntity>& e) {
            auto text = dynamic_cast<AText*>(e.get());
            return text && text->getText().find(":" ) != std::string::npos;
        }), _elements.end());
    
        auto scoresIt = _scores.find("lib/" + _chosenGame + ".so");
        if (scoresIt != _scores.end()) {
            const auto &bestScores = scoresIt->second;
            auto title = std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{50, 70}, "Best Scores:", false, 0);
            title->setColor({255, 255, 0});
            _elements.push_back(std::move(title));
        
            int padding = 1;
            for (const auto &score : bestScores) {
                auto text = std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{50, 70 + padding * 5}, score.first + " : " + std::to_string(score.second), false, 0);
                _elements.push_back(std::move(text));
                padding++;
            }
        }
    }

}
