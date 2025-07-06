// centipedeUpdate.cpp
#include "Centipede.hpp"
#include "Entity.hpp"
#include <chrono>
#include <cmath>
#include <algorithm>

namespace Arcade {

    int Centipede::updateGame(Event event) {
        handlePlayerMovement(event);
        handleShooting(event);
        updateProjectiles();
        updateCentipedes();
        cleanupOutOfBoundsCentipedes();
        checkGameOver();
        return _score;
    }
    
    void Centipede::handlePlayerMovement(Event event) {
        auto player = dynamic_cast<ARectangle *>(_elements[0].get());
        if (!player) return;
    
        auto pos = player->getPos();
        if (event.getType() == Event::TypeEvent::UP_ARROW && pos.second > 80) pos.second--;
        else if (event.getType() == Event::TypeEvent::DOWN_ARROW && pos.second < 98) pos.second++;
        else if (event.getType() == Event::TypeEvent::LEFT_ARROW && pos.first > 0) pos.first--;
        else if (event.getType() == Event::TypeEvent::RIGHT_ARROW && pos.first < 100) pos.first++;
    
        player->setPos(pos);
    }
    
    void Centipede::handleShooting(Event event) {
        static auto lastShot = std::chrono::steady_clock::now();
        auto now = std::chrono::steady_clock::now();
        if (event.getType() == Event::TypeEvent::ENTER_KEY &&
            std::chrono::duration_cast<std::chrono::milliseconds>(now - lastShot).count() > 200) {
            
            auto player = dynamic_cast<ARectangle *>(_elements[0].get());
            if (!player) return;
            auto pos = player->getPos();
            pos.second -= 1;
            
            auto projectile = std::make_unique<ARectangle>(std::make_pair(1, 3), pos, std::make_tuple(255, 0, 0), 1);
            _elements.push_back(std::move(projectile));
            lastShot = now;
        }
    }
    
    void Centipede::updateProjectiles() {
        for (size_t i = 0; i < _elements.size(); ++i) {
            auto projectile = dynamic_cast<ARectangle *>(_elements[i].get());
            if (!projectile || projectile->getID() != 1) continue;
        
            auto pos = projectile->getPos();
            pos.second--;
            if (pos.second < 0) {
                _elements.erase(_elements.begin() + i);
                --i;
                continue;
            }
        
            bool hit = false;
            for (size_t j = 0; j < _elements.size(); ++j) {
                auto target = dynamic_cast<ARectangle *>(_elements[j].get());
                if (!target || target->getID() == 1) continue;
            
                if (std::abs(target->getPos().first - pos.first) < 2 &&
                    std::abs(target->getPos().second - pos.second) < 2) {
                    if (target->getID() == 2) {
                        auto size = target->getSize();
                        if (size.first > 1 && size.second > 1)
                            target->setSize({size.first / 1.2, size.second / 1.2});
                        else {
                            _elements.erase(_elements.begin() + j);
                            --j;
                        }
                    } else if (target->getID() == 3 || target->getID() == 4) {
                        _score++;
                        _nbCentipede--;
                        for (size_t k = j + 1; k < _elements.size(); k++) {
                            auto next = dynamic_cast<ARectangle *>(_elements[k].get());
                            if (next && (next->getID() == 3 || next->getID() == 4))
                                next->setID(next->getID() == 3 ? 4 : 3);
                            else break;
                        }
                        auto newMushroom = std::make_unique<ARectangle>(
                            target->getSize(), target->getPos(), std::make_tuple(100, 100, 100), 2);
                        _elements.push_back(std::move(newMushroom));
                        _elements.erase(_elements.begin() + j);
                        --j;
                    }
                    _elements.erase(_elements.begin() + i);
                    --i;
                    hit = true;
                    break;
                }
            }
            if (!hit) projectile->setPos(pos);
        }
    
        for (auto &element : _elements) {
            auto scoreText = dynamic_cast<AText *>(element.get());
            if (scoreText && scoreText->getID() == 3) {
                scoreText->setText("Score: " + std::to_string(_score));
                return;
            }
        }
    
        auto scoreText = std::make_unique<AText>(std::make_pair(5, 5), std::make_pair(8, 1), "Score: " + std::to_string(_score), false, 3);
        scoreText->setColor(std::make_tuple(255, 255, 255));
        _elements.push_back(std::move(scoreText));
    }
    
    void Centipede::updateCentipedes() {
        for (auto &entity : _elements) {
            auto centi = dynamic_cast<ARectangle *>(entity.get());
            if (!centi || (centi->getID() != 3 && centi->getID() != 4)) continue;
        
            auto pos = centi->getPos();
            pos.first += (centi->getID() == 3) ? 1 : -1;
        
            for (const auto &other : _elements) {
                auto mushroom = dynamic_cast<ARectangle *>(other.get());
                if (!mushroom || mushroom->getID() != 2) continue;
            
                if (std::abs(mushroom->getPos().first - pos.first) < 2 &&
                    std::abs(mushroom->getPos().second - pos.second) < 2) {
                    pos.first -= (centi->getID() == 3) ? 1 : -1;
                    pos.second += 3;
                    centi->setID(centi->getID() == 3 ? 4 : 3);
                    break;
                }
            }
        
            if (pos.first < 0 || pos.first >= 100) {
                pos.first = std::clamp(pos.first, 0.0, 99.0);
                pos.second += 3;
                centi->setID(centi->getID() == 3 ? 4 : 3);
            }
            centi->setPos(pos);
        }
    }
    
    void Centipede::cleanupOutOfBoundsCentipedes() {
        bool allGone = true;
        for (size_t i = 0; i < _elements.size(); ++i) {
            auto c = dynamic_cast<ARectangle *>(_elements[i].get());
            if (c && (c->getID() == 3 || c->getID() == 4)) {
                if (c->getPos().second >= 100) {
                    _elements.erase(_elements.begin() + i);
                    --i;
                    _score--;
                    if (_score < 0)
                        _score = 0;
                    continue;
                }
                allGone = false;
            }
        }
    
        if (allGone && _centipedeCount < _maxCentipedes) {
            _elements.erase(std::remove_if(_elements.begin(), _elements.end(), [](const auto &e) {
                auto c = dynamic_cast<ARectangle *>(e.get());
                return c && (c->getID() == 3 || c->getID() == 4);
            }), _elements.end());
        
            _nbCentipede = 0;
            spawnCentipede();
        }
    }
    
    void Centipede::checkGameOver() {
        bool allCentipedesGone = true;
        for (const auto &entity : _elements) {
            auto centi = dynamic_cast<ARectangle *>(entity.get());
            if (centi && (centi->getID() == 3 || centi->getID() == 4)) {
                allCentipedesGone = false;
                break;
            }
        }
    
        if (allCentipedesGone && _centipedeCount >= _maxCentipedes) {
            _elements.clear();
            _nbCentipede = 0;
            _centipedeCount = 0;
            init({}, {}, _windowSize.first, _windowSize.second, {});
        }
    
        auto player = dynamic_cast<ARectangle *>(_elements[0].get());
        if (!player) return;
    
        for (const auto &entity : _elements) {
            auto centi = dynamic_cast<ARectangle *>(entity.get());
            if (centi && (centi->getID() == 3 || centi->getID() == 4)) {
                if (std::abs(centi->getPos().first - player->getPos().first) < 2 &&
                    std::abs(centi->getPos().second - player->getPos().second) < 2) {
                    displayEndScreen();
                    _gameState = GameState::GAME_OVER;
                }
            }
        }
    }

}