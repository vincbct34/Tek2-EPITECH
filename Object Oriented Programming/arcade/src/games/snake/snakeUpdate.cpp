/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snakeUpdate
*/

#include "Snake.hpp"

namespace Arcade {
    
    int Snake::updateGame(Event event) {
        updateDirection(event);

        std::vector<std::pair<double, double>> oldPositions;
        oldPositions = getOldPositions(oldPositions);

        moveSnake();
        updateSegmentsPositions(oldPositions);
        checkAppleCollision();
        checkSelfCollision();
        return _score;
    }
    
    void Snake::updateDirection(const Event &event) {
        switch (event.getType()) {
            case Event::TypeEvent::UP_ARROW:
            if (_direction != 1) _direction = 0;
            break;
            case Event::TypeEvent::DOWN_ARROW:
            if (_direction != 0) _direction = 1;
            break;
            case Event::TypeEvent::LEFT_ARROW:
            if (_direction != 3) _direction = 2;
            break;
            case Event::TypeEvent::RIGHT_ARROW:
            if (_direction != 2) _direction = 3;
            break;
            default:
            break;
        }
    }

    std::vector<std::pair<double, double>> Snake::getOldPositions(std::vector<std::pair<double, double>> oldPositions) {
        for (const auto &element : _elements) {
            auto snake = dynamic_cast<ARectangle *>(element.get());
            if (snake && snake->getID() != 2) oldPositions.push_back(snake->getPos());
        }

        return oldPositions;
    }
    
    void Snake::moveSnake() {
        auto head = dynamic_cast<ARectangle *>(_elements[0].get());
        if (!head) return;
    
        auto pos = head->getPos();
    
        if (_direction == 0) pos.second -= 1;
        else if (_direction == 1) pos.second += 1;
        else if (_direction == 2) pos.first -= 1;
        else if (_direction == 3) pos.first += 1;
    
        if (pos.first < 0) pos.first = 100;
        else if (pos.first >= 100) pos.first = 0;
        if (pos.second < 0) pos.second = 100;
        else if (pos.second >= 100) pos.second = 0;
    
        head->setPos(pos);
    }
    
    void Snake::updateSegmentsPositions(std::vector<std::pair<double, double>> oldPositions) {
        int id = 1;
        for (size_t i = 1; i < _elements.size(); ++i) {
            auto segment = dynamic_cast<ARectangle *>(_elements[i].get());
            if (segment && segment->getID() == 1) {
                segment->setPos(oldPositions[id - 1]);
                id++;
            }
        }
    }
    
    void Snake::checkAppleCollision() {
        auto head = dynamic_cast<ARectangle *>(_elements[0].get());
        auto apple = dynamic_cast<ARectangle *>(_elements[4].get());
        if (!head || !apple) return;

        if (std::abs(head->getPos().first - apple->getPos().first) < 2 &&
            std::abs(head->getPos().second - apple->getPos().second) < 2) {

            _score++;
            apple->setPos({ static_cast<double>(rand() % 100), static_cast<double>(rand() % 100) });

            auto newSegment = std::make_unique<ARectangle>(std::make_pair(2, 2), std::make_pair(0, 0), std::make_tuple(100, 100, 100), 1);
            _elements.push_back(std::move(newSegment));

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
    }

    void Snake::checkSelfCollision() {
        auto head = dynamic_cast<ARectangle *>(_elements[0].get());
        if (!head || _direction == -1) return;

        int bodyCount = 0;
        for (const auto &element : _elements) {
            auto segment = dynamic_cast<ARectangle *>(element.get());
            if (segment && segment->getID() == 1) {
                bodyCount++;
            }
        }

        if (bodyCount <= 3) return;

        for (size_t i = 1; i < _elements.size(); ++i) {
            auto segment = dynamic_cast<ARectangle *>(_elements[i].get());
            if (segment && segment->getID() == 1 && head->getPos() == segment->getPos()) {
                displayEndScreen();
                _gameState = GameState::GAME_OVER;
            }
        }
    }

}